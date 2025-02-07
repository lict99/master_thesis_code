# %%
# Importing packages
from itertools import product
from pathlib import Path

import polars as pl
from polars import col, lit, when

from functions import surv_expr

# %%
# Setting up the directory for the output
output_dir: Path = Path("results/01")
if not output_dir.exists():
    output_dir.mkdir()

# %%
# Survival data of CRC patients for West China
hx_surv_df: pl.DataFrame = (
    pl.scan_csv(
        Path("data/hx/raw_data_with_comments_241121.csv"),
        null_values=["NA", " ", ""],
        schema_overrides={"register": pl.String},
    )
    # We only include patients with CRC diagnosis
    .filter(col("inclusion") == 1)
    .with_columns(
        # Date transformation
        # So far the date columns are represented as string
        # The format is dirty, so we need to clean it up into format "YYYY-MM-DD"
        col(
            "surgery_date",
            "diagnosis_date_for_nonsurgery",
            "last_fu_date",
            "death_date",
            "local_recurrence_date",
            "metastasis_date",
            "last_radio_date",
        )
        .str.replace_all("/", "-", literal=True)
        .str.split("-")
        .fill_null(lit([], pl.List(pl.String)))
        .list.gather([0, 1, 2], null_on_oob=True)
        .list.eval(
            pl.concat_str(
                pl.element().get(0),
                pl.element().get(1).str.pad_start(2, "0"),
                # Some date have only year and month
                # So we impute the empty day with 15
                when(
                    pl.Expr.and_(
                        pl.element().get(0).is_not_null(),
                        pl.element().get(1).is_not_null(),
                        pl.element().get(2).is_null(),
                    )
                )
                .then(lit("15"))
                .otherwise(pl.element().get(2).str.pad_start(2, "0")),
                separator="-",
                ignore_nulls=False,
            )
        )
        .list.first()
        .str.to_date("%Y-%m-%d"),
        # Sex transformation
        # 1 is male and 2 is female
        col("sex").replace_strict([1, 2], ["male", "female"], default=lit(None)),
        # Stage transformation
        # The stage is primarily pathological stage, otherwise clinical stage
        when(col("p_stage").is_not_null())
        .then(col("p_stage"))
        .otherwise(col("c_stage"))
        .alias("stage"),
        # OS indicator
        # Patients who died by any cause are considered as overall survival events
        col("death").alias("os"),
        # CSS indicator
        # Patients who died by CRC are considered as cancer-specific survival events
        col("death_by_crc").alias("css"),
        # DFS indicator
        # Patients who died by CRC, had local recurrence, or metastasis
        # are considered as disease-free survival events
        # We only consider patients who had surgery
        when(col("surgery_date").is_not_null())
        .then(
            when(
                pl.Expr.or_(
                    col("death_by_crc") == 1,
                    col("local_recurrence") == 1,
                    col("metastasis") == 1,
                )
            )
            .then(lit(1))
            .when(
                pl.Expr.and_(
                    col("death_by_crc") == 0,
                    col("local_recurrence") == 0,
                    col("metastasis") == 0,
                )
            )
            .then(lit(0))
            .otherwise(lit(None))
        )
        .otherwise(lit(None))
        .alias("dfs"),
    )
    .with_columns(
        # Start date of follow-up
        # This date is only for OS and CSS
        # For DFS is surgery date because of different definitions
        # The start date is primarily surgery date
        # Otherwise diagnosis date for non-surgery
        when(col("surgery_date").is_not_null())
        .then(col("surgery_date"))
        .otherwise(col("diagnosis_date_for_nonsurgery"))
        .alias("start_fu_date_os_css"),
        # End date of follow-up for OS
        # For patients with OS events, the end date is death date
        # Otherwise the latest date of last follow-up or radiograph test
        # for patients without OS events
        when(col("os") == 1)
        .then(col("death_date"))
        .when(col("os") == 0)
        .then(pl.max_horizontal("last_fu_date", "last_radio_date"))
        .otherwise(lit(None))
        .alias("end_fu_date_os"),
        # End date of follow-up for CSS
        # For patients with CSS events, the end date is death date
        # Otherwise the latest date of death, last follow-up or radiograph test
        # for patients without CSS events
        when(col("css") == 1)
        .then(col("death_date"))
        .when(col("css") == 0)
        .then(pl.max_horizontal("last_fu_date", "last_radio_date", "death_date"))
        .otherwise(lit(None))
        .alias("end_fu_date_css"),
        # End date of follow-up for DFS
        # For patients with DFS events
        # the end date is the earliest date of death, local recurrence, or metastasis
        # Otherwise the latest date of radiograph test for patients without DFS events
        # We do not consider the last follow-up date for patients without DFS events
        # because we cannot detect DFS events according to the phone contact follow-up
        when(col("dfs") == 1)
        .then(
            pl.min_horizontal(
                "death_date",
                "local_recurrence_date",
                "metastasis_date",
            )
        )
        .when(col("dfs") == 0)
        .then(col("last_radio_date"))
        .otherwise(lit(None))
        .alias("end_fu_date_dfs"),
    )
    .with_columns(
        # OS time with unit day
        col("end_fu_date_os")
        .sub(col("start_fu_date_os_css"))
        .dt.total_days()
        .alias("os_time"),
        # CSS time with unit day
        col("end_fu_date_css")
        .sub(col("start_fu_date_os_css"))
        .dt.total_days()
        .alias("css_time"),
        # DFS time with unit day
        col("end_fu_date_dfs")
        .sub(col("surgery_date"))
        .dt.total_days()
        .alias("dfs_time"),
    )
    .with_columns(
        # Survival indicators for 1, 3 and 5 years
        [surv_expr(surv, yr) for surv, yr in product(["os", "css", "dfs"], [1, 3, 5])]
    )
    .collect()
)

# %%
# Logical check
for surv in ["os", "css", "dfs"]:
    if hx_surv_df.get_column(f"{surv}_time").lt(0).any():
        raise ValueError(f"Negative time to follow-up for {surv.upper()}")

# %%
# Saving the cleansed data
hx_surv_df.write_csv(output_dir.joinpath("hx_survival_data.csv"))

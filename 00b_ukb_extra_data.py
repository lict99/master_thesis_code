# %%
# Importing packages
from pathlib import Path

import polars as pl
from polars import col

from functions import value_maps

# %%
# Setting up the directory for the output
output_dir: Path = Path("results/00")
if not output_dir.exists():
    output_dir.mkdir()

# %%
# Survival data of CRC patients in UK Biobank
surv_data: pl.DataFrame = pl.read_csv(
    Path("results/00/ukb_survival_data.csv"),
    null_values=["", " ", "NA"],
)

# %%
# Extra data from UK Biobank
extra_data: pl.DataFrame = (
    pl.scan_csv(
        Path("data/ukb/ukb_initial_visit.csv"),
        null_values=["", " ", "NA"],
    )
    .filter(
        # We only include CRC patients from the survival data
        # and those with complete platelet count
        pl.Expr.and_(
            col("eid").is_in(surv_data.get_column("eid")),
            col("platelet_count").is_not_null(),
        )
    )
    .with_columns(
        col("platelet_count_acquisition_time").str.slice(0, 10).str.to_date("%Y-%m-%d"),
        col("date_of_attending_assessment_centre").str.to_date("%Y-%m-%d"),
        # Platelet count > 300
        col("platelet_count").cut([300], labels=["no", "yes"]).alias("plt_300"),
        # Platelet count > 400
        col("platelet_count").cut([400], labels=["no", "yes"]).alias("plt_400"),
    )
    .with_columns(
        col("platelet_count_acquisition_time")
        .sub(col("date_of_attending_assessment_centre"))
        .dt.total_days()
        .alias("test_lag_days")
    )
    # We only include patients who are tested platelet count
    # within 7 days of attending the assessment centre
    .filter((col("test_lag_days") >= 0) & (col("test_lag_days") < 7))
    .with_columns([col(nm).replace_strict(value_maps[nm]) for nm in value_maps.keys()])
    .collect()
)

# %%
# Merging the survival data and the extra data
ukb_all_df: pl.DataFrame = (
    surv_data.join(extra_data, on="eid", how="inner", validate="1:1")
    .with_columns(
        # Diagnostic lag time
        # Days from the date of attending to the date of CRC diagnosis.
        col("date_crc_diagnosis")
        .str.to_date("%Y-%m-%d")
        .sub(col("date_of_attending_assessment_centre"))
        .dt.total_days()
        .alias("diagnostic_lag_time")
    )
    # We only include patients
    # who are diagnosed with CRC after attending the assessment centre
    .filter(col("diagnostic_lag_time") >= 0)
    .with_columns(
        # Age at CRC diagnosis
        col("age_when_attended_assessment_centre")
        .add(col("diagnostic_lag_time") / 365.25)
        .alias("age_at_diagnosis")
    )
)

# %%
# Selecting the columns of interest
data_columns: list[str] = [
    "eid",
    "age_at_diagnosis",
    "sex",
    "body_mass_index",
    "ethnic_background",
    "smoking_status",
    "alcohol_drinker_status",
    "diagnostic_lag_time",
    "platelet_count",
    "plt_300",
    "plt_400",
    "os",
    "os_time",
    *[f"os_{n}yr" for n in [1, 3, 5]],
    "css",
    "css_time",
    *[f"css_{n}yr" for n in [1, 3, 5]],
]

ukb_df: pl.DataFrame = ukb_all_df.select(col(data_columns))

# %%
# Saving the data
ukb_all_df.write_csv(output_dir.joinpath("ukb_all_data.csv"))
ukb_df.write_csv(output_dir.joinpath("ukb_data.csv"))

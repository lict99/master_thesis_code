# %%
# Importing packages
from pathlib import Path

import polars as pl
from polars import col, lit

# %%
# Setting up the directory for the output
output_dir: Path = Path("results/01")
if not output_dir.exists():
    output_dir.mkdir()

# %%
# Survival data of CRC patients in West China
surv_df: pl.DataFrame = pl.read_csv(
    Path("results/01/hx_survival_data.csv"),
    schema_overrides={"register": pl.String},
)

# %%
# Mapping Chinese variable names to their corresponding English labels
name_dict: dict[str, str] = {
    "登记号": "register",
    "身高/cm": "height_cm",
    "体重/kg": "weight_kg",
    "抽烟": "smoking",
    "喝酒": "alcohol",
    "新辅助治疗": "neo_adjuvant_therapy",
    "血小板": "platelet_count",
}

# %%
# Extra data from West China
extra_df: pl.DataFrame = (
    pl.read_excel(
        Path("data/hx/a名单总表20241121.xlsx"),
        sheet_id=1,
        engine="calamine",  # fastexcel
        read_options={"skip_rows": 0, "header_row": 1, "dtypes": "string"},
        columns=list(name_dict.keys()),
        drop_empty_rows=True,
    )
    .select(
        col(name_dict.keys())
        .str.strip_chars()
        .replace({"na": None})
        .name.map(lambda x: name_dict[x])
    )
    .filter(
        # We only keep patients from survival data
        # and patients with complete platelet count
        pl.Expr.and_(
            col("register").is_in(surv_df.get_column("register")),
            col("platelet_count").is_not_null(),
        )
    )
    .with_columns(col("weight_kg", "height_cm", "platelet_count").cast(pl.Float64))
    .with_columns(
        # Body mass index calculation
        col("weight_kg")
        .truediv(col("height_cm").truediv(100).pow(2))
        .alias("body_mass_index"),
        # Recoding binary variables
        col("smoking", "alcohol").replace_strict(
            {"否": "never", "无": "never", "否·": "never", "是": "ever"},
            default=lit(None),
        ),
        col("neo_adjuvant_therapy").replace_strict(
            {"否": "no", "是": "yes"},
            default=lit(None),
        ),
        # Platelet count > 300
        col("platelet_count").cut([300], labels=["no", "yes"]).alias("plt_300"),
        # Platelet count > 400
        col("platelet_count").cut([400], labels=["no", "yes"]).alias("plt_400"),
    )
)

# %%
# Merging the survival data and the extra data
hx_full_df: pl.DataFrame = surv_df.join(
    extra_df,
    on="register",
    how="inner",
    validate="1:1",
)

# %%
# Selecting the columns of interest
data_columns: list[str] = [
    "id",
    "register",
    "age",
    "sex",
    "body_mass_index",
    "smoking",
    "alcohol",
    "stage",
    "neo_adjuvant_therapy",
    "platelet_count",
    "plt_300",
    "plt_400",
    "os",
    "os_time",
    *[f"os_{n}yr" for n in [1, 3, 5]],
    "css",
    "css_time",
    *[f"css_{n}yr" for n in [1, 3, 5]],
    "dfs",
    "dfs_time",
    *[f"dfs_{n}yr" for n in [1, 3, 5]],
]

hx_df: pl.DataFrame = hx_full_df.select(col(data_columns))

# %%
# Saving the data
hx_full_df.write_csv(output_dir.joinpath("hx_full_data.csv"))
hx_df.write_csv(output_dir.joinpath("hx_data.csv"))

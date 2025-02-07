# %%
# Importing packages
from datetime import date
from itertools import product
from pathlib import Path

import polars as pl
from polars import col, lit, when

from functions import surv_expr

# %%
# Setting up the directory for the output
output_dir: Path = Path("results/00")
if not output_dir.exists():
    output_dir.mkdir()

# %%
# Listing all data files
csv_files: list[Path] = list(Path("data/ukb").glob("*.csv"))

# %%
# Scanning all data into a dictionary
lazy_data: dict[str, pl.LazyFrame] = {
    fs.name: pl.scan_csv(fs, null_values=["", " ", "NA"]) for fs in csv_files
}

# %%
# Regular expression for the colorectal cancer (CRC)
CRC_ICD9: str = "^15[34]"
CRC_ICD10: str = "^C1[89]|^C2[01]"

# %%
# Cancer registry of CRC diagnosis
crc_cancer_registry: pl.DataFrame = (
    lazy_data["date_cancer_registry_update.csv"]
    .select(pl.all().name.to_lowercase())
    .filter(
        # We filter out NULL data and only include CRC diagnosis
        pl.Expr.and_(
            col("date_of_cancer_diagnosis").is_not_null(),
            pl.Expr.or_(
                col("type_of_cancer_icd9").str.contains(CRC_ICD9),
                col("type_of_cancer_icd10").str.contains(CRC_ICD10),
            ),
        )
    )
    .select(
        # eid
        col("eid"),
        # CRC diagnosis
        when(col("type_of_cancer_icd10").is_not_null())
        .then(col("type_of_cancer_icd10"))
        .when(col("type_of_cancer_icd9").is_not_null())
        .then(col("type_of_cancer_icd9"))
        .otherwise(lit(None))
        .alias("crc_diagnosis"),
        # Date of CRC diagnosis
        col("date_of_cancer_diagnosis").alias("date_crc_diagnosis"),
    )
    .collect()
)

# %%
# Diagnosis records of ICD9 for CRC
crc_diagnosis_icd9: pl.DataFrame = (
    lazy_data["diagnosis_ICD9.csv"]
    .select(pl.all().name.to_lowercase())
    .filter(
        # We filter out NULL data and only include CRC diagnosis
        pl.Expr.and_(
            col("diagnoses_icd9").str.contains(CRC_ICD9),
            col("date_icd9").is_not_null(),
        )
    )
    .select(
        # eid
        col("eid"),
        # CRC diagnosis
        col("diagnoses_icd9").alias("crc_diagnosis"),
        # Date of CRC diagnosis
        col("date_icd9").alias("date_crc_diagnosis"),
    )
    .collect()
)

# %%
# Diagnosis records of ICD10 for CRC
crc_diagnosis_icd10: pl.DataFrame = (
    lazy_data["diagnosis_ICD10_update.csv"]
    .select(pl.all().name.to_lowercase())
    .filter(
        # We filter out NULL data and only include CRC diagnosis
        pl.Expr.and_(
            col("diagnosis_icd10").str.contains(CRC_ICD10),
            col("date_icd10").is_not_null(),
        )
    )
    .select(
        # eid
        col("eid"),
        # CRC diagnosis
        col("diagnosis_icd10").alias("crc_diagnosis"),
        # Date of CRC diagnosis
        col("date_icd10").alias("date_crc_diagnosis"),
    )
    .collect()
)

# %%
# Merging all CRC diagnosis data
# We only keep the first CRC diagnosis record of each individual
crc_diagnosis: pl.DataFrame = (
    pl.concat(
        [crc_cancer_registry, crc_diagnosis_icd9, crc_diagnosis_icd10],
        how="vertical",
    )
    .with_columns(col("date_crc_diagnosis").str.to_date("%Y-%m-%d"))
    .sort(col("eid"), col("date_crc_diagnosis"), descending=False)
    .filter(col("eid").is_first_distinct())
)

# %%
# Death records of CRC individuals
crc_death: pl.DataFrame = (
    lazy_data["date_death_update.csv"]
    .select(pl.all().name.to_lowercase())
    .filter(
        # We filter out NULL data and only include individuals with CRC diagnosis
        pl.Expr.and_(
            col("eid").is_in(crc_diagnosis.get_column("eid")),
            col("date_death").is_not_null(),
            col("icd10").is_not_null(),
        )
    )
    .group_by(col("eid"))
    .agg(
        # Death due to CRC
        when(col("icd10").str.contains(CRC_ICD10).any())
        .then(lit(1))
        .otherwise(lit(0))
        .alias("crc_death"),
        # Date of death
        # We only keep individuals with a single date of death
        # Otherwise, we set the date of death to NULL
        when(col("date_death").n_unique() == 1)
        .then(col("date_death").first().str.to_date("%Y-%m-%d"))
        .otherwise(lit(None)),
        # ICD10 codes for the cause of death
        col("icd10").str.join("+").alias("icd10_death"),
    )
    .collect()
)

# %%
# Survival data for CRC patients
ukb_surv_df: pl.DataFrame = (
    crc_diagnosis.join(crc_death, on="eid", how="left", validate="1:1")
    .with_columns(
        # The date of last follow-up is the date of death if the patient died
        # Otherwise 2023-01-01
        when(col("date_death").is_not_null())
        .then(col("date_death"))
        .otherwise(lit(date(2023, 1, 1), dtype=pl.Date))
        .alias("date_last_fu")
    )
    .with_columns(
        # OS indicator
        when(col("date_death").is_not_null())
        .then(lit(1))
        .otherwise(lit(0))
        .alias("os"),
        # Time to follow-up of OS
        # Days from the date of CRC diagnosis to the date of last follow-up
        col("date_last_fu")
        .sub(col("date_crc_diagnosis"))
        .dt.total_days()
        .alias("os_time"),
        # CSS indicator
        when(col("crc_death") == 1).then(lit(1)).otherwise(lit(0)).alias("css"),
        # Time to follow-up of CSS
        # Days from the date of CRC diagnosis to the date of last follow-up
        col("date_last_fu")
        .sub(col("date_crc_diagnosis"))
        .dt.total_days()
        .alias("css_time"),
    )
    .with_columns(
        # Survival indicators for 1, 3 and 5 years
        [surv_expr(surv, yr) for surv, yr in product(["os", "css"], [1, 3, 5])]
    )
)

# %%
# Logical check for the survival data
if ukb_surv_df.get_column("os_time").lt(0).any():
    raise ValueError("Negative time to follow-up for OS")
if ukb_surv_df.get_column("css_time").lt(0).any():
    raise ValueError("Negative time to follow-up for CSS")

# %%
# Saving the survival data
ukb_surv_df.write_csv(output_dir.joinpath("ukb_survival_data.csv"))

# %%
# Attaching packages and functions
library("readr")
library("dplyr")

# %%
# Setting up output directory
output_dir <- "results/08"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Reading West China cohort data
hx_data <- read_csv("results/01/hx_data.csv")

# %%
# Reading West China SNP data
snp_samples <- read_delim(
  "data/snp/asa_array/raw_data/22B0218D_23B0203E_23P1212B00A_24B0516F.fam",
  delim = " ",
  col_names = FALSE
)

# %%
# Getting shared samples between SNP and cohort data
shared_samples <- intersect(snp_samples[[1]], hx_data[["id"]])

# %%
# Extracting sex information for shared samples
sex_info <- hx_data |>
  slice(match(shared_samples, hx_data[["id"]], 0)) |>
  mutate(
    sex = case_match(sex, "male" ~ 1, "female" ~ 2, .default = 0),
    id2 = id
  ) |>
  select(id, id2, sex)

# %%
# Saving sex information for shared samples
write_delim(
  sex_info,
  file = file.path(output_dir, "sex_info.txt"),
  delim = " ",
  col_names = FALSE,
  na = "0"
)

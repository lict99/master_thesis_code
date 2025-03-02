# %%
library("readr")
library("dplyr")

# %%
output_dir <- "results/08"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
hx_data <- read_csv("results/01/hx_data.csv")

# %%
snp_samples <- read_delim(
  "data/snp/asa_array/raw_data/22B0218D_23B0203E_23P1212B00A_24B0516F.fam",
  delim = " ",
  col_names = FALSE
)

# %%
shared_samples <- intersect(snp_samples[[1]], hx_data[["id"]])

# %%
sex_info <- hx_data |>
  slice(match(shared_samples, hx_data[["id"]], 0)) |>
  mutate(
    sex = case_match(sex, "male" ~ 1, "female" ~ 2, .default = 0),
    id2 = id
  ) |>
  select(id, id2, sex)

# %%
write_delim(
  sex_info,
  file = file.path(output_dir, "sex_info.txt"),
  delim = " ",
  col_names = FALSE,
  na = "0"
)

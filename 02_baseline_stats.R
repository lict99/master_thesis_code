# %%
# Attaching packages and functions
library("table1")
library("readr")
library("dplyr")
library("openxlsx2")

source("functions/table1_func.R", local = TRUE)

# %%
# Setting up the output directory
output_dir <- "results/02"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Reading UK Biobank data
ukb_data <- read_csv("results/00/ukb_data.csv")

# Arranging UK Biobank data
ukb_tb1_df <- ukb_data |>
  mutate(
    plt_300 = case_match(plt_300, "no" ~ FALSE, "yes" ~ TRUE),
    plt_400 = case_match(plt_400, "no" ~ FALSE, "yes" ~ TRUE),
    os = case_match(os, 0 ~ FALSE, 1 ~ TRUE),
    css = case_match(css, 0 ~ FALSE, 1 ~ TRUE),
    dfs = NA,
    fu_time = os_time,
    age = age_at_diagnosis,
    smoking = smoking_status,
    alcohol = alcohol_drinker_status,
    neo_adjuvant_therapy = NA,
    group = "UK Biobank"
  ) |>
  select(
    platelet_count, plt_300, plt_400,
    os, css, dfs, fu_time,
    age, sex, body_mass_index, smoking, alcohol, neo_adjuvant_therapy,
    diagnostic_lag_time,
    group
  )

# %%
# Reading West China data
hx_data <- read_csv("results/01/hx_data.csv")

# Arranging West China data
hx_tb1_df <- hx_data |>
  mutate(
    plt_300 = case_match(plt_300, "no" ~ FALSE, "yes" ~ TRUE),
    plt_400 = case_match(plt_400, "no" ~ FALSE, "yes" ~ TRUE),
    os = case_match(os, 0 ~ FALSE, 1 ~ TRUE),
    css = case_match(css, 0 ~ FALSE, 1 ~ TRUE),
    dfs = case_match(dfs, 0 ~ FALSE, 1 ~ TRUE),
    fu_time = os_time,
    neo_adjuvant_therapy = case_match(
      neo_adjuvant_therapy,
      "no" ~ FALSE,
      "yes" ~ TRUE
    ),
    diagnostic_lag_time = NA,
    group = "West China"
  ) |>
  select(
    platelet_count, plt_300, plt_400,
    os, css, dfs, fu_time,
    age, sex, body_mass_index, smoking, alcohol, neo_adjuvant_therapy,
    diagnostic_lag_time,
    group
  )

# %%
# Combining UK Biobank and West China data
tb1_df <- as.data.frame(rbind(ukb_tb1_df, hx_tb1_df))

# Table 1
tb1 <- table1(
  ~ . | group,
  tb1_df,
  overall = FALSE,
  render.continuous = render_numeral,
  extra.col = list(p_value = cal_p_value)
) |>
  as.data.frame()

# %%
# Writing table 1 to Excel
write_xlsx(tb1, file.path(output_dir, "table1.xlsx"))

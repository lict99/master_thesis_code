# %%
library("readr")
library("dplyr")
library("ggplot2")
library("showtext")

source("functions/font_config.R", local = TRUE)
source("functions/rcs_coxph.R", local = TRUE)

showtext_auto()

# %%
output_dir <- "results/05"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
ukb_data <- read_csv("results/00/ukb_data.csv") |>
  mutate(
    sex = factor(sex, levels = c("female", "male")),
    smoking_status = factor(smoking_status, levels = c("never", "ever")),
    alcohol_drinker_status = factor(
      alcohol_drinker_status,
      levels = c("never", "ever")
    )
  ) |>
  as.data.frame()

# %%
for (event in c("os", "css")) {
  for (covar_set in c("set1", "set2")) {
    char_event <- switch(event,
      os = "总体生存期",
      css = "癌症特异性生存期",
      stop("Unknown event", call. = FALSE)
    )
    covars <- switch(covar_set,
      set1 = c("age_at_diagnosis", "sex"),
      set2 = c(
        "age_at_diagnosis", "sex", "body_mass_index",
        "smoking_status", "alcohol_drinker_status"
      ),
      stop("Unknown covariate set", call. = FALSE)
    )
    ukb_p <- plot_coxph_rcs(
      data = ukb_data,
      time = paste0(event, "_time"),
      event = event,
      target = "platelet_count",
      covariates = covars,
      font_family = font_zh,
      xlab = "血小板计数 (10<sup>9</sup>/L)",
      ylab = sprintf("风险比 (%s)", char_event)
    )
    ggsave(
      file.path(output_dir, sprintf("ukb_%s_%s_rcs.pdf", event, covar_set)),
      plot = ukb_p,
      width = 5,
      height = 5
    )
  }
}

# %%
hx_data <- read_csv("results/01/hx_data.csv") |>
  mutate(
    sex = factor(sex, levels = c("female", "male")),
    neo_adjuvant_therapy = factor(
      neo_adjuvant_therapy,
      levels = c("no", "yes")
    ),
    smoking = factor(smoking, levels = c("never", "ever")),
    alcohol = factor(alcohol, levels = c("never", "ever"))
  ) |>
  as.data.frame()

# %%
for (event in c("os", "css", "dfs")) {
  for (covar_set in c("set1", "set2")) {
    char_event <- switch(event,
      os = "总体生存期",
      css = "癌症特异性生存期",
      dfs = "无疾病生存期",
      stop("Unknown event", call. = FALSE)
    )
    covars <- switch(covar_set,
      set1 = c("age", "sex"),
      set2 = c(
        "age", "sex", "body_mass_index",
        "smoking", "alcohol", "neo_adjuvant_therapy"
      ),
      stop("Unknown covariate set", call. = FALSE)
    )
    hx_p <- plot_coxph_rcs(
      data = hx_data,
      time = paste0(event, "_time"),
      event = event,
      target = "platelet_count",
      covariates = covars,
      font_family = font_zh,
      xlab = "血小板计数 (10<sup>9</sup>/L)",
      ylab = sprintf("风险比 (%s)", char_event)
    )
    ggsave(
      file.path(output_dir, sprintf("hx_%s_%s_rcs.pdf", event, covar_set)),
      plot = hx_p,
      width = 5,
      height = 5
    )
  }
}

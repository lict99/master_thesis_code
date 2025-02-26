# %%
library("readr")
library("dplyr")
library("ggplot2")
library("showtext")

source("functions/font_config.R", local = TRUE)
source("functions/coxph_pairwise.R", local = TRUE)

showtext_auto()

# %%
output_dir <- "results/06"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
ukb_data <- read_csv("results/00/ukb_data.csv") |>
  mutate(
    plt_100u = platelet_count / 100,
    sex = factor(sex, levels = c("female", "male")),
    smoking_status = factor(smoking_status, levels = c("never", "ever")),
    alcohol_drinker_status = factor(
      alcohol_drinker_status,
      levels = c("never", "ever")
    )
  )

# %%
for (event in c("os", "css")) {
  for (covar_set in c("set1", "set2")) {
    covars <- switch(covar_set,
      set1 = c("age_at_diagnosis", "sex"),
      set2 = c(
        "age_at_diagnosis", "sex", "body_mass_index",
        "smoking_status", "alcohol_drinker_status"
      ),
      stop("Unknown set of covariates", call. = FALSE)
    )

    ukb_rolling_results <- calc_rolling_coxph(
      data = ukb_data,
      time = paste0(event, "_time"),
      event = event,
      target = "plt_100u",
      covariates = covars,
      rolling_by = "diagnostic_lag_time",
      start = 0,
      end = 5000,
      window = 365.25,
      step = 7
    )

    p <- ggplot(data = ukb_rolling_results, aes(x = end)) +
      geom_hline(yintercept = 1, color = "gray") +
      geom_line(aes(y = hr_u95, color = "ci"), linetype = 3) +
      geom_line(aes(y = hr_l95, color = "ci"), linetype = 3) +
      geom_line(aes(y = hr, color = "hr")) +
      labs(x = "延迟诊断时间 (天)", y = "风险比", color = NULL) +
      scale_color_manual(
        breaks = c("hr", "ci"),
        values = c("#BC3C29FF", "#0072B5FF"),
        labels = c("风险比", "95% 置信区间")
      ) +
      theme_classic(base_family = font_zh) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.5, 1),
        legend.direction = "horizontal",
        legend.justification.inside = c(0.5, 1),
        axis.text = element_text(color = "black")
      )

    ggsave(
      file.path(output_dir, sprintf("%s_%s_rolling.pdf", event, covar_set)),
      plot = p,
      width = 5,
      height = 2,
      units = "in"
    )
  }
}

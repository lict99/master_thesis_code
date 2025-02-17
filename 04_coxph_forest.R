# %%
library("readr")
library("tidyr")
library("dplyr")
library("survival")
library("grid")
library("forestploter")
library("showtext")

source("functions/font_config.R", local = TRUE)
source("functions/coxph_pairwise.R", local = TRUE)

showtext_auto()

# %%
output_dir <- "results/04"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
ukb_data <- read_csv("results/00/ukb_data.csv") |>
  mutate(
    plt_100u = platelet_count / 100,
    plt_300 = factor(plt_300, levels = c("no", "yes")),
    plt_400 = factor(plt_400, levels = c("no", "yes")),
    ethnic_background = factor(
      ethnic_background,
      levels = c("white", "asian", "black", "mixed", "other")
    ),
    sex = factor(sex, levels = c("female", "male")),
    smoking_status = factor(smoking_status, levels = c("never", "ever")),
    alcohol_drinker_status = factor(
      alcohol_drinker_status,
      levels = c("never", "ever")
    )
  )

# %%
ukb_coxph_df <- calc_coxph_pairwise(
  event_time_list = list(
    c(event = "os", time = "os_time"),
    c(event = "css", time = "css_time")
  ),
  targets = c("plt_100u", "plt_300", "plt_400"),
  covariates_list = list(
    c("age_at_diagnosis", "sex"),
    c(
      "age_at_diagnosis", "sex", "body_mass_index",
      "smoking_status", "alcohol_drinker_status", "ethnic_background"
    )
  ),
  data = ukb_data
)

# %%
ukb_forest_data <- ukb_coxph_df |>
  mutate(
    hr_fmt = sprintf("%.2f (%.2f - %.2f)", hr, hr_l95, hr_u95),
    p_fmt = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
  ) |>
  pivot_wider(
    names_from = target,
    values_from = c(coef, se, p_value, hr, hr_l95, hr_u95, hr_fmt, p_fmt)
  ) |>
  mutate(
    hr_fmt = paste(
      hr_fmt_plt_100u,
      hr_fmt_plt_300yes,
      hr_fmt_plt_400yes,
      sep = "\n"
    ),
    p_fmt = paste(
      p_fmt_plt_100u,
      p_fmt_plt_300yes,
      p_fmt_plt_400yes,
      sep = "\n"
    ),
    ci_col = paste(rep(" ", 20), collapse = " "),
    model = covariates |>
      nchar() |>
      factor(labels = c("模型1", "模型2")) |>
      as.character()
  ) |>
  add_row(
    event_type = "os", .before = 1
  ) |>
  add_row(
    event_type = "css", .after = 3
  ) |>
  mutate(
    event_type = case_when(
      duplicated(event_type) ~ paste0("    ", model),
      .default = event_type
    )
  ) |>
  mutate(
    event_type = case_match(
      event_type,
      "os" ~ "总体生存期",
      "css" ~ "癌症特异性生存期",
      .default = event_type
    )
  )

ukb_forest_layout <- ukb_forest_data |>
  select(event_type, hr_fmt, ci_col, p_fmt) |>
  mutate(across(everything(), function(x) if_else(is.na(x), "", x))) |>
  rename_with(function(x) {
    vapply(
      x,
      function(y) {
        switch(y,
          event_type = "生存结局",
          hr_fmt = "HR (95% CI)",
          ci_col = " ",
          p_fmt = "P值",
          stop("Unknown column name", call. = FALSE)
        )
      },
      character(1L)
    )
  })

# %%
theme <- forest_theme(
  core = list(bg_params = list(fill = c("white", "gray95"))),
  base_family = font_zh,
  ci_pch = 19,
  ci_lwd = 2,
  ci_col = c("#BC3C29FF", "#0072B5FF", "#20854EFF"),
  legend_name = "血小板计数",
  legend_value = c(
    expression("每" ~ 100 ~ "×" ~ 10^9 * "/L"),
    expression(">" ~ 300 ~ "×" ~ 10^9 * "/L"),
    expression(">" ~ 400 ~ "×" ~ 10^9 * "/L")
  ),
  legend_position = "bottom",
  legend_gp = gpar(cex = 0.9),
  xaxis_gp = gpar(lwd = 1, cex = 0.8)
)

ukb_p <- forest(
  ukb_forest_layout,
  est = list(
    ukb_forest_data$hr_plt_100u,
    ukb_forest_data$hr_plt_300yes,
    ukb_forest_data$hr_plt_400yes
  ),
  lower = list(
    ukb_forest_data$hr_l95_plt_100u,
    ukb_forest_data$hr_l95_plt_300yes,
    ukb_forest_data$hr_l95_plt_400yes
  ),
  upper = list(
    ukb_forest_data$hr_u95_plt_100u,
    ukb_forest_data$hr_u95_plt_300yes,
    ukb_forest_data$hr_u95_plt_400yes
  ),
  ci_column = 3,
  ref_line = 1,
  nudge_y = 1 / 3,
  ticks_at = c(0.75, 1, 1.25, 1.5),
  theme = theme
) |>
  edit_plot(
    row = c(1, 4),
    part = "body",
    which = "text",
    gp = gpar(fontface = "bold")
  ) |>
  add_border(part = "header", where = "bottom")

pdf(
  file.path(output_dir, "ukb_coxph_forest.pdf"),
  width = get_wh(ukb_p)[1],
  height = get_wh(ukb_p)[2]
)
showtext_begin()
plot(ukb_p)
showtext_end()
dev.off()
unlink("Rplots.pdf")

# %%
hx_data <- read_csv("results/01/hx_data.csv") |>
  mutate(
    plt_100u = platelet_count / 100,
    plt_300 = factor(plt_300, levels = c("no", "yes")),
    plt_400 = factor(plt_400, levels = c("no", "yes")),
    sex = factor(sex, levels = c("female", "male")),
    neo_adjuvant_therapy = factor(
      neo_adjuvant_therapy,
      levels = c("no", "yes")
    ),
    smoking = factor(smoking, levels = c("never", "ever")),
    alcohol = factor(alcohol, levels = c("never", "ever"))
  )

# %%
hx_coxph_df <- calc_coxph_pairwise(
  event_time_list = list(
    c(event = "os", time = "os_time"),
    c(event = "css", time = "css_time"),
    c(event = "dfs", time = "dfs_time")
  ),
  targets = c("plt_100u", "plt_300", "plt_400"),
  covariates_list = list(
    c("age", "sex"),
    c(
      "age", "sex", "body_mass_index",
      "smoking", "alcohol", "neo_adjuvant_therapy"
    )
  ),
  data = hx_data
)
# %%
hx_forest_data <- hx_coxph_df |>
  mutate(
    hr_fmt = sprintf("%.2f (%.2f - %.2f)", hr, hr_l95, hr_u95),
    p_fmt = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
  ) |>
  pivot_wider(
    names_from = target,
    values_from = c(coef, se, p_value, hr, hr_l95, hr_u95, hr_fmt, p_fmt)
  ) |>
  mutate(
    hr_fmt = paste(
      hr_fmt_plt_100u,
      hr_fmt_plt_300yes,
      hr_fmt_plt_400yes,
      sep = "\n"
    ),
    p_fmt = paste(
      p_fmt_plt_100u,
      p_fmt_plt_300yes,
      p_fmt_plt_400yes,
      sep = "\n"
    ),
    ci_col = paste(rep(" ", 20), collapse = " "),
    model = covariates |>
      nchar() |>
      factor(labels = c("模型1", "模型2")) |>
      as.character()
  ) |>
  add_row(
    event_type = "os", .before = 1
  ) |>
  add_row(
    event_type = "css", .after = 3
  ) |>
  add_row(
    event_type = "dfs", .after = 6
  ) |>
  mutate(
    event_type = case_when(
      duplicated(event_type) ~ paste0("    ", model),
      .default = event_type
    )
  ) |>
  mutate(
    event_type = case_match(
      event_type,
      "os" ~ "总体生存期",
      "css" ~ "癌症特异性生存期",
      "dfs" ~ "无疾病生存期",
      .default = event_type
    )
  )

hx_forest_layout <- hx_forest_data |>
  select(event_type, hr_fmt, ci_col, p_fmt) |>
  mutate(across(everything(), function(x) if_else(is.na(x), "", x))) |>
  rename_with(function(x) {
    vapply(
      x,
      function(y) {
        switch(y,
          event_type = "生存结局",
          hr_fmt = "HR (95% CI)",
          ci_col = " ",
          p_fmt = "P值",
          stop("Unknown column name", call. = FALSE)
        )
      },
      character(1L)
    )
  })

# %%
hx_p <- forest(
  hx_forest_layout,
  est = list(
    hx_forest_data$hr_plt_100u,
    hx_forest_data$hr_plt_300yes,
    hx_forest_data$hr_plt_400yes
  ),
  lower = list(
    hx_forest_data$hr_l95_plt_100u,
    hx_forest_data$hr_l95_plt_300yes,
    hx_forest_data$hr_l95_plt_400yes
  ),
  upper = list(
    hx_forest_data$hr_u95_plt_100u,
    hx_forest_data$hr_u95_plt_300yes,
    hx_forest_data$hr_u95_plt_400yes
  ),
  ci_column = 3,
  ref_line = 1,
  nudge_y = 1 / 3,
  xlim = c(0.4, 3.6),
  ticks_at = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5),
  theme = theme
) |>
  edit_plot(
    row = c(1, 4, 7),
    part = "body",
    which = "text",
    gp = gpar(fontface = "bold")
  ) |>
  add_border(part = "header", where = "bottom")

pdf(
  file.path(output_dir, "hx_coxph_forest.pdf"),
  width = get_wh(hx_p)[1],
  height = get_wh(hx_p)[2]
)
showtext_begin()
plot(hx_p)
showtext_end()
dev.off()

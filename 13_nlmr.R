# %%
# Attaching packages and functions
library("readr")
library("dplyr")
library("nlmr")
library("ggplot2")
library("ggtext")
library("showtext")

source("functions/font_config.R", local = TRUE)

showtext_auto()

# %%
# Setting up output directory
output_dir <- "results/13"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Reading PRS data
prs_data <- read_csv("results/12/prs.csv")

# %%
# Reading West China data
hx_data <- read_csv("results/01/hx_data.csv") |>
  mutate(
    sex_male = case_match(
      sex,
      "male" ~ 1,
      "female" ~ 0,
      .default = NA_real_
    )
  ) |>
  select(id, platelet_count, age, sex_male, os_3yr, css_3yr, dfs_3yr)

# %%
# Merging PRS data with West China data
nlmr_data <- left_join(prs_data, hx_data, by = join_by(iid == id)) |>
  as.data.frame()

# %%
# Performing nonlinear Mendelian randomization
for (event in c("os", "css", "dfs")) {
  event_char <- switch(
    event,
    os = "总体生存期",
    css = "癌症特异性生存期",
    dfs = "无疾病生存期"
  )

  for (prs in c("wprs", "uprs")) {
    set.seed(1)
    # Constructing the data frame for nonlinear Mendelian randomization
    # We use the 3-year event data as the outcome
    data <- data.frame(
      y = nlmr_data[[paste0(event, "_3yr")]],
      x = nlmr_data$platelet_count,
      g = nlmr_data[[prs]],
      age = nlmr_data$age,
      sex_male = nlmr_data$sex_male
    ) |>
      na.omit()

    # Fitting fractional polynomial model
    fit <- fracpoly_mr(
      y = data$y,
      x = data$x,
      g = data$g,
      covar = data[c("age", "sex_male")],
      family = "binomial",
      q = 5,
      xpos = 0.5,
      d = 1,
      fig = TRUE,
      ref = median(hx_data$platelet_count)
    )

    plot_data <- fit$figure$data

    # P value of nonlinear test
    pval <- if_else(
      fit$p_tests[, "fp"] < 0.001,
      "P 值 < 0.001",
      sprintf("P 值 = %.3f", fit$p_tests[, "fp"])
    )

    p <- ggplot(data = plot_data, aes(x = x)) +
      geom_hline(yintercept = 1, color = "gray") +
      geom_line(aes(y = uci, color = "ci")) +
      geom_line(aes(y = lci, color = "ci")) +
      geom_line(aes(y = yest, color = "or"), linewidth = 1) +
      annotate(
        "point",
        x = median(hx_data$platelet_count),
        y = 1,
        color = "#BC3C29FF",
        size = 3
      ) +
      annotate(
        "label",
        x = min(plot_data$x),
        y = min(plot_data$lci),
        label = pval,
        label.size = 0,
        family = font_zh,
        hjust = -0.1,
        vjust = 0.5
      ) +
      labs(
        color = NULL,
        x = "血小板计数 (10<sup>9</sup>/L)",
        y = sprintf("比值比 (三年%s)", event_char)
      ) +
      scale_y_log10() +
      scale_color_manual(
        breaks = c("or", "ci"),
        values = c("#0072B5FF", "black"),
        labels = c("比值比", "95% 置信区间")
      ) +
      theme_classic(base_family = font_zh) +
      theme(
        axis.text = element_text(color = "black"),
        axis.title = element_markdown(),
        legend.position = "inside",
        legend.position.inside = c(0.5, 1),
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA)
      )

    ggsave(
      file.path(output_dir, sprintf("nlmr_%s_%s.pdf", event, prs)),
      plot = p,
      width = 4,
      height = 3
    )
  }
}

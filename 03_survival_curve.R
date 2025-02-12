# %%
# Attaching packages and functions
library("readr")
library("dplyr")
library("survival")
library("survminer")
library("showtext")
library("ggplot2")
library("ggtext")
library("patchwork")

source("functions/surv_plot.R", local = TRUE)
source("functions/font_config.R", local = TRUE)

showtext_auto()

# %%
# Setting output directory
output_dir <- "results/03"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Loading UK Biobank data
ukb_data <- read_csv("results/00/ukb_data.csv") |>
  mutate(
    plt_300 = factor(plt_300, levels = c("yes", "no")),
    plt_400 = factor(plt_400, levels = c("yes", "no"))
  )

# %%
# Plotting survival curves for UK Biobank data
for (event in c("os", "css")) {
  for (plt in c("plt_300", "plt_400")) {
    fml <- as.formula(sprintf("Surv(%s_time, %s) ~ %s", event, event, plt))
    p <- plot_ggsurv(
      surv_fit(fml, data = ukb_data),
      legend.title = plt,
      legend.labs = levels(ukb_data[[plt]]),
      title = event,
      family = font_zh
    )
    ggsave(
      file.path(output_dir, paste0("ukb_", plt, "_", event, ".pdf")),
      plot = p,
      width = 7,
      height = 7
    )
  }
}

# %%
# Loading West China data
hx_data <- read_csv("results/01/hx_data.csv") |>
  mutate(
    plt_300 = factor(plt_300, levels = c("yes", "no")),
    plt_400 = factor(plt_400, levels = c("yes", "no"))
  )

# %%
# Plotting survival curves for West China data
for (event in c("os", "css", "dfs")) {
  for (plt in c("plt_300", "plt_400")) {
    fml <- as.formula(sprintf("Surv(%s_time, %s) ~ %s", event, event, plt))
    p <- plot_ggsurv(
      surv_fit(fml, data = hx_data),
      legend.title = plt,
      legend.labs = levels(hx_data[[plt]]),
      title = event,
      family = font_zh
    )
    ggsave(
      file.path(output_dir, paste0("hx_", plt, "_", event, ".pdf")),
      plot = p,
      width = 7,
      height = 7
    )
  }
}

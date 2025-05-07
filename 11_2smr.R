# %%
# Attaching packages and functions
library("readr")
library("dplyr")
library("TwoSampleMR")
library("ggplot2")
library("showtext")
library("openxlsx2")
library("future.apply")

source("functions/font_config.R", local = TRUE)

showtext_auto()

# %%
# Setting up output directory
output_dir <- "results/11"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Loading exposure data
load("results/07/exposure_data.rda")

# %%
# Loading outcome data
gwas_coxph <- lapply(
  list(os = "os", css = "css", dfs = "dfs"),
  function(x) {
    data <- read_csv(sprintf("results/10/gwas_%s_coxph.csv", x))

    if (any(data$p_value < 5e-8)) {
      stop("p-value < 5e-8 for", x)
    }

    formatted_data <- data |>
      mutate(phenotype = toupper(x), id = x) |>
      as.data.frame() |>
      format_data(
        type = "outcome",
        phenotype_col = "phenotype",
        snp_col = "snp_id",
        beta_col = "coef",
        se_col = "se",
        eaf_col = "eaf",
        effect_allele_col = "effect_allele",
        other_allele_col = "other_allele",
        pval_col = "p_value",
        chr_col = "chromosome",
        pos_col = "position",
        id_col = "id",
        samplesize_col = "n"
      )

    return(formatted_data)
  }
)

# Saving outcome data
save(gwas_coxph, file = file.path(output_dir, "gwas_coxph.rda"))

# %%
# Harmonizing data
mr_iv_info <- lapply(
  gwas_coxph,
  function(x) harmonise_data(exposure_data, x, action = 2)
)

# Saving harmonized data
save(
  mr_iv_info,
  file = file.path(output_dir, "mr_iv_info.rda")
)

# %%
# Running MR analysis
plan(multisession)

mr_results <- future_lapply(
  mr_iv_info,
  function(x) {
    set.seed(1)

    # Getting valid IVs
    valid_iv <- x[x$mr_keep, , drop = FALSE] |>
      mutate(fmt_eaf_exp = sprintf("%.2f", eaf.exposure)) |>
      arrange(as.numeric(chr.exposure), as.numeric(pos.exposure))

    # Calculating MR results
    results <- mr(
      x,
      method_list = c("mr_ivw", "mr_egger_regression", "mr_weighted_median")
    ) |>
      generate_odds_ratios() |>
      mutate(
        or_fmt = sprintf("%.2f (%.2f - %.2f)", or, or_lci95, or_uci95),
        pval_fmt = if_else(pval < 0.001, "< 0.001", sprintf("%.3f", pval))
      )

    # Performing heterogeneity analysis
    heterogeneity <- mr_heterogeneity(x) |>
      mutate(
        q_fmt = sprintf("%.2f", Q),
        q_pval_fmt = if_else(Q_pval < 0.001, "< 0.001", sprintf("%.3f", Q_pval))
      )

    # Performing pleiotropy analysis
    pleiotropy <- mr_pleiotropy_test(x) |>
      mutate(
        egger_intercept_fmt = sprintf("%.2f", egger_intercept),
        se_fmt = sprintf("%.2f", se),
        pval_fmt = if_else(pval < 0.001, "< 0.001", sprintf("%.3f", pval))
      )

    # Performing MR-PRESSO analysis
    presso <- run_mr_presso(x)[[1]][["MR-PRESSO results"]] |>
      with({
        `%|%` <- function(x, y) if (inherits(x, "try-error")) y else x

        no_outliers <- try(
          as.character(length(`Distortion Test`$`Outliers Indices`)),
          silent = TRUE
        )

        distortion_coefficient <- try(
          sprintf("%.2f", `Distortion Test`$`Distortion Coefficient`),
          silent = TRUE
        )

        distortion_pvalue <- try(
          if_else(
            `Distortion Test`$Pvalue < 0.001,
            "< 0.001",
            sprintf("%.3f", `Distortion Test`$Pvalue)
          ),
          silent = TRUE
        )

        data.frame(
          global_rss = sprintf("%.2f", `Global Test`$RSSobs),
          global_pvalue = if_else(
            `Global Test`$Pvalue < 0.001,
            "< 0.001",
            sprintf("%.3f", `Global Test`$Pvalue)
          ),
          n_outliers = no_outliers %|% "0",
          distortion_coefficient = distortion_coefficient %|% "-",
          distortion_pvalue = distortion_pvalue %|% "-"
        )
      })

    return(
      list(
        valid_iv = valid_iv,
        results = results,
        heterogeneity = heterogeneity,
        pleiotropy = pleiotropy,
        presso = presso
      )
    )
  },
  future.seed = TRUE
)

# %%
# Saving MR analysis results
for (idx in c("valid_iv", "results", "heterogeneity", "pleiotropy", "presso")) {
  write_xlsx(
    lapply(mr_results, function(x) x[[idx]]),
    file.path(output_dir, sprintf("mr_%s.xlsx", idx))
  )
}

# %%
# Plotting MR scatter and leave-one-out plots
for (i in c("os", "css", "dfs")) {
  i_char <- switch(
    i,
    os = "总体生存期",
    css = "癌症特异性生存期",
    dfs = "无疾病生存期",
    stop("Unknown event", call. = FALSE)
  )

  # MR scatter plot
  p_scatter <- mr_scatter_plot(mr_results[[i]]$results, mr_iv_info[[i]])[[1]] +
    labs(
      x = "遗传变异对血小板计数的效应",
      y = sprintf("遗传变异对%s的效应", i_char),
      color = NULL
    ) +
    scale_color_manual(
      values = c("#BC3C29FF", "#0072B5FF", "#20854EFF"),
      labels = c("逆方差加权", "MR-Egger", "加权中位数"),
      breaks = c("Inverse variance weighted", "MR Egger", "Weighted median")
    ) +
    guides(color = guide_legend(ncol = 3)) +
    theme_classic(base_family = font_zh) +
    theme(
      axis.text = element_text(color = "black"),
      legend.position = "inside",
      legend.position.inside = c(0.5, 1),
      legend.justification = c(0.5, 1),
      legend.direction = "horizontal"
    )

  ggsave(
    file.path(output_dir, sprintf("mr_scatter_%s.pdf", i)),
    plot = p_scatter,
    width = 5,
    height = 4
  )

  # MR leave-one-out plot
  p_loo <- mr_leaveoneout_plot(mr_leaveoneout(mr_iv_info[[i]]))[[1]] +
    scale_color_manual(values = c("black", "#BC3C29FF")) +
    labs(
      x = sprintf("血小板计数对%s\n留一验证法分析", i_char),
      y = NULL
    ) +
    theme_classic(base_family = font_zh) +
    theme(
      axis.text = element_text(color = "black"),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )

  ggsave(
    file.path(output_dir, sprintf("mr_loo_%s.pdf", i)),
    plot = p_loo,
    width = 3,
    height = 6
  )
}

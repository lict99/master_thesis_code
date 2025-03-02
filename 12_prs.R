# %%
library("readr")
library("dplyr")
library("ggplot2")
library("ggtext")
library("patchwork")
library("showtext")

source("functions/font_config.R", local = TRUE)

showtext_auto()

# %%
output_dir <- "results/12"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
load("results/11/mr_iv_info.rda")

# %%
mr_prs <- lapply(
  mr_iv_info,
  function(x) {
    data <- x[x$mr_keep, , drop = FALSE]

    iv_file <- tempfile(fileext = ".txt")
    prs_dir <- tempdir()

    iv_df <- data.frame(
      snp = data$SNP,
      allele = ifelse(
        data$beta.exposure > 0,
        data$effect_allele.exposure,
        data$other_allele.exposure
      ),
      w_beta = abs(data$beta.exposure),
      u_beta = 1
    )

    write_delim(
      iv_df,
      iv_file,
      delim = " ",
      col_names = FALSE
    )

    system(
      sprintf(
        "plink --bfile %s --score %s 1 2 3 sum --out %s",
        "results/09/data_final",
        iv_file,
        file.path(prs_dir, "wprs")
      )
    )

    system(
      sprintf(
        "plink --bfile %s --score %s 1 2 4 sum --out %s",
        "results/09/data_final",
        iv_file,
        file.path(prs_dir, "uprs")
      )
    )

    wprs <- read_table(file.path(prs_dir, "wprs.profile"))
    uprs <- read_table(file.path(prs_dir, "uprs.profile"))

    if (!identical(wprs$IID, uprs$IID)) {
      stop("Not identical IID in PRS", call. = FALSE)
    }

    return(
      data.frame(
        iid = wprs$IID,
        wprs = wprs$SCORESUM,
        uprs = uprs$SCORESUM
      )
    )
  }
)

# %%
if (identical(mr_prs$os, mr_prs$css) && identical(mr_prs$os, mr_prs$dfs)) {
  prs_data <- mr_prs$os
} else {
  stop("Not identical PRS between OS, CSS, and DFS", call. = FALSE)
}

write_csv(prs_data, file.path(output_dir, "prs.csv"))

# %%
hx_data <- read_csv("results/01/hx_data.csv") |>
  select(id, platelet_count) |>
  as.data.frame()

# %%
plot_data <- left_join(prs_data, hx_data, by = join_by(iid == id)) |>
  as.data.frame()

# %%
for (prs in c("wprs", "uprs")) {
  prs_char <- switch(prs,
    wprs = "加权多基因风险评分",
    uprs = "非加权多基因风险评分"
  )

  color_line <- "#BC3C29FF"
  color_density <- "#0072B5FF"

  fit <- summary(
    lm(
      formula = as.formula(sprintf("platelet_count ~ %s", prs)),
      data = plot_data
    )
  )

  fval <- sprintf("F = %.2f", fit$fstatistic[1])

  pval <- pf(
    fit$fstatistic[1],
    fit$fstatistic[2],
    fit$fstatistic[3],
    lower.tail = FALSE
  ) |>
    (function(x) {
      if_else(
        x < 0.001,
        "P 值 < 0.001",
        sprintf("P 值 = %.3f", x)
      )
    })()

  p_xy <- ggplot(data = plot_data, aes(x = .data[[prs]], y = platelet_count)) +
    geom_point(color = "gray50", alpha = 0.5, size = 1) +
    stat_ellipse(linetype = 2, linewidth = 1) +
    geom_smooth(
      method = "lm",
      formula = "y ~ x",
      color = color_line,
      linewidth = 1.5
    ) +
    annotate(
      geom = "label",
      x = min(plot_data[[prs]]),
      y = max(plot_data[["platelet_count"]]),
      label = sprintf("%s\n%s", fval, pval),
      label.size = 0,
      family = font_zh,
      hjust = 0,
      vjust = 1
    ) +
    labs(
      x = prs_char,
      y = "血小板计数 (10<sup>9</sup>/L)"
    ) +
    theme_classic(base_family = font_zh) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_markdown()
    )

  p_x <- ggplot(data = plot_data) +
    geom_density(
      aes(x = .data[[prs]]),
      color = color_density,
      fill = alpha(color_density, 0.8)
    ) +
    labs(y = NULL) +
    theme_classic(base_family = font_zh) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  p_y <- ggplot(data = plot_data) +
    geom_density(
      aes(x = platelet_count),
      color = color_density,
      fill = alpha(color_density, 0.8)
    ) +
    labs(y = NULL) +
    scale_y_continuous(n.breaks = 2) +
    coord_flip() +
    theme_classic(base_family = font_zh) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  p <- p_x + plot_spacer() + p_xy + p_y +
    plot_layout(ncol = 2, widths = c(8, 2), heights = c(2, 8))

  ggsave(
    file.path(output_dir, sprintf("%s.pdf", prs)),
    plot = p,
    width = 5,
    height = 5
  )
}

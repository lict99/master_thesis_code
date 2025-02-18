#' Wrapper for `ggsurvplot` to Customize Survival Plots
#'
#' This function is a wrapper around [survminer::ggsurvplot()] to create and
#' customize survival plots with specific settings for legend titles, labels,
#' and themes.
#'
#' @param fit A survival fit object, see [survminer::ggsurvplot()].
#' @param legend.title A character string specifying the legend title. Must be
#' one of "plt_300" or "plt_400".
#' @param legend.labs A character vector specifying the legend labels. Must
#' contain "yes" or "no".
#' @param title A character string specifying the plot title. Must be one of
#' "os", "css", or "dfs".
#' @param family A character string specifying the font family for the plot.
#' @param ... Additional arguments passed to [survminer::ggsurvplot()].
#'
#' @return A `patchwork` object combining the survival plot and the risk table.
#' @note This is a high coupled funtion to control workflow.
plot_ggsurv <- function(
    fit,
    legend.title,
    legend.labs,
    title,
    family,
    ...) {
  params <- list(
    fit = fit,
    palette = c("#BC3C29FF", "#0072B5FF"),
    censor = FALSE,
    pval = FALSE,
    pval.method = FALSE,
    risk.table = TRUE,
    legend.title = switch(legend.title,
      plt_300 = "血小板计数 &gt; 300&times;10<sup>9</sup>/L",
      plt_400 = "血小板计数 &gt; 400&times;10<sup>9</sup>/L",
      stop("Unknown legend title", call. = FALSE)
    ),
    legend.labs = vapply(
      legend.labs,
      function(x) {
        switch(x,
          yes = "是",
          no = "否",
          stop("Unknown legend label", call. = FALSE)
        )
      },
      character(1L)
    ),
    title = switch(title,
      os = "总体生存期",
      css = "癌症特异性生存期",
      dfs = "无疾病生存期",
      stop("Unknown title", call. = FALSE)
    ),
    xlab = "随访时间 (天)",
    ylab = "生存率",
    risk.table.title = "存在风险的人数"
  )
  if (any(names(list(...)) %in% names(params))) {
    stop("Duplicated parameters", call. = FALSE)
  }
  p <- do.call(survminer::ggsurvplot, c(params, list(...)))
  pvalue <- survminer::surv_pvalue(fit, method = "survdiff")[["pval"]]
  # We set the minimal value of y-axis to 0.4
  ymin <- 0.4
  p$plot <- p$plot +
    ggplot2::annotate(
      geom = "richtext",
      x = 0,
      y = ymin,
      label = paste0("log-rank 检验<br>P 值 = ", sprintf("%.3f", pvalue)),
      label.color = NA,
      size = 5,
      family = family,
      hjust = 0,
      vjust = 0
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = family),
      legend.title = ggtext::element_markdown(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(limits = c(ymin, 1))
  p$table <- p$table + ggplot2::theme(
    text = ggplot2::element_text(family = family),
    axis.title.y = ggtext::element_markdown(),
    plot.title = ggplot2::element_text(size = 15)
  )
  pp <- (p$plot / p$table) + patchwork::plot_layout(heights = c(0.75, 0.25))
  return(pp)
}

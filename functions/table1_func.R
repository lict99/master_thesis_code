#' Render Numerical Summary Statistics
#'
#' @param x A numeric vector.
#' @param ... Additional arguments.
#'
#' @return A character vector containing formatted summary statistics:
#' - `Mean (SD)`: Mean and standard deviation;
#' - `Median (IQR)`: Median and interquartile range (Q1, Q3).
#'
#' @details
#' The statistics are rounded with 0-padding and 1 digit.
render_numeral <- function(x, ...) {
  loadNamespace("table1")

  with(
    table1::stats.apply.rounding(
      table1::stats.default(x, ...),
      digits = 1,
      rounding.fn = table1::round_pad,
      ...
    ),
    c(
      "",
      `Mean (SD)` = sprintf("%s (%s)", MEAN, SD),
      `Median (IQR)` = sprintf("%s (%s, %s)", MEDIAN, Q1, Q3)
    )
  )
}

#' Calculate P-Value for Group Comparisons
#'
#' @param x A list of vectors where each vector represents a group.
#' @param ... Ignored.
#'
#' @return A formatted p-value as a character string.
#'
#' @details
#' - Wilcoxon rank-sum test with continuity correction for numeric data;
#' - Fisher's exact test for categorical data.
cal_p_value <- function(x, ...) {
  loadNamespace("stats")

  y <- unlist(x, use.names = FALSE)
  g <- factor(rep(seq_along(x), times = vapply(x, length, integer(1L))))

  if (is.numeric(y)) {
    p <- stats::wilcox.test(y ~ g, correct = TRUE)$p.value
  } else if (is.factor(y) || is.character(y) || is.logical(y)) {
    p <- stats::fisher.test(table(y, g))$p.value
  } else {
    stop("Not supported data type", call. = FALSE)
  }

  return(format.pval(p, digits = 3, eps = 0.001))
}

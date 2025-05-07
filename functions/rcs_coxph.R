#' Calculate Cox Proportional Hazard models with Restricted Cubic Splines
#'
#' This internal function fits two Cox proportional hazard models:
#' 1. A linear model with the target variable.
#' 2. A non-linear model using restricted cubic splines for the target variable.
#' It then calculates non-linear hazard ratios across the range of the target
#' variable.
#'
#' @param data A data frame containing the variables for analysis.
#' @param time The name of the time variable.
#' @param event The name of the event indicator variable.
#' @param target The name of the target variable to be modeled with restricted
#' cubic splines.
#' @param covariates A character vector of covariate names to include in the
#' model.
#'
#' @return A list containing:
#' 1. target_value: original values of the target variable.
#' 2. prediction: data frame with predicted hazard ratios and confidence
#' intervals.
#' 3. p_value: p-value from likelihood ratio test comparing linear vs.
#' non-linear models.
#' 4. ph_pval: global p-value from proportional hazard assumption test.
.calc_coxph_rcs <- function(
    data,
    time,
    event,
    target,
    covariates) {
  loadNamespace("stats")
  loadNamespace("survival")
  loadNamespace("Hmisc")
  loadNamespace("lmtest")
  loadNamespace("smoothHR")

  linear_fml <- stats::as.formula(
    paste(
      sprintf("Surv(%s, %s)", time, event),
      "~",
      paste(c(target, covariates), collapse = " + ")
    )
  )
  linear_model <- survival::coxph(formula = linear_fml, data = data)

  rcs_fml <- stats::as.formula(
    paste(
      sprintf("Surv(%s, %s)", time, event),
      "~",
      paste(
        c(
          sprintf("Hmisc::rcspline.eval(%s, nk = 3, inclx = TRUE)", target),
          covariates
        ),
        collapse = " + "
      )
    )
  )
  rcs_model <- eval(
    substitute(
      survival::coxph(formula = fml, data = data, x = TRUE),
      list(fml = rcs_fml)
    )
  )

  pval <- lmtest::lrtest(linear_model, rcs_model)[2, "Pr(>Chisq)"]

  hr_obj <- smoothHR::smoothHR(data = data, coxfit = rcs_model)

  points <- smoothHR::predict.HR(
    object = hr_obj,
    predictor = target,
    prob = 0.5,
    prediction.values = seq(
      min(data[[target]], na.rm = TRUE),
      max(data[[target]], na.rm = TRUE),
      length.out = 100
    ),
    conf.level = 0.95
  )

  points <- as.data.frame(points)
  points["hr"] <- exp(points["LnHR"])
  points["hr_l95"] <- exp(points["lower .95"])
  points["hr_u95"] <- exp(points["upper .95"])

  return(
    list(
      target_value = data[[target]],
      prediction = points,
      p_value = pval,
      ph_pval = hr_obj$phtest$table["GLOBAL", "p"]
    )
  )
}

#' Plot Cox Proportional Hazard models with Restricted Cubic Splines
#'
#' This function creates a visualization of hazard ratios estimated using
#' restricted cubic splines in a Cox proportional hazard model. The plot
#' includes the hazard ratio curve, 95% confidence intervals, and a density
#' distribution of the target variable.
#'
#' @param data A data frame containing the variables for analysis.
#' @param time The name of the time variable.
#' @param event The name of the event indicator variable.
#' @param target The name of the target variable to be modeled with restricted
#' cubic splines.
#' @param covariates A character vector of covariate names to include in the
#' model.
#' @param font_family Font family to be used in the plot.
#' @param xlab Label for the x-axis (defaults to target variable name).
#' @param ylab Label for the y-axis (defaults to "风险比").
#'
#' @return A ggplot2 object displaying the hazard ratio curve, confidence
#' intervals, and density distribution of the target variable.
plot_coxph_rcs <- function(
    data,
    time,
    event,
    target,
    covariates,
    font_family,
    xlab = target,
    ylab = "风险比") {
  loadNamespace("stats")
  loadNamespace("scales")
  loadNamespace("ggplot2")
  loadNamespace("ggtext")

  rcs <- .calc_coxph_rcs(
    time = time,
    event = event,
    target = target,
    covariates = covariates,
    data = data
  )

  target_density <- stats::density(rcs$target_value, na.rm = TRUE)
  density_limits <- c(0, max(target_density$y))
  hr_limits <- c(0, max(rcs$prediction$hr_u95))
  density_y2 <- scales::rescale(target_density$y, hr_limits, density_limits)
  color_density <- "#0072B5FF"
  color_hr <- "#BC3C29FF"
  color_ci <- "black"

  nl_pval_fmt <- ifelse(
    rcs$p_value < 0.001,
    "< 0.001",
    sprintf("= %.3f", rcs$p_value)
  )
  ph_pval_fmt <- ifelse(
    rcs$ph_pval < 0.001,
    "< 0.001",
    sprintf("= %.3f", rcs$ph_pval)
  )

  .data <- ggplot2::.data
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = target_density$x,
        ymin = min(density_y2),
        ymax = density_y2,
        fill = "density"
      ),
      color = color_density
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "gray") +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data[[target]],
        y = .data$hr_l95,
        color = "ci"
      ),
      data = rcs$prediction,
      linetype = 2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data[[target]],
        y = .data$hr_u95,
        color = "ci"
      ),
      data = rcs$prediction,
      linetype = 2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data[[target]],
        y = .data$hr,
        color = "hr"
      ),
      data = rcs$prediction,
      linewidth = 1,
      linetype = 1
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      subtitle = sprintf(
        "非线性 P 值 %s\n比例风险假设 P 值 %s",
        nl_pval_fmt,
        ph_pval_fmt
      ),
      fill = NULL,
      color = NULL
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        function(x) scales::rescale(x, density_limits, c(0, max(x))),
        name = "密度"
      )
    ) +
    ggplot2::scale_fill_manual(
      breaks = "density",
      values = ggplot2::alpha(color_density, 0.8),
      labels = "密度"
    ) +
    ggplot2::scale_color_manual(
      breaks = c("hr", "ci"),
      values = c(color_hr, color_ci),
      labels = c("风险比", "95% 置信区间")
    ) +
    ggplot2::theme_classic(base_family = font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggtext::element_markdown(),
      plot.subtitle = ggplot2::element_text(size = 8)
    )

  return(p)
}

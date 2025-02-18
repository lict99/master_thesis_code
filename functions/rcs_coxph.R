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

  points <- smoothHR::predict.HR(
    object = smoothHR::smoothHR(data = data, coxfit = rcs_model),
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
    list(target_value = data[[target]], prediction = points, p_value = pval)
  )
}

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
    ggplot2::annotate(
      geom = "label",
      x = min(target_density$x),
      y = hr_limits[2],
      label = ifelse(
        rcs$p_value < 0.001,
        "P 值 < 0.001",
        sprintf("P 值 = %.3f", rcs$p_value)
      ),
      label.size = 0,
      family = font_family,
      hjust = 0,
      vjust = 0
    ) +
    ggplot2::theme_classic(base_family = font_family) +
    ggplot2::theme(
      legend.position = "top",
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggtext::element_markdown()
    )

  return(p)
}

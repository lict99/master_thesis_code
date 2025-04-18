#' Calculate Cox Proportional Hazards Model
#'
#' An internal helper function that fits a Cox Proportional Hazards model and
#' extracts relevant statistics for the target variable.
#'
#' @param data A data frame containing the variables in the model.
#' @param time Character string specifying the time variable name.
#' @param event Character string specifying the event variable name.
#' @param target Character string specifying the target predictor variable.
#' @param covariates Character vector of covariate names to include in the
#' model.
#'
#' @return A data frame with model results.
.calc_coxph <- function(
    data,
    time,
    event,
    target,
    covariates) {
  loadNamespace("survival")

  fml <- as.formula(
    paste(
      sprintf("Surv(%s, %s)", time, event),
      "~",
      paste(c(target, covariates), collapse = " + ")
    )
  )
  model <- summary(survival::coxph(formula = fml, data = data))
  nm_target <- grep(
    paste0("^", target),
    rownames(model$coefficients),
    value = TRUE
  )
  df <- data.frame(
    event_type = event,
    n_sample = model$n,
    n_event = model$nevent,
    covariates = paste(covariates, collapse = " + "),
    target = nm_target,
    coef = model$coefficients[nm_target, "coef"],
    se = model$coefficients[nm_target, "se(coef)"],
    p_value = model$coefficients[nm_target, "Pr(>|z|)"],
    hr = model$conf.int[nm_target, "exp(coef)"],
    hr_l95 = model$conf.int[nm_target, "lower .95"],
    hr_u95 = model$conf.int[nm_target, "upper .95"]
  )

  return(df)
}

#' Calculate Cox Proportional Hazards Models for Pairwise Combinations
#'
#' Performs Cox Proportional Hazards regression for all combinations of targets,
#' covariates, and event/time pairs.
#'
#' @param data A data frame containing all the variables.
#' @param event_time_list List of event/time pairs, each containing "time" and
#' "event" elements.
#' @param targets Character vector of target predictor variables to analyze.
#' @param covariates_list List of covariate sets to include in models.
#'
#' @return A data frame combining results from all models.
calc_coxph_pairwise <- function(
    data,
    event_time_list,
    targets,
    covariates_list) {
  product_df <- expand.grid(
    target = targets,
    covariates = covariates_list,
    event_time = event_time_list,
    stringsAsFactors = FALSE
  )

  all_model_list <- lapply(
    seq_len(nrow(product_df)),
    function(i) {
      .calc_coxph(
        data = data,
        time = product_df[i, "event_time"][[1]][["time"]],
        event = product_df[i, "event_time"][[1]][["event"]],
        target = product_df[i, "target"],
        covariates = unlist(product_df[i, "covariates"], use.names = FALSE)
      )
    }
  )

  return(do.call("rbind", all_model_list))
}

#' Calculate Rolling Window Cox Proportional Hazards Models
#'
#' Fits Cox Proportional Hazards models over a series of time windows, creating
#' a rolling analysis of hazard ratios over time.
#'
#' @param data A data frame containing all variables.
#' @param time Character string specifying the time variable name.
#' @param event Character string specifying the event variable name.
#' @param target Character string specifying the target predictor variable.
#' @param covariates Character vector of covariate names to include in the
#' model.
#' @param rolling_by Character string specifying the variable to use for
#' defining windows.
#' @param start Numeric value for the start of the first time point.
#' @param end Numeric value for the end of the last time point.
#' @param window Numeric value for the width of window.
#' @param step Numeric value for the step size between consecutive windows.
#'
#' @return A data frame combining results from all time windows, with additional
#' start and end columns indicating the window boundaries.
calc_rolling_coxph <- function(
    data,
    time,
    event,
    target,
    covariates,
    rolling_by,
    start,
    end,
    window,
    step) {
  window_start <- seq(from = start, to = end - window, by = step)
  window_end <- window_start + window

  df_list <- mapply(
    FUN = function(start, end) {
      sub_idx <- data[[rolling_by]] >= start & data[[rolling_by]] <= end
      sub_data <- data[sub_idx, , drop = FALSE]
      df <- .calc_coxph(
        data = sub_data,
        time = time,
        event = event,
        target = target,
        covariates = covariates
      )
      df["start"] <- start
      df["end"] <- end
      return(df)
    },
    start = window_start,
    end = window_end,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  return(do.call("rbind", df_list))
}

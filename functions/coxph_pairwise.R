.calc_coxph <- function(
    time,
    event,
    target,
    covariates,
    data) {
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

calc_coxph_pairwise <- function(
    event_time_list,
    targets,
    covariates_list,
    data) {
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
        time = product_df[i, "event_time"][[1]][["time"]],
        event = product_df[i, "event_time"][[1]][["event"]],
        target = product_df[i, "target"],
        covariates = unlist(product_df[i, "covariates"], use.names = FALSE),
        data = data
      )
    }
  )

  return(do.call("rbind", all_model_list))
}

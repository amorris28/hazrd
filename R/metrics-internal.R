.calc_hr_metric = function(data, phs, time, event, hr_method, numerator, denominator, cxph) {

  if (hr_method == "continuous_group") {
    cutvals_numerator <- c(quantile(data[[phs]], numerator[1]), quantile(data[[phs]], numerator[2]))
    cutvals_denominator  <- c(quantile(data[[phs]], denominator[1]), quantile(data[[phs]], denominator[2]))

    beta = as.numeric(cxph$coefficients)

    ix_numerator <- which(data[[phs]] >= cutvals_numerator[1] & data[[phs]] <= cutvals_numerator[2])
    ix_denominator  <- which(data[[phs]] >= cutvals_denominator[1] & data[[phs]] <= cutvals_denominator[2])

    beta_phs <- beta * data[[phs]]
    beta_phs_numerator <- mean(beta_phs[ix_numerator])
    beta_phs_denominator <- mean(beta_phs[ix_denominator])
    estimate <- exp(beta_phs_numerator - beta_phs_denominator)
    metric = paste0("HR[", round(numerator[1] * 100), "-", round(numerator[2] * 100), 
                  "]_[", round(denominator[1]  * 100), "-", round(denominator[2]  * 100), "]")
  } else if (hr_method == "continuous_point") {
    beta       <- as.numeric(cxph$coefficients)
    phs_numerator   <- as.numeric(quantile(data[[phs]], numerator[1]))
    phs_denominator    <- as.numeric(quantile(data[[phs]], denominator[2]))
    estimate   <- exp(beta * (phs_numerator - phs_denominator))
    ix_numerator    <- which(data[[phs]] >= quantile(data[[phs]], numerator[1]))
    ix_denominator     <- which(data[[phs]] <= quantile(data[[phs]], denominator[2]))
    metric = paste0("HR", numerator[1] * 100, "_", denominator[2] * 100)
  } else if (hr_method == "categorical") {
    stop("hr_method '", hr_method, "' not yet implemented")
  } else stop("Unknown hr_method: '", hr_method, "'")

  tibble::tibble(
    metric = metric,
    estimate  = estimate,
    conf_low  = NA_real_,
    conf_high = NA_real_,
    se        = NA_real_,
    n_numerator    = length(ix_numerator),
    n_denominator     = length(ix_denominator),
    method    = hr_method,
    adjusted  = FALSE
  )
}

.calc_cindex_metric = function(data, phs, time, event, cindex_method, cxph) {
  if (cindex_method == "harrell") {
    estimate <- as.numeric(cxph$concordance['concordance'])
  } else if (cindex_method == "uno") {
    stop("Uno's C not yet implemented")
  } else stop("Unknown cindex_method: '", cindex_method, "'")

    tibble::tibble(
      metric    = "C_index",
      estimate  = estimate,
      conf_low  = NA_real_,
      conf_high = NA_real_,
      se        = NA_real_,
      n_numerator    = NA_integer_,
      n_denominator     = NA_integer_,
      method    = cindex_method,
      adjusted  = FALSE
    )
}


.calc_or_metric = function(data, phs, time, event, numerator, denominator, or_age) {
    cutvals_numerator <- c(quantile(data[[phs]], numerator[1]), quantile(data[[phs]], numerator[2]))
    cutvals_denominator <- c(quantile(data[[phs]], denominator[1]), quantile(data[[phs]], denominator[2]))

    ix_numerator <- which(data[[phs]] >= cutvals_numerator[1] & data[[phs]] <= cutvals_numerator[2])
    ix_denominator <- which(data[[phs]] >= cutvals_denominator[1] & data[[phs]] <= cutvals_denominator[2])

    data_numerator <- data[ix_numerator, ]
    data_denominator  <- data[ix_denominator, ]

    model_numerator <- coxph(Surv(data_numerator[[time]], data_numerator[[event]]) ~ 1)
    model_denominator <- coxph(Surv(data_denominator[[time]],  data_denominator[[event]])  ~ 1)

    fit_numerator <- survfit(model_numerator)
    fit_denominator <- survfit(model_denominator)

    # Explicitly handle ages outside the observed KM time range to avoid
    # silent NA/Inf propagation from approx() or division-by-zero when
    # survival == 0.
    p_numerator <- NA_real_
    p_denominator <- NA_real_

    if (length(fit_numerator$time) > 0) {
      if (or_age < min(fit_numerator$time) || or_age > max(fit_numerator$time)) {
        p_numerator <- NA_real_
      } else {
        p_numerator <- as.numeric(approx(fit_numerator$time, fit_numerator$surv, xout = or_age, rule = 1)$y)
      }
    }

    if (length(fit_denominator$time) > 0) {
      if (or_age < min(fit_denominator$time) || or_age > max(fit_denominator$time)) {
        p_denominator <- NA_real_
      } else {
        p_denominator <- as.numeric(approx(fit_denominator$time, fit_denominator$surv, xout = or_age, rule = 1)$y)
      }
    }

    # Warn and return NA if either group's KM estimate is unavailable at the
    # requested age (outside observed follow-up) or if survival probability is
    # effectively zero which would lead to infinite odds.
    if (is.na(p_numerator) || is.na(p_denominator)) {
      warning("or_age ", or_age, " is outside the observed time range for one or both groups. OR cannot be computed.")
      estimate <- NA_real_
    } else if (p_numerator <= 0 || p_denominator <= 0) {
      warning("or_age ", or_age, " results in zero survival probability for one or both groups; OR undefined.")
      estimate <- NA_real_
    } else {
      event_odds_numerator <- (1 - p_numerator) / p_numerator
      event_odds_denominator <- (1 - p_denominator) / p_denominator
      estimate <- event_odds_numerator / event_odds_denominator
    }

    tibble::tibble(
      metric = paste0("OR[", round(numerator[1] * 100), "-", round(numerator[2] * 100), 
                      "]_[", round(denominator[1]  * 100), "-", round(denominator[2]  * 100), "]_age", or_age),
      estimate  = estimate,
      conf_low  = NA_real_,
      conf_high = NA_real_,
      se        = NA_real_,
      n_numerator    = length(ix_numerator),
      n_denominator     = length(ix_denominator),
      method    = NA_character_,
      adjusted  = FALSE
    )
}

.calc_hrsd_metric = function(data, phs, time, event, cxph) {
  beta     <- as.numeric(cxph$coefficients)
  phs_sd   <- sd(data[[phs]])
  estimate <- exp(beta * phs_sd)

  tibble::tibble(
    metric        = "HR_SD",
    estimate      = estimate,
    conf_low      = NA_real_,
    conf_high     = NA_real_,
    se            = NA_real_,
    n_numerator   = NA_integer_,
    n_denominator = NA_integer_,
    method        = NA_character_,
    adjusted      = FALSE
  )
}
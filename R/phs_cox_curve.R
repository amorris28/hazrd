#' Cox model survival curves at specified PHS percentiles
#'
#' Fit a Cox proportional-hazards model with `phs` as the sole predictor and
#' return predicted survival curves for individuals at specified PHS
#' percentiles. Unlike [phs_km_curve()], these are smooth model-based
#' predictions rather than empirical group estimates.
#'
#' Percentile values are computed from `ref_data` when supplied, which
#' supports a training→validation workflow where the reference distribution
#' comes from a training cohort.
#'
#' @param data data.frame with columns specified by `phs`, `time`, `event`
#' @param phs string or numeric vector; column name or vector of PHS values
#' @param time string or numeric vector; column name or vector of event times
#' @param event string or numeric vector; column name or vector of event
#'   indicators (0/1)
#' @param percentiles numeric vector of percentiles strictly in (0, 1) at
#'   which to compute Cox-predicted survival curves; default
#'   `c(0.01, 0.05, 0.20, 0.50, 0.80, 0.95, 0.99)`
#' @param ref_data optional data.frame used to compute the PHS value at each
#'   requested percentile (training reference); if `NULL`, percentiles are
#'   computed from `data`
#' @param output `'plot'` (default) or `'data'`
#' @param conf_int logical; include confidence intervals in output/plot
#' @param conf_int_alpha numeric; alpha for confidence ribbons when plotting
#' @param palette string; colour palette for plots (default: `'hazrd'`)
#' @param ... additional args (reserved)
#'
#' @return ggplot object (when `output = 'plot'`) or data.frame (when
#'   `output = 'data'`). The data frame has columns `time`, `estimate`,
#'   `conf.low`, `conf.high`, `percentile`, and `percentile_value`.
#'
#' @importFrom survival coxph survfit Surv
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs scale_color_brewer scale_fill_brewer theme_minimal
#' @export
phs_cox_curve <- function(data,
                          phs         = "phs",
                          time        = "age",
                          event       = "status",
                          percentiles = c(0.01, 0.05, 0.20, 0.50, 0.80, 0.95, 0.99),
                          ref_data    = NULL,
                          output      = "plot",
                          conf_int    = TRUE,
                          conf_int_alpha = 0.15,
                          palette     = "hazrd",
                          ...) {

  # ── input validation ───────────────────────────────────────────────────────
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  for (col in c(phs, time, event)) {
    if (is.character(col) && !(col %in% names(data)))
      stop("Column '", col, "' not found in data.")
  }
  output <- match.arg(output, c("plot", "data"))
  if (!is.numeric(percentiles) || any(percentiles <= 0 | percentiles >= 1))
    stop("'percentiles' must be a numeric vector with all values strictly between 0 and 1.")

  # ── resolve inputs ─────────────────────────────────────────────────────────
  if (is.character(phs)   && !is.null(data)) phs_vals   <- data[[phs]]   else phs_vals   <- phs
  if (is.character(time)  && !is.null(data)) time_vals  <- data[[time]]  else time_vals  <- time
  if (is.character(event) && !is.null(data)) event_vals <- data[[event]] else event_vals <- event

  df <- data.frame(time = time_vals, event = as.integer(event_vals), phs = phs_vals)

  # ── fit Cox model ──────────────────────────────────────────────────────────
  cox_fit <- survival::coxph(survival::Surv(time, event) ~ phs, data = df)

  # ── compute PHS values at requested percentiles ────────────────────────────
  ref_phs <- if (!is.null(ref_data) && is.data.frame(ref_data)) {
    if (is.character(phs)) ref_data[[phs]] else ref_data
  } else {
    phs_vals
  }

  phs_at_pct <- stats::quantile(ref_phs, probs = percentiles, type = 7)
  pct_labels <- paste0("P", round(percentiles * 100))

  # ── predict survival curve for each percentile ────────────────────────────
  out_list <- lapply(seq_along(percentiles), function(i) {
    new_df <- data.frame(phs = as.numeric(phs_at_pct[i]))
    fit <- tryCatch(
      survival::survfit(cox_fit, newdata = new_df),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    tib <- data.frame(
      time            = fit$time,
      estimate        = fit$surv,
      conf.low        = if (!is.null(fit$lower)) fit$lower else NA_real_,
      conf.high       = if (!is.null(fit$upper)) fit$upper else NA_real_,
      percentile      = pct_labels[i],
      percentile_value = percentiles[i]
    )
    tib
  })

  out_df <- do.call(rbind, out_list)
  rownames(out_df) <- NULL

  # Clamp to [0, 1]
  if (nrow(out_df) > 0) {
    out_df$estimate  <- pmin(pmax(out_df$estimate,  0), 1)
    out_df$conf.low  <- pmin(pmax(out_df$conf.low,  0), 1)
    out_df$conf.high <- pmin(pmax(out_df$conf.high, 0), 1)
  }

  # Order factor levels by percentile value (low → high)
  out_df$percentile <- factor(out_df$percentile, levels = pct_labels)

  if (identical(output, "data")) return(out_df)

  # ── build ggplot ───────────────────────────────────────────────────────────
  p <- ggplot2::ggplot(out_df, ggplot2::aes(x = time, y = estimate, color = percentile)) +
    ggplot2::geom_line(linewidth = 0.8)

  if (conf_int) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = percentile),
      alpha = conf_int_alpha, color = NA
    )
  }

  p <- p +
    ggplot2::labs(x = "Time", y = "Survival", color = "Percentile", fill = "Percentile") +
    ggplot2::theme_minimal()

  if (identical(palette, "hazrd")) p <- p + ggplot2::scale_color_brewer(palette = "RdYlBu")

  p
}

utils::globalVariables(c("percentile", "estimate", "conf.low", "conf.high", "time"))

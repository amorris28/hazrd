#' Kaplan-Meier curves stratified by PHS percentile
#'
#' Compute Kaplan-Meier survival curves stratified by PHS percentile
#' groups. Returns either a ggplot object (output = "plot") or a tidy
#' data frame (output = "data"). Preset strata and custom cutpoints are
#' supported; percentiles are computed from `ref_data` when supplied so a
#' training->validation workflow is possible.
#'
#' @param data data.frame with columns specified by `phs`, `time`, `event`
#' @param phs string or numeric vector; column name or vector of PHS values
#' @param time string or numeric vector; column name or vector of event time
#' @param event string or numeric vector; column name or vector of event indicator (0/1)
#' @param breaks numeric vector of percentile cutpoints strictly in (0, 1);
#'   default `c(0.20, 0.80)` (bottom 20% / middle 60% / top 20%)
#' @param ref_data optional data.frame used to compute percentile cutpoints (training reference)
#' @param output 'plot' (default) or 'data'
#' @param conf_int logical; include confidence intervals in output/plot
#' @param conf_int_alpha numeric; alpha for confidence ribbons when plotting
#' @param palette string; name of color palette to use for plots (default: 'hazrd')
#' @param risk_table logical; if TRUE returns numbers-at-risk in the data output (experimental)
#' @param ... additional args (reserved)
#'
#' @return ggplot object (when output = 'plot') or data.frame (when output = 'data')
#' @importFrom survival survfit Surv
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon labs scale_color_brewer scale_fill_brewer theme_minimal
#' @export
phs_km_curve <- function(data,
                         phs = "phs",
                         time = "age",
                         event = "status",
                         breaks = c(0.20, 0.80),
                         ref_data = NULL,
                   output = "plot",
                   conf_int = TRUE,
                   conf_int_alpha = 0.15,
                   palette = "hazrd",
                   risk_table = FALSE,
                   ...) {

  # ── input validation ─────────────────────────────────────────────
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  for (col in c(phs, time, event)) {
    if (is.character(col) && !(col %in% names(data)))
      stop("Column '", col, "' not found in data.")
  }
  output <- match.arg(output, c("plot", "data"))
  if (!is.numeric(breaks) || any(breaks <= 0 | breaks >= 1))
    stop("'breaks' must be a numeric vector with all values strictly between 0 and 1.")

  # Resolve inputs (allow passing column names as strings)
  if (is.character(phs) && !is.null(data)) phs_vals <- data[[phs]] else phs_vals <- phs
  if (is.character(time) && !is.null(data)) time_vals <- data[[time]] else time_vals <- time
  if (is.character(event) && !is.null(data)) event_vals <- data[[event]] else event_vals <- event

  df <- data.frame(time = time_vals, event = as.integer(event_vals), phs = phs_vals)

  # Decide cutpoints
  cuts <- sort(unique(breaks))

  # Percentile calculation source
  ref_phs <- if (!is.null(ref_data) && is.data.frame(ref_data)) {
    if (is.character(phs)) ref_data[[phs]] else ref_data
  } else {
    phs_vals
  }

  # Compute percentile ranks in [0,1]
  phs_pct <- as.numeric(stats::ecdf(ref_phs)(phs_vals))

  # Build stratum labels and factor
  cut_bounds <- c(0, cuts, 1)
  # create human friendly labels like "Bottom 20%", "Middle 60%", "Top 20%"
  labels <- sapply(seq_along(cut_bounds[-1]), function(i) {
    lo <- round(cut_bounds[i] * 100)
    hi <- round(cut_bounds[i+1] * 100)
    paste0(lo, "-", hi, "%")
  })

  # Assign percentiles into strata
  stratum <- cut(phs_pct, breaks = cut_bounds, include.lowest = TRUE, labels = labels, right = FALSE)

  df$stratum <- stratum

  # For each stratum, compute a one-group survfit
  strata_levels <- levels(df$stratum)
  out_list <- lapply(strata_levels, function(s) {
    subdf <- df[df$stratum == s, , drop = FALSE]
    if (nrow(subdf) == 0) return(NULL)
    fit <- tryCatch(survival::survfit(survival::Surv(time, event) ~ 1, data = subdf), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    tib <- data.frame(time = fit$time,
                      estimate = fit$surv,
                      conf.low = if (!is.null(fit$lower)) fit$lower else NA_real_,
                      conf.high = if (!is.null(fit$upper)) fit$upper else NA_real_,
                      n.risk = if (!is.null(fit$n.risk)) fit$n.risk else NA_integer_,
                      n.event = if (!is.null(fit$n.event)) fit$n.event else NA_integer_)
    tib$stratum <- s
    tib
  })

  out_df <- do.call(rbind, out_list)
  rownames(out_df) <- NULL

  # Clamp survival probabilities and CI bounds to [0, 1] to avoid plotting
  # ribbons that extend beyond the valid probability range due to numeric
  # imprecision or extrapolation.
  if (nrow(out_df) > 0) {
    if ("estimate" %in% names(out_df)) {
      out_df$estimate <- pmin(pmax(out_df$estimate, 0), 1)
    }
    if ("conf.low" %in% names(out_df)) {
      out_df$conf.low <- pmin(pmax(out_df$conf.low, 0), 1)
    }
    if ("conf.high" %in% names(out_df)) {
      out_df$conf.high <- pmin(pmax(out_df$conf.high, 0), 1)
    }
  }

  if (identical(output, "data")) {
    if (risk_table) {
      # risk_table data is already included as n.risk/n.event per time x stratum
      return(out_df)
    }
    return(out_df)
  }

  # Build ggplot
  p <- ggplot2::ggplot(out_df, ggplot2::aes(x = time, y = estimate, color = stratum)) +
    ggplot2::geom_step(linewidth = 0.8)

  if (conf_int) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = stratum), alpha = conf_int_alpha, color = NA)
  }

  p <- p + ggplot2::labs(x = "Time", y = "Survival", color = "Stratum", fill = "Stratum") +
    ggplot2::theme_minimal()

  # simple palette mapping for the repo default
  if (identical(palette, "hazrd")) {
    p <- p +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::scale_fill_brewer(palette = "Set1")
  }

  p
}

utils::globalVariables(c("estimate", "conf.low", "conf.high", "time", "stratum", "percentile"))

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
#' @param intervals list of `c(lo, hi)` pairs defining percentile bands, e.g.
#'   `list(c(0.80, 1), c(0, 0.20))`. Bands may overlap. Takes precedence over
#'   `breaks` when non-`NULL`. Default produces four bands:
#'   top 5 (0.95-1), top 20 (0.80-1), middle 40 (0.30-0.70), and bottom 20 (0-0.20).
#' @param breaks numeric vector of percentile cutpoints strictly in (0, 1)
#'   used to form exclusive bands (legacy); ignored when `intervals` is non-`NULL`.
#'   Retained for backward compatibility.
#' @param ref_data optional data.frame used to compute percentile cutpoints (training reference)
#' @param output 'plot' (default) or 'data'
#' @param conf_int logical; include confidence intervals in output/plot
#' @param conf_int_alpha numeric; alpha for confidence ribbons when plotting
#' @param palette string; name of color palette to use for plots (default: 'hazrd')
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
                         intervals = list(c(0.95, 1), c(0.80, 1), c(0.30, 0.70), c(0, 0.20)),
                         breaks = NULL,
                         ref_data = NULL,
                   output = "plot",
                   conf_int = TRUE,
                   conf_int_alpha = 0.15,
                   palette = "hazrd",
                   ...) {

  # ── input validation ─────────────────────────────────────────────
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  for (col in c(phs, time, event)) {
    if (is.character(col) && !(col %in% names(data)))
      stop("Column '", col, "' not found in data.")
  }
  output <- match.arg(output, c("plot", "data"))

  # Resolve interval list: intervals takes precedence over breaks
  if (!is.null(intervals)) {
    if (!is.list(intervals) ||
        !all(sapply(intervals, function(iv)
          is.numeric(iv) && length(iv) == 2 && iv[1] >= 0 && iv[2] <= 1 && iv[1] < iv[2])))
      stop("'intervals' must be a list of c(lo, hi) vectors with 0 <= lo < hi <= 1.")
    interval_list <- intervals
  } else if (!is.null(breaks)) {
    if (!is.numeric(breaks) || any(breaks <= 0 | breaks >= 1))
      stop("'breaks' must be a numeric vector with all values strictly between 0 and 1.")
    cut_bounds <- c(0, sort(unique(breaks)), 1)
    interval_list <- lapply(seq_len(length(cut_bounds) - 1),
                            function(i) c(cut_bounds[i], cut_bounds[i + 1]))
  } else {
    stop("One of 'intervals' or 'breaks' must be non-NULL.")
  }

  # Resolve inputs (allow passing column names as strings)
  if (is.character(phs) && !is.null(data)) phs_vals <- data[[phs]] else phs_vals <- phs
  if (is.character(time) && !is.null(data)) time_vals <- data[[time]] else time_vals <- time
  if (is.character(event) && !is.null(data)) event_vals <- data[[event]] else event_vals <- event

  df <- data.frame(time = time_vals, event = as.integer(event_vals), phs = phs_vals)

  # Percentile calculation source
  ref_phs <- if (!is.null(ref_data) && is.data.frame(ref_data)) {
    if (is.character(phs)) ref_data[[phs]] else ref_data
  } else {
    phs_vals
  }

  # Compute percentile ranks in [0,1]
  phs_pct <- as.numeric(stats::ecdf(ref_phs)(phs_vals))

  # Helper: human-friendly label for an interval
  make_label <- function(lo, hi) paste0(round(lo * 100), "-", round(hi * 100), "%")

  # For each interval, compute a one-group survfit on the subset
  out_list <- lapply(interval_list, function(iv) {
    lo <- iv[1]; hi <- iv[2]
    upper_mask <- if (hi >= 1) phs_pct <= hi else phs_pct < hi
    mask <- phs_pct >= lo & upper_mask
    subdf <- df[mask, , drop = FALSE]
    if (nrow(subdf) == 0) return(NULL)
    label <- make_label(lo, hi)
    fit <- tryCatch(survival::survfit(survival::Surv(time, event) ~ 1, data = subdf), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    tib <- data.frame(time = fit$time,
                      estimate = fit$surv,
                      conf.low = if (!is.null(fit$lower)) fit$lower else NA_real_,
                      conf.high = if (!is.null(fit$upper)) fit$upper else NA_real_,
                      n.risk = if (!is.null(fit$n.risk)) fit$n.risk else NA_integer_,
                      n.event = if (!is.null(fit$n.event)) fit$n.event else NA_integer_)
    tib$stratum <- label
    tib
  })

  out_df <- do.call(rbind, out_list)
  rownames(out_df) <- NULL

  # Preserve user-specified interval ordering in the legend
  if (!is.null(out_df) && nrow(out_df) > 0) {
    out_df$stratum <- factor(
      out_df$stratum,
      levels = sapply(interval_list, function(iv) make_label(iv[1], iv[2]))
    )
  }

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
    return(out_df)
  }

  # Build ggplot
  x_range <- range(out_df$time, na.rm = TRUE)
  p <- ggplot2::ggplot(out_df, ggplot2::aes(x = time, y = estimate, color = stratum)) +
    ggplot2::geom_step(linewidth = 0.8)

  if (conf_int) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = stratum), alpha = conf_int_alpha, color = NA)
  }

  p <- p +
    ggplot2::scale_x_continuous(limits = x_range) +
    ggplot2::labs(x = "Time", y = "Survival", color = "Stratum", fill = "Stratum") +
    ggplot2::theme_minimal()

  if (identical(palette, "hazrd")) {
    p <- p +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::scale_fill_brewer(palette = "Set1")
  }

  p
}

utils::globalVariables(c("estimate", "conf.low", "conf.high", "time", "stratum"))



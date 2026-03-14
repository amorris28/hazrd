#' Cumulative incidence (absolute risk) curves stratified by PHS percentile
#'
#' Computes cumulative incidence curves for PHS percentile-stratified groups
#' using 1 - Kaplan-Meier (method = "km") or the Aalen-Johansen estimator
#' when competing risks are present (method = "aalen_johansen"). Returns a
#' ggplot2 object or a tidy data frame.
#'
#' @param data data.frame with columns specified by `phs`, `time`, and `event`
#' @param phs string; column name of the continuous PHS values
#' @param time string; column name of the time-to-event variable
#' @param event string; column name of the event indicator (0 = censored,
#'   1 = event of interest)
#' @param competing_event string or NULL; column name of the competing event
#'   indicator (0/1). Required when `method = "aalen_johansen"`.
#' @param intervals list of `c(lo, hi)` pairs defining percentile bands, e.g.
#'   `list(c(0.80, 1), c(0, 0.20))`. Bands may overlap. Default produces four
#'   bands matching `phs_km_curve()`: top 5 (0.95-1), top 20 (0.80-1),
#'   middle 40 (0.30-0.70), and bottom 20 (0-0.20).
#' @param ref_data optional data.frame used as the reference population for
#'   computing percentile cutpoints (e.g., a training cohort).
#' @param method string; estimation method. `"km"` (default): cumulative
#'   incidence as 1 minus the Kaplan-Meier survival estimate. Appropriate
#'   when censoring is independent and no competing risks are present.
#'   `"aalen_johansen"`: Aalen-Johansen cumulative incidence estimator;
#'   recommended when competing risks are present (requires
#'   `competing_event`). `"fine_gray"`: stub, not yet implemented.
#' @param time_points numeric vector or NULL; when `output = "data"`, return
#'   estimates interpolated to these specific time points (one row per
#'   stratum x time_point combination). When NULL, all event-time rows are
#'   returned.
#' @param output string; `"plot"` (default) returns a ggplot object,
#'   `"data"` returns a tidy data frame.
#' @param conf_int logical; whether to include confidence intervals
#'   (default TRUE).
#' @param conf_int_alpha numeric; transparency for CI ribbons when plotting
#'   (default 0.15).
#' @param palette string; color palette name (default `"hazrd"`).
#' @param risk_table logical; if TRUE and `output = "plot"`, attach a
#'   numbers-at-risk table below the plot. Requires the
#'   \pkg{patchwork} package (default FALSE).
#' @param ... additional arguments (reserved for future use).
#'
#' @return A ggplot object when `output = "plot"`, or a tidy data frame when
#'   `output = "data"`. The data frame has columns `time`, `stratum`,
#'   `risk_estimate`, `conf.low`, `conf.high`, `n.risk`, `n.event`. When
#'   `time_points` is supplied with `output = "data"`, one row per stratum x
#'   time_point is returned.
#'
#' @examples
#' data(test_data)
#'
#' # Default: cumulative incidence plot (top 5, top 20, middle 40, bottom 20)
#' phs_abs_risk(test_data)
#'
#' # Return tidy data for custom plotting
#' abs_data <- phs_abs_risk(test_data, output = "data")
#'
#' # Extract estimates at specific time points
#' phs_abs_risk(test_data, time_points = c(60, 70, 80), output = "data")
#'
#' # Custom intervals
#' phs_abs_risk(test_data,
#'   intervals = list(c(0.80, 1), c(0, 0.20)))
#'
#' @importFrom survival survfit Surv
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon labs
#'   scale_color_brewer scale_fill_brewer scale_x_continuous
#'   scale_y_continuous theme_minimal
#' @importFrom scales percent_format
#' @export
phs_abs_risk <- function(
    data,
    phs             = "phs",
    time            = "age",
    event           = "status",
    competing_event = NULL,
    intervals       = list(c(0.95, 1), c(0.80, 1), c(0.30, 0.70), c(0, 0.20)),
    ref_data        = NULL,
    method          = "km",
    time_points     = NULL,
    output          = "plot",
    conf_int        = TRUE,
    conf_int_alpha  = 0.15,
    palette         = "hazrd",
    risk_table      = FALSE,
    ...) {

  # ── input validation ────────────────────────────────────────────────────────
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.")
  for (col in c(phs, time, event)) {
    if (is.character(col) && !(col %in% names(data)))
      stop("Column '", col, "' not found in data.")
  }
  if (!is.null(competing_event) && is.character(competing_event) &&
      !(competing_event %in% names(data)))
    stop("Column '", competing_event, "' not found in data.")

  output <- match.arg(output, c("plot", "data"))
  method <- match.arg(method, c("km", "aalen_johansen", "fine_gray"))

  if (identical(method, "fine_gray"))
    stop("method = 'fine_gray' is not yet implemented.")
  if (identical(method, "aalen_johansen") && is.null(competing_event))
    stop("method = 'aalen_johansen' requires 'competing_event' to be specified.")

  # ── validate and store interval list ──────────────────────────────────────
  interval_list <- .resolve_abs_risk_intervals(intervals)

  # ── extract column vectors ──────────────────────────────────────────────────
  phs_vals   <- if (is.character(phs))   data[[phs]]   else phs
  time_vals  <- if (is.character(time))  data[[time]]  else time
  event_vals <- if (is.character(event)) data[[event]] else event
  comp_vals  <- if (!is.null(competing_event)) {
    if (is.character(competing_event)) data[[competing_event]] else competing_event
  } else NULL

  df <- data.frame(time  = time_vals,
                   event = as.integer(event_vals),
                   phs   = phs_vals)
  if (!is.null(comp_vals))
    df$competing <- as.integer(comp_vals)

  # ── percentile ranks ────────────────────────────────────────────────────────
  ref_phs <- if (!is.null(ref_data) && is.data.frame(ref_data)) {
    if (is.character(phs)) ref_data[[phs]] else ref_data
  } else phs_vals
  phs_pct <- as.numeric(stats::ecdf(ref_phs)(phs_vals))

  # ── helpers: stratum label constructor ──────────────────────────────────────
  make_short_label <- function(lo, hi)
    paste0(round(lo * 100), "-", round(hi * 100), "%")

  # ── per-stratum cumulative incidence ────────────────────────────────────────
  out_list <- lapply(interval_list, function(iv) {
    lo <- iv[1]; hi <- iv[2]
    upper_mask <- if (hi >= 1) phs_pct <= hi else phs_pct < hi
    mask  <- phs_pct >= lo & upper_mask
    subdf <- df[mask, , drop = FALSE]
    if (nrow(subdf) == 0L) return(NULL)

    short_lbl <- make_short_label(lo, hi)

    tib <- if (identical(method, "km")) {
      fit <- tryCatch(
        survival::survfit(survival::Surv(time, event) ~ 1, data = subdf),
        error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      # CIF = 1 - S(t); CI bounds are reversed relative to survival CI
      data.frame(
        time          = fit$time,
        risk_estimate = pmin(pmax(1 - fit$surv,   0), 1),
        conf.low      = pmin(pmax(if (!is.null(fit$upper)) 1 - fit$upper else NA_real_, 0), 1),
        conf.high     = pmin(pmax(if (!is.null(fit$lower)) 1 - fit$lower else NA_real_, 0), 1),
        n.risk        = if (!is.null(fit$n.risk))  fit$n.risk  else NA_integer_,
        n.event       = if (!is.null(fit$n.event)) fit$n.event else NA_integer_
      )
    } else {
      # aalen_johansen: factor event encoding 0=censored, 1=event, 2=competing
      status_aj <- factor(
        ifelse(subdf$event == 1L, 1L,
               ifelse(subdf$competing == 1L, 2L, 0L)),
        levels = 0:2
      )
      fit <- tryCatch(
        survival::survfit(survival::Surv(subdf$time, status_aj) ~ 1),
        error = function(e) NULL)
      if (is.null(fit) || is.null(fit$pstate)) return(NULL)
      # pstate columns: "0" (event-free), "1" (CIF for event), "2" (CIF for competing)
      ci_col <- which(colnames(fit$pstate) == "1")
      if (length(ci_col) == 0L) ci_col <- 2L
      data.frame(
        time          = fit$time,
        risk_estimate = pmin(pmax(fit$pstate[, ci_col], 0), 1),
        conf.low      = pmin(pmax(
          if (!is.null(fit$lower)) fit$lower[, ci_col] else NA_real_, 0), 1),
        conf.high     = pmin(pmax(
          if (!is.null(fit$upper)) fit$upper[, ci_col] else NA_real_, 0), 1),
        n.risk        = if (!is.null(fit$n.risk))  fit$n.risk  else NA_integer_,
        n.event       = if (!is.null(fit$n.event)) fit$n.event else NA_integer_
      )
    }

    if (is.null(tib) || nrow(tib) == 0L) return(NULL)
    tib$stratum <- short_lbl
    tib
  })

  out_df <- do.call(rbind, out_list)
  rownames(out_df) <- NULL

  # Ordered factor preserves the interval_list ordering in legends and tables
  short_levels <- sapply(interval_list, function(iv) make_short_label(iv[1], iv[2]))
  out_df$stratum <- factor(out_df$stratum, levels = short_levels)

  # ── output = 'data' ─────────────────────────────────────────────────────────
  if (identical(output, "data")) {
    if (!is.null(time_points)) {
      out_df <- .interpolate_cif_at_times(out_df, time_points, short_levels)
    }
    return(out_df)
  }

  # ── build ggplot ─────────────────────────────────────────────────────────────
  x_range <- range(out_df$time, na.rm = TRUE)
  p <- ggplot2::ggplot(
    out_df,
    ggplot2::aes(x = time, y = risk_estimate, color = stratum)
  ) +
    ggplot2::geom_step(linewidth = 0.8)

  if (conf_int) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = stratum),
      alpha = conf_int_alpha, color = NA
    )
  }

  p <- p +
    ggplot2::scale_x_continuous(limits = x_range) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(x = "Time", y = "Cumulative Risk",
                  color = "Stratum", fill = "Stratum") +
    ggplot2::theme_minimal()

  if (identical(palette, "hazrd")) {
    p <- p +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::scale_fill_brewer(palette = "Set1")
  }

  if (risk_table) {
    return(.attach_risk_table(p, out_df, x_range = x_range, palette = palette))
  }

  p
}

# ── Internal: validate interval list ─────────────────────────────────────────
.resolve_abs_risk_intervals <- function(intervals) {
  if (!is.list(intervals) ||
      !all(sapply(intervals, function(iv)
        is.numeric(iv) && length(iv) == 2 && iv[1] >= 0 && iv[2] <= 1 && iv[1] < iv[2])))
    stop("'intervals' must be a list of c(lo, hi) vectors with 0 <= lo < hi <= 1.")
  intervals
}

# ── Internal: interpolate CIF step function at requested time points ──────────
.interpolate_cif_at_times <- function(out_df, time_points, short_levels) {
  rows <- lapply(short_levels, function(s) {
    sub  <- out_df[out_df$stratum == s, , drop = FALSE]
    lapply(time_points, function(t) {
      prior_idx <- which(sub$time <= t)
      if (length(prior_idx) == 0L) {
        data.frame(time = t, stratum = s,
                   risk_estimate = 0, conf.low = 0, conf.high = 0,
                   n.risk = max(sub$n.risk, na.rm = TRUE), n.event = 0L,
                   stringsAsFactors = FALSE)
      } else {
        r <- sub[max(prior_idx), , drop = FALSE]
        data.frame(time = t, stratum = s,
                   risk_estimate = r$risk_estimate,
                   conf.low      = r$conf.low,
                   conf.high     = r$conf.high,
                   n.risk        = r$n.risk,
                   n.event       = r$n.event,
                   stringsAsFactors = FALSE)
      }
    })
  })
  tbl <- do.call(rbind, do.call(c, rows))
  tbl$stratum <- factor(tbl$stratum, levels = short_levels)
  rownames(tbl) <- NULL
  tbl
}

utils::globalVariables(c("risk_estimate", "conf.low", "conf.high"))

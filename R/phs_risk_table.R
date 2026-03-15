#' Numbers-at-risk table for PHS percentile-stratified groups
#'
#' Builds a ggplot2 numbers-at-risk strip intended for combining with a
#' [phs_abs_risk()] or [phs_km_curve()] plot using patchwork. Accepts the
#' same data and interval arguments as those functions and computes the
#' survival data internally.
#'
#' @param data data.frame with columns specified by `phs`, `time`, and `event`
#' @param phs string; column name of the continuous PHS values
#' @param time string; column name of the time-to-event variable
#' @param event string; column name of the event indicator (0 = censored,
#'   1 = event of interest)
#' @param competing_event string or NULL; column name of the competing event
#'   indicator. Required when `method = "aalen_johansen"`.
#' @param intervals list of `c(lo, hi)` pairs defining percentile bands.
#'   Should match the `intervals` used in the accompanying
#'   [phs_abs_risk()] or [phs_km_curve()] call.
#' @param ref_data optional data.frame used as the reference population for
#'   computing percentile cutpoints.
#' @param method string; `"km"` (default) or `"aalen_johansen"`. Should
#'   match the method used in the accompanying curve function.
#' @param palette string; color palette name. Should match the palette used
#'   in the accompanying curve plot (default `"hazrd"`).
#' @param time_points numeric vector or NULL; time values at which to display
#'   numbers at risk. For example, `c(50, 60, 70, 80, 90)`. When NULL
#'   (default), values are chosen automatically using `pretty()`.
#' @param ... additional arguments (reserved for future use).
#'
#' @return A ggplot2 object displaying the numbers at risk over time for each
#'   stratum. Combine with the output of [phs_abs_risk()] or
#'   [phs_km_curve()] using patchwork, e.g.:
#'   `p / phs_risk_table(data) + patchwork::plot_layout(heights = c(3, 1))`.
#'
#' @examples
#' data(test_data)
#'
#' p  <- phs_abs_risk(test_data)
#' rt <- phs_risk_table(test_data)
#'
#' # Specify exact ages for the at-risk columns:
#' rt <- phs_risk_table(test_data, time_points = c(50, 60, 70, 80))
#'
#' # Combine with patchwork:
#' # p / rt + patchwork::plot_layout(heights = c(3, 1))
#'
#' @export
phs_risk_table <- function(
    data,
    phs             = "phs",
    time            = "age",
    event           = "status",
    competing_event = NULL,
    intervals       = list(c(0.95, 1), c(0.80, 1), c(0.30, 0.70), c(0, 0.20)),
    ref_data        = NULL,
    method          = "km",
    palette         = "hazrd",
    time_points     = NULL,
    ...) {

  out_df <- phs_abs_risk(
    data            = data,
    phs             = phs,
    time            = time,
    event           = event,
    competing_event = competing_event,
    intervals       = intervals,
    ref_data        = ref_data,
    method          = method,
    output          = "data"
  )

  x_range <- range(out_df$time, na.rm = TRUE)

  if (!is.null(time_points)) {
    risk_times <- sort(unique(as.numeric(time_points)))
  } else {
    risk_times <- pretty(x_range, n = 5L)
    risk_times <- risk_times[risk_times >= x_range[1] & risk_times <= x_range[2]]
    if (length(risk_times) == 0L) risk_times <- x_range
  }

  risk_tbl <- .risk_at_times(out_df, risk_times)
  .build_risk_table_gg(risk_tbl, x_range = x_range, palette = palette)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

# For each stratum, look up n.risk at each requested time point using
# step-function semantics: at time t, report the n.risk at the last observed
# event time <= t. Before the first event, use the largest n.risk in the
# stratum as a proxy for the initial at-risk count.
.risk_at_times <- function(out_df, risk_times) {
  strata_levels <- levels(out_df$stratum)
  result <- lapply(strata_levels, function(s) {
    sub <- out_df[out_df$stratum == s, , drop = FALSE]
    n_at_t <- vapply(risk_times, function(t) {
      prior_idx <- which(sub$time <= t)
      if (length(prior_idx) == 0L) {
        max(sub$n.risk, na.rm = TRUE)
      } else {
        sub$n.risk[max(prior_idx)]
      }
    }, numeric(1L))
    data.frame(
      time    = risk_times,
      stratum = factor(s, levels = strata_levels),
      n_risk  = as.integer(round(n_at_t)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, result)
}

# Build a ggplot text-table showing at-risk counts per stratum per time point.
# Strata are listed bottom-to-top (reversed) so the top-most row in the table
# corresponds to the first legend entry of the curve above it.
.build_risk_table_gg <- function(risk_tbl, x_range, palette = "hazrd") {
  orig_levels <- levels(risk_tbl$stratum)
  p <- ggplot2::ggplot(risk_tbl,
                       ggplot2::aes(x = time, y = stratum,
                                    label = n_risk, color = stratum)) +
    ggplot2::geom_text(size = 3.2, show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = x_range) +
    ggplot2::scale_y_discrete(limits = rev(orig_levels)) +
    ggplot2::labs(x = "Time", y = NULL, title = "Number at risk") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(size = 9, face = "plain"),
      plot.margin  = ggplot2::margin(0, 5.5, 5.5, 5.5)
    )
  if (identical(palette, "hazrd")) {
    p <- p + ggplot2::scale_color_brewer(palette = "Set1", drop = FALSE)
  }
  p
}

utils::globalVariables(c("n_risk", "stratum", "time"))

#' Attach a numbers-at-risk table below a curve plot
#'
#' Combines a [phs_abs_risk()] or [phs_km_curve()] plot with a numbers-at-risk
#' strip using patchwork. Accepts either an already-built risk table ggplot
#' (from [phs_risk_table()]) or a raw data.frame, in which case
#' [phs_risk_table()] is called internally with any extra arguments passed via
#' `...`.
#'
#' @param plot ggplot object returned by [phs_abs_risk()] or [phs_km_curve()],
#'   optionally with additional layers applied.
#' @param risk_table ggplot object returned by [phs_risk_table()], or a
#'   data.frame. When a data.frame is supplied, [phs_risk_table()] is called
#'   internally; any additional arguments (e.g. `intervals`, `time_points`,
#'   `palette`) are forwarded via `...`.
#' @param heights numeric vector of length 2 controlling the relative heights
#'   of the curve and table panels (default `c(3, 1)`).
#' @param ... additional arguments forwarded to [phs_risk_table()] when
#'   `risk_table` is a data.frame. Ignored when `risk_table` is already a
#'   ggplot.
#'
#' @return A patchwork object combining the curve and risk table.
#'
#' @examples
#' data(test_data)
#'
#' # Quick usage: build and attach in two lines
#' p <- phs_abs_risk(test_data)
#' attach_risk_table(p, test_data)
#'
#' # Full control: customise both objects before combining
#' p  <- phs_abs_risk(test_data) + ggplot2::theme_classic()
#' rt <- phs_risk_table(test_data, time_points = c(55, 65, 75, 85))
#' attach_risk_table(p, rt)
#'
#' @importFrom ggplot2 is.ggplot
#' @export
attach_risk_table <- function(plot, risk_table, heights = c(3, 1), ...) {
  if (!ggplot2::is.ggplot(plot))
    stop("'plot' must be a ggplot object.")

  if (!ggplot2::is.ggplot(risk_table)) {
    if (!is.data.frame(risk_table))
      stop("'risk_table' must be a ggplot object or a data.frame.")
    risk_table <- phs_risk_table(risk_table, ...)
  }

  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').",
         call. = FALSE)

  patchwork::wrap_plots(plot, risk_table, ncol = 1L, heights = heights)
}

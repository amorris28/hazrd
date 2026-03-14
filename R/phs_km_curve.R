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
                         intervals = list(c(0.95, 1), c(0.80, 1), c(0.30, 0.70), c(0, 0.20)),
                         breaks = NULL,
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

  if (risk_table) {
    return(.attach_risk_table(p, out_df, x_range = x_range, palette = palette))
  }

  p
}

# ── Internal: risk table helpers ─────────────────────────────────────────────

# For each stratum, look up n.risk at each requested time point using the
# step-function semantics: at time t, report the n.risk recorded at the
# last observed event time <= t.  Before the first event, use the largest
# n.risk in the stratum as a proxy for the initial at-risk count.
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
# corresponds to the first legend entry of the survival curve above it.
.build_risk_table_gg <- function(risk_tbl, x_range, palette = "hazrd") {
  orig_levels        <- levels(risk_tbl$stratum)
  risk_tbl$stratum   <- factor(risk_tbl$stratum, levels = rev(orig_levels))
  p <- ggplot2::ggplot(risk_tbl,
                       ggplot2::aes(x = time, y = stratum,
                                    label = n_risk, color = stratum)) +
    ggplot2::geom_text(size = 3.2, show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = x_range) +
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

# Combine a survival/CIF plot with a risk table strip using patchwork.
# Falls back to returning the plain plot with a warning when patchwork is
# not installed.
.attach_risk_table <- function(p_main, out_df, x_range, palette = "hazrd") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning(
      "Package 'patchwork' is required to display a risk table. ",
      "Install it with install.packages('patchwork'). ",
      "Returning the plot without a risk table.",
      call. = FALSE
    )
    return(p_main)
  }
  risk_times <- pretty(x_range, n = 5L)
  risk_times <- risk_times[risk_times >= x_range[1] & risk_times <= x_range[2]]
  if (length(risk_times) == 0L) risk_times <- x_range
  risk_tbl <- .risk_at_times(out_df, risk_times)
  p_risk   <- .build_risk_table_gg(risk_tbl, x_range = x_range, palette = palette)
  patchwork::wrap_plots(p_main, p_risk, ncol = 1L, heights = c(3, 1))
}

utils::globalVariables(c("estimate", "conf.low", "conf.high", "time", "stratum",
                          "percentile", "n_risk"))

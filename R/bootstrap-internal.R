# ── Bootstrap internals ───────────────────────────────────────────────────────
#
# .boot_statistic()  – passed to boot::boot(); returns a named numeric vector
#                      of point estimates for every requested metric.
# .run_bootstrap()   – orchestrates boot::boot() + boot::boot.ci() and merges
#                      CI/SE results back into the point-estimate tibble.
#
# Design notes:
#   • The statistic function re-fits the Cox model on each resample so that HR,
#     HR_SD, and C-index all derive from the same resample-level model — exactly
#     as they do in the non-bootstrap path in phs_metrics().
#   • boot::boot() returns one numeric *vector* per replicate; we track metric
#     order via a character vector (metric_names) so we can match CIs back to
#     the right rows in the output tibble.
#   • parallel / n_cores are wired through to boot::boot() but default to "no"
#     / 1L; adding real parallel support later is just a matter of removing the
#     stub check below.

# ------------------------------------------------------------------------------
# .boot_statistic
# ------------------------------------------------------------------------------
# data      – full data frame
# indices   – row indices selected by boot::boot() for this replicate
# phs, time, event – column name strings (same convention as phs_metrics)
# hr_method, hr_pairs, cindex_method, or_age, or_pairs – forwarded metric args
# metrics   – character vector; controls which metrics to compute
#
# Returns a *named* numeric vector whose names match the `metric` column of the
# point-estimate tibble.  boot::boot() requires a plain numeric vector; names
# are preserved and used by .run_bootstrap() to align CIs.

.boot_statistic <- function(data, indices,
                             phs, time, event,
                             metrics,
                             hr_method, hr_pairs,
                             cindex_method,
                             or_age, or_pairs) {

  d <- data[indices, , drop = FALSE]

  # Re-fit Cox on the resample (shared across HR, HR_SD, C-index)
  needs_coxph <- any(c("HR", "C_index", "HR_SD") %in% metrics)
  cxph <- if (needs_coxph) {
    tryCatch(
      survival::coxph(survival::Surv(d[[time]], d[[event]]) ~ d[[phs]]),
      error = function(e) NULL
    )
  } else NULL

  estimates <- c()

  if ("HR" %in% metrics) {
    hr_rows <- lapply(hr_pairs, function(pair) {
      tryCatch(
        .calc_hr_metric(d, phs, time, event, hr_method,
                        pair$numerator, pair$denominator, cxph),
        error = function(e) {
          # Return a placeholder row with NA so the vector length stays stable
          tibble::tibble(metric = NA_character_, estimate = NA_real_)
        }
      )
    })
    hr_tbl <- dplyr::bind_rows(hr_rows)
    vals <- stats::setNames(hr_tbl$estimate, hr_tbl$metric)
    estimates <- c(estimates, vals)
  }

  if ("C_index" %in% metrics) {
    ci_row <- tryCatch(
      .calc_cindex_metric(d, phs, time, event, cindex_method, cxph = cxph),
      error = function(e) tibble::tibble(metric = "C_index", estimate = NA_real_)
    )
    estimates <- c(estimates, stats::setNames(ci_row$estimate, ci_row$metric))
  }

  if ("HR_SD" %in% metrics) {
    hrsd_row <- tryCatch(
      .calc_hrsd_metric(d, phs, time, event, cxph),
      error = function(e) tibble::tibble(metric = "HR_SD", estimate = NA_real_)
    )
    estimates <- c(estimates, stats::setNames(hrsd_row$estimate, hrsd_row$metric))
  }

  if ("OR" %in% metrics) {
    or_rows <- lapply(or_pairs, function(pair) {
      lapply(or_age, function(age) {
        tryCatch(
          suppressWarnings(
            .calc_or_metric(d, phs, time, event,
                            pair$numerator, pair$denominator, age)
          ),
          error = function(e) {
            nm <- paste0("OR[", round(pair$numerator[1] * 100), "-",
                         round(pair$numerator[2] * 100), "]_[",
                         round(pair$denominator[1] * 100), "-",
                         round(pair$denominator[2] * 100), "]_age", age)
            tibble::tibble(metric = nm, estimate = NA_real_)
          }
        )
      })
    })
    or_tbl <- dplyr::bind_rows(unlist(or_rows, recursive = FALSE))
    vals <- stats::setNames(or_tbl$estimate, or_tbl$metric)
    estimates <- c(estimates, vals)
  }

  # boot::boot() requires an unnamed (or consistently named) numeric vector;
  # names are carried along as an attribute and used for alignment.
  estimates
}


# ------------------------------------------------------------------------------
# .run_bootstrap
# ------------------------------------------------------------------------------
# point_estimates  – tibble returned by the non-bootstrap path of phs_metrics()
# data, phs, time, event, metrics, hr_method, hr_pairs,
# cindex_method, or_age, or_pairs  – forwarded to .boot_statistic()
# n_boot           – number of replicates
# ci_level         – e.g. 0.95
# boot_method      – "percentile", "bca", or "normal"
# seed             – optional integer
# parallel         – "no", "multicore", or "snow" (passed to boot::boot())
# n_cores          – integer; passed as `ncpus` to boot::boot()
# strata_col       – optional string; column name to stratify resampling on
#
# Returns the same tibble as point_estimates with conf_low, conf_high, se filled.

.run_bootstrap <- function(point_estimates,
                            data,
                            phs, time, event,
                            metrics,
                            hr_method, hr_pairs,
                            cindex_method,
                            or_age, or_pairs,
                            n_boot      = 1000L,
                            ci_level    = 0.95,
                            boot_method = "percentile",
                            seed        = NULL,
                            parallel    = "no",
                            n_cores     = 1L,
                            strata_col  = NULL) {

  # -- stub guard for parallel (easy to remove when implementing) --------------
  if (!identical(parallel, "no")) {
    stop(
      "parallel = '", parallel, "' is not yet implemented. ",
      "Use parallel = 'no' for now."
    )
  }

  # -- input checks ------------------------------------------------------------
  boot_method <- match.arg(boot_method, c("percentile", "bca", "normal"))

  if (!is.null(seed)) set.seed(seed)

  # -- strata vector -----------------------------------------------------------
  # boot::boot() requires strata to be an integer or factor vector.
  # Coerce character columns silently so callers don't need to pre-convert.
  strata_vec <- if (!is.null(strata_col)) {
    if (!(strata_col %in% names(data))) {
      stop("strata column '", strata_col, "' not found in data.")
    }
    col <- data[[strata_col]]
    if (is.character(col)) as.factor(col) else col
  } else {
    rep(1L, nrow(data))   # boot::boot treats a single level as no stratification
  }

  # -- run boot ----------------------------------------------------------------
  boot_out <- boot::boot(
    data      = data,
    statistic = .boot_statistic,
    R         = n_boot,
    strata    = strata_vec,
    parallel  = parallel,
    ncpus     = n_cores,
    # extra args forwarded to .boot_statistic:
    phs           = phs,
    time          = time,
    event         = event,
    metrics       = metrics,
    hr_method     = hr_method,
    hr_pairs      = hr_pairs,
    cindex_method = cindex_method,
    or_age        = or_age,
    or_pairs      = or_pairs
  )

  # boot_out$t  is an  R × p  matrix; boot_out$t0  is the length-p vector of
  # observed statistics.  Column order matches the names on boot_out$t0.
  metric_names <- names(boot_out$t0)
  n_metrics    <- length(metric_names)

  # -- extract CI & SE per metric ----------------------------------------------
  ci_list <- lapply(seq_len(n_metrics), function(i) {

    # boot.ci type mapping
    bci_type <- switch(boot_method,
      percentile = "perc",
      bca        = "bca",
      normal     = "norm"
    )

    ci_obj <- tryCatch(
      boot::boot.ci(boot_out, conf = ci_level, type = bci_type, index = i),
      error = function(e) NULL
    )

    # Extract bounds from the boot.ci object
    # Each type lives in a differently named slot; last two elements are lo/hi.
    bounds <- if (!is.null(ci_obj)) {
      slot_name <- switch(bci_type,
        perc = "percent",
        bca  = "bca",
        norm = "normal"
      )
      ci_mat <- ci_obj[[slot_name]]
      if (!is.null(ci_mat)) {
        as.numeric(ci_mat[length(ci_mat) - 1:0])   # [lo, hi]
      } else c(NA_real_, NA_real_)
    } else c(NA_real_, NA_real_)

    # SE from the bootstrap distribution (SD of replicates)
    col_vals <- boot_out$t[, i]
    se_val   <- stats::sd(col_vals, na.rm = TRUE)

    list(
      metric    = metric_names[i],
      conf_low  = bounds[1],
      conf_high = bounds[2],
      se        = se_val
    )
  })

  ci_tbl <- dplyr::bind_rows(lapply(ci_list, tibble::as_tibble))

  # -- merge back into point_estimates -----------------------------------------
  # Left-join on `metric` so rows with no bootstrap result keep NA.
  # Drop the incoming NA placeholders (.orig suffix) then enforce the canonical
  # column order so bootstrap = TRUE and bootstrap = FALSE output is identical
  # in structure (left_join appends new columns at the end by default).
  result <- dplyr::left_join(point_estimates, ci_tbl, by = "metric",
                              suffix = c(".orig", ""))

  for (col in c("conf_low", "conf_high", "se")) {
    orig_col <- paste0(col, ".orig")
    if (orig_col %in% names(result)) result[[orig_col]] <- NULL
  }

  canonical_cols <- c("metric", "estimate", "conf_low", "conf_high", "se",
                       "n_numerator", "n_denominator", "method", "adjusted")
  result[, canonical_cols]
}
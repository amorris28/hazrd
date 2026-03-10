#' Calculate evaluation metrics for Polygenic Hazard Scores
#'
#' @param data a data.frame containing the columns specified by \code{phs},
#'   \code{time}, and \code{event}
#' @param phs a string specifying the column name containing the polygenic
#'   hazard score
#' @param time a string specifying the column name containing the time to event
#'   or censoring
#' @param event a string specifying the column name containing the event
#'   indicator (0 = censored, 1 = event)
#' @param metrics a character vector specifying which metrics to compute.
#'   Options are \code{"HR"}, \code{"C_index"}, \code{"HR_SD"}, \code{"OR"}.
#'
#' @param hr_method the method to use for HR calculation. One of
#'   \code{"continuous_group"} (default), \code{"continuous_point"}, or
#'   \code{"categorical"}. Only \code{"continuous_group"} is currently
#'   implemented.
#' @param hr_numerator numeric. The lower boundary of the numerator group as a
#'   percentile (e.g., 0.80 for the top 20%). The numerator group is defined as
#'   \[hr_numerator, 1.0\]. Ignored if \code{hr_pairs} is provided. Default
#'   is 0.80.
#' @param hr_denominator numeric. The upper boundary of the denominator group
#'   as a percentile (e.g., 0.20 for the bottom 20%). The denominator group is
#'   defined as \[0.0, hr_denominator\]. Ignored if \code{hr_pairs} is
#'   provided. Default is 0.20.
#' @param hr_pairs a list of HR specifications for computing multiple HRs in
#'   one call. Each element should be a named list with \code{numerator} and
#'   \code{denominator}, each a length-2 numeric vector specifying the
#'   \[lower, upper\] percentile boundaries of each band. Cannot be combined
#'   with \code{hr_numerator}/\code{hr_denominator}. Example:
#'   \preformatted{hr_pairs = list(
#'     list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
#'     list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
#'   )}
#'
#' @param cindex_method the method to use for C-index calculation. One of
#'   \code{"harrell"} (default) or \code{"uno"} (not yet implemented).
#'
#' @param or_age an integer or numeric vector specifying the age(s) at which
#'   the odds ratio should be calculated. Required when \code{"OR"} is in
#'   \code{metrics}. One row is returned per age.
#' @param or_numerator numeric. Lower boundary of the numerator band for OR
#'   calculation. Default is 0.80.
#' @param or_denominator numeric. Upper boundary of the denominator band for OR
#'   calculation. Default is 0.20.
#' @param or_pairs Same structure as \code{hr_pairs} but for OR calculation.
#'
#' @param bootstrap logical. Whether to compute bootstrapped confidence
#'   intervals. Default is \code{FALSE}. When \code{TRUE}, \code{conf_low},
#'   \code{conf_high}, and \code{se} are populated in the returned tibble.
#' @param n_boot integer. Number of bootstrap replicates. Default is 1000.
#' @param ci_level numeric. Confidence level for bootstrap CIs. Default is
#'   0.95.
#' @param boot_method character. Method for deriving CIs from the bootstrap
#'   distribution. One of \code{"percentile"} (default), \code{"bca"}
#'   (bias-corrected accelerated), or \code{"normal"}.
#' @param seed optional integer. Random seed for reproducibility.
#' @param parallel character. Parallelisation backend passed to
#'   \code{boot::boot()}. One of \code{"no"} (default), \code{"multicore"},
#'   or \code{"snow"}. Only \code{"no"} is currently implemented.
#' @param n_cores integer. Number of cores for parallel bootstrapping. Passed
#'   as \code{ncpus} to \code{boot::boot()}. Default is 1L.
#' @param strata optional string. Column name to stratify resampling on
#'   (preserves case/control ratio within each level). Default is \code{NULL}.
#'
#' @details
#' ## HR Arguments
#' \code{hr_numerator} and \code{hr_denominator} are convenience arguments for
#' the common case of a single HR comparing the top and bottom of the PHS
#' distribution. For multiple HRs or non-standard reference bands (e.g., a
#' middle reference group), use \code{hr_pairs} instead. Providing both will
#' raise an error.
#'
#' ## Bootstrapping
#' When \code{bootstrap = TRUE}, \code{phs_metrics()} calls \code{boot::boot()}
#' internally. On each replicate the Cox model is re-fitted on the resampled
#' data so that all metrics derived from it (HR, HR_SD, C-index) are jointly
#' consistent. Failures in individual replicates (e.g., degenerate resamples)
#' produce \code{NA} for that replicate and are excluded from the SE/CI
#' calculation with a warning.
#'
#' @return a tibble with one row per metric and columns:
#' \describe{
#'   \item{metric}{Full explicit metric name, e.g. \code{"HR[80-100]_[0-20]"},
#'     \code{"C_index"}, \code{"OR[80-100]_[0-20]_age70"}}
#'   \item{estimate}{Point estimate computed on the full dataset}
#'   \item{conf_low}{Lower CI bound (\code{NA} if \code{bootstrap = FALSE})}
#'   \item{conf_high}{Upper CI bound (\code{NA} if \code{bootstrap = FALSE})}
#'   \item{se}{Bootstrap standard error (\code{NA} if \code{bootstrap = FALSE})}
#'   \item{n_numerator}{Sample size in the numerator group (HR and OR only)}
#'   \item{n_denominator}{Sample size in the denominator group (HR and OR only)}
#'   \item{method}{Method flag used, e.g. \code{"continuous_group"},
#'     \code{"harrell"}; \code{NA} for OR}
#'   \item{adjusted}{Whether covariates were used (always \code{FALSE} until
#'     covariate support is added)}
#' }
#'
#' @importFrom survival coxph Surv
#' @importFrom dplyr bind_rows left_join
#' @examples
#' # Simple case – HR[80-100]_[0-20]
#' phs_metrics(test_data, metrics = "HR", hr_numerator = 0.80, hr_denominator = 0.20)
#'
#' # Multiple HRs with custom bands
#' phs_metrics(test_data, metrics = "HR",
#'   hr_pairs = list(
#'     list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
#'     list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
#'   ))
#'
#' # With bootstrapped CIs
#' phs_metrics(test_data, metrics = c("HR", "C_index"),
#'   bootstrap = TRUE, n_boot = 500, seed = 42)
#' @export
phs_metrics <- function(data,
                        phs            = "phs",
                        time           = "age",
                        event          = "status",
                        metrics        = c("HR", "C_index"),
                        hr_method      = "continuous_group",
                        hr_numerator   = NULL,
                        hr_denominator = NULL,
                        hr_pairs       = NULL,
                        cindex_method  = "harrell",
                        or_age         = NULL,
                        or_numerator   = NULL,
                        or_denominator = NULL,
                        or_pairs       = NULL,
                        bootstrap      = FALSE,
                        n_boot         = 999L,
                        ci_level       = 0.95,
                        boot_method    = "percentile",
                        seed           = NULL,
                        parallel       = "no",
                        n_cores        = 1L,
                        strata         = NULL) {

  # ── input validation ────────────────────────────────────────────────────────
  for (col in c(phs, time, event)) {
    if (!(col %in% names(data))) stop("Column '", col, "' not found in data.")
  }
  hr_method   <- match.arg(hr_method,  c("continuous_group", "continuous_point", "categorical"))
  cindex_method <- match.arg(cindex_method, c("harrell", "uno"))
  valid_metrics <- c("HR", "C_index", "HR_SD", "OR")
  bad_metrics   <- setdiff(metrics, valid_metrics)
  if (length(bad_metrics) > 0) {
    stop(
      "Unknown metric(s): ", paste(bad_metrics, collapse = ", "), ". ",
      "Valid options are: ", paste(valid_metrics, collapse = ", "), "."
    )
  }
  boot_method <- match.arg(boot_method, c("percentile", "bca", "normal"))
  parallel    <- match.arg(parallel,   c("no", "multicore", "snow"))

  # ── resolve HR pairs ────────────────────────────────────────────────────────
  if ("HR" %in% metrics) {
    if (!is.null(hr_pairs) && (!is.null(hr_numerator) || !is.null(hr_denominator))) {
      stop("Provide either hr_numerator/hr_denominator or hr_pairs, not both.")
    }
    if (is.null(hr_pairs)) {
      hr_numerator   <- if (is.null(hr_numerator))   0.80 else hr_numerator
      hr_denominator <- if (is.null(hr_denominator)) 0.20 else hr_denominator
      hr_pairs <- list(list(numerator   = c(hr_numerator, 1.00),
                            denominator = c(0.00, hr_denominator)))
    }
  } else {
    hr_pairs <- hr_pairs %||% list()
  }

  # ── resolve OR pairs ────────────────────────────────────────────────────────
  if ("OR" %in% metrics) {
    if (is.null(or_age)) stop("'or_age' is required when 'OR' is included in metrics.")
    if (!is.null(or_pairs) && (!is.null(or_numerator) || !is.null(or_denominator))) {
      stop("Provide either or_numerator/or_denominator or or_pairs, not both.")
    }
    if (is.null(or_pairs)) {
      or_numerator   <- if (is.null(or_numerator))   0.80 else or_numerator
      or_denominator <- if (is.null(or_denominator)) 0.20 else or_denominator
      or_pairs <- list(list(numerator   = c(or_numerator, 1.00),
                            denominator = c(0.00, or_denominator)))
    }
  } else {
    or_pairs <- or_pairs %||% list()
    or_age   <- or_age  %||% numeric(0)
  }

  # ── point estimates (always computed on the full dataset) ───────────────────
  needs_coxph <- any(c("HR", "C_index", "HR_SD") %in% metrics)
  cxph <- if (needs_coxph) {
    coxph(Surv(data[[time]], data[[event]]) ~ data[[phs]])
  } else NULL

  results <- list()

  if ("HR" %in% metrics) {
    hr_rows <- lapply(hr_pairs, function(pair) {
      .calc_hr_metric(data, phs, time, event, hr_method,
                      pair$numerator, pair$denominator, cxph)
    })
    results[["HR"]] <- dplyr::bind_rows(hr_rows)
  }

  if ("C_index" %in% metrics) {
    results[["C_index"]] <- .calc_cindex_metric(data, phs, time, event,
                                                cindex_method, cxph = cxph)
  }

  if ("HR_SD" %in% metrics) {
    results[["HR_SD"]] <- .calc_hrsd_metric(data, phs, time, event, cxph)
  }

  if ("OR" %in% metrics) {
    or_rows <- lapply(or_pairs, function(pair) {
      lapply(or_age, function(age) {
        .calc_or_metric(data, phs, time, event,
                        pair$numerator, pair$denominator, age)
      })
    })
    results[["OR"]] <- dplyr::bind_rows(unlist(or_rows, recursive = FALSE))
  }

  point_estimates <- dplyr::bind_rows(results)

  # ── bootstrap ───────────────────────────────────────────────────────────────
  if (!bootstrap) return(point_estimates)

  .run_bootstrap(
    point_estimates = point_estimates,
    data            = data,
    phs             = phs,
    time            = time,
    event           = event,
    metrics         = metrics,
    hr_method       = hr_method,
    hr_pairs        = hr_pairs,
    cindex_method   = cindex_method,
    or_age          = or_age,
    or_pairs        = or_pairs,
    n_boot          = n_boot,
    ci_level        = ci_level,
    boot_method     = boot_method,
    seed            = seed,
    parallel        = parallel,
    n_cores         = n_cores,
    strata_col      = strata
  )
}
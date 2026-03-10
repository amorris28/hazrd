# ── Deprecated functions ──────────────────────────────────────────────────────
#
# These functions are retained for backward compatibility only.
# They emit a deprecation warning and forward to phs_metrics().
#
# Removal target: v0.3.0

# ------------------------------------------------------------------------------
# get_hr()
# Signature from latest release: data, phs, age, status, lower_interval,
# upper_interval, CI, bootstrap_iterations, swc, swc_popnumcases,
# swc_popnumcontrols
# lower_interval / upper_interval each accept a single quantile value OR a
# length-2 vector; single values are expanded to [0, val] and [val, 1]
# respectively.  swc has no equivalent in phs_metrics() yet.
# ------------------------------------------------------------------------------

#' Deprecated: use phs_metrics() instead
#'
#' `get_hr()` is deprecated as of hazrd 0.2.0. Please use
#' [phs_metrics()] with `metrics = "HR"` instead.
#'
#' @param data a data.frame containing PHS data
#' @param phs column name for PHS values. Default `"phs"`
#' @param age column name for time-to-event. Default `"age"`
#' @param status column name for event indicator. Default `"status"`
#' @param lower_interval single quantile (upper bound of denominator band,
#'   lower bound defaults to 0) or length-2 vector `c(lower, upper)`.
#'   Default `0.20`
#' @param upper_interval single quantile (lower bound of numerator band,
#'   upper bound defaults to 1) or length-2 vector `c(lower, upper)`.
#'   Default `0.80`
#' @param CI logical. If `TRUE`, bootstrap CIs are computed. Default `FALSE`
#' @param bootstrap_iterations number of bootstrap replicates. Default `1000`
#' @param swc logical. Sample weight correction. Not supported in
#'   `phs_metrics()` — a warning is emitted if `TRUE`. Default `FALSE`
#' @param swc_popnumcases ignored (swc not yet implemented in `phs_metrics()`)
#' @param swc_popnumcontrols ignored (swc not yet implemented in `phs_metrics()`)
#'
#' @return A list with elements `index`, `value`, `conf.low`, `conf.high`,
#'   and `iters`, matching the return structure of the previous release.
#'
#' @seealso [phs_metrics()]
#' @export
get_hr <- function(data                 = NULL,
                   phs                  = "phs",
                   age                  = "age",
                   status               = "status",
                   lower_interval       = 0.20,
                   upper_interval       = 0.80,
                   CI                   = FALSE,
                   bootstrap_iterations = 1000,
                   swc                  = FALSE,
                   swc_popnumcases      = NULL,
                   swc_popnumcontrols   = NULL) {

  # Expand single-value intervals to bands, matching old calc_hr behaviour:
  # lower_interval = 0.20  ->  denominator = c(0.00, 0.20)
  # upper_interval = 0.80  ->  numerator   = c(0.80, 1.00)
  denominator <- if (length(lower_interval) == 1L) c(0.00, lower_interval) else lower_interval
  numerator   <- if (length(upper_interval) == 1L) c(upper_interval, 1.00) else upper_interval

  if (swc) {
    warning(
      "Sample weight correction (swc) is not yet implemented in phs_metrics(). ",
      "The swc argument will be ignored."
    )
  }

  .Deprecated(
    new = "phs_metrics",
    msg = paste0(
      "'get_hr()' is deprecated as of hazrd 0.2.0.\n",
      "Use phs_metrics(data, metrics = 'HR', hr_pairs = list(\n",
      "  list(numerator   = c(", numerator[1],   ", ", numerator[2],   "),\n",
      "       denominator = c(", denominator[1], ", ", denominator[2], "))\n",
      ")) instead."
    )
  )

  result <- phs_metrics(
    data      = data,
    phs       = phs,
    time      = age,
    event     = status,
    metrics   = "HR",
    hr_pairs  = list(list(numerator = numerator, denominator = denominator)),
    bootstrap = CI,
    n_boot    = bootstrap_iterations,
    ci_level  = 0.95
  )

  # Reproduce the old return structure exactly so existing code keeps working
  list(
    index      = paste0("HR",
                        scales::label_percent(suffix = "")(upper_interval[[length(upper_interval)]]),
                        "_",
                        scales::label_percent(suffix = "")(lower_interval[[1L]])),
    value      = result$estimate,
    conf.low   = result$conf_low,
    conf.high  = result$conf_high,
    iters      = if (CI) bootstrap_iterations else NULL
  )
}


# ------------------------------------------------------------------------------
# get_cindex()   [old first arg: data]
# ------------------------------------------------------------------------------

#' Deprecated: use phs_metrics() instead
#'
#' `get_cindex()` is deprecated as of hazrd 0.2.0. Please use
#' [phs_metrics()] with `metrics = "C_index"` instead.
#'
#' @param data a data.frame containing PHS data
#' @param phs column name for PHS values. Default `"phs"`
#' @param age column name for time-to-event. Default `"age"`
#' @param status column name for event indicator. Default `"status"`
#' @param bootstrap.iterations number of bootstrap replicates, or `NULL`
#' @param conf.level confidence level. Default `0.95`
#'
#' @return A list with element `value` and optionally `conf.low`/`conf.high`.
#'
#' @seealso [phs_metrics()]
#' @export
get_cindex <- function(data                 = NULL,
                       phs                  = "phs",
                       age                  = "age",
                       status               = "status",
                       bootstrap.iterations = NULL,
                       conf.level           = 0.95) {
  .Deprecated(
    new = "phs_metrics",
    msg = paste0(
      "'get_cindex()' is deprecated as of hazrd 0.2.0.\n",
      "Use phs_metrics(data, metrics = 'C_index') instead."
    )
  )

  do_boot <- !is.null(bootstrap.iterations)

  result <- phs_metrics(
    data      = data,
    phs       = phs,
    time      = age,
    event     = status,
    metrics   = "C_index",
    bootstrap = do_boot,
    n_boot    = if (do_boot) bootstrap.iterations else 1000L,
    ci_level  = conf.level
  )

  out <- list(value = result$estimate)
  if (do_boot) {
    out$conf.low  <- result$conf_low
    out$conf.high <- result$conf_high
  }
  out
}


# ------------------------------------------------------------------------------
# get_hrsd()   [old first arg: data; bootstrap triggered by conf.int flag]
# ------------------------------------------------------------------------------

#' Deprecated: use phs_metrics() instead
#'
#' `get_hrsd()` is deprecated as of hazrd 0.2.0. Please use
#' [phs_metrics()] with `metrics = "HR_SD"` instead.
#'
#' @param data a data.frame containing PHS data
#' @param phs column name for PHS values. Default `"phs"`
#' @param age column name for time-to-event. Default `"age"`
#' @param status column name for event indicator. Default `"status"`
#' @param conf.int logical. If `TRUE`, bootstrap CIs are computed.
#'   Default `FALSE`
#' @param conf.level confidence level. Default `0.95`
#' @param bootstrap.iterations number of bootstrap replicates. Required when
#'   `conf.int = TRUE`
#'
#' @return A list with element `value` and optionally `conf.low`/`conf.high`.
#'
#' @seealso [phs_metrics()]
#' @export
get_hrsd <- function(data                 = NULL,
                     phs                  = "phs",
                     age                  = "age",
                     status               = "status",
                     conf.int             = FALSE,
                     conf.level           = 0.95,
                     bootstrap.iterations) {
  .Deprecated(
    new = "phs_metrics",
    msg = paste0(
      "'get_hrsd()' is deprecated as of hazrd 0.2.0.\n",
      "Use phs_metrics(data, metrics = 'HR_SD') instead."
    )
  )

  result <- phs_metrics(
    data      = data,
    phs       = phs,
    time      = age,
    event     = status,
    metrics   = "HR_SD",
    bootstrap = conf.int,
    n_boot    = if (conf.int) bootstrap.iterations else 1000L,
    ci_level  = conf.level
  )

  out <- list(value = result$estimate)
  if (conf.int) {
    out$conf.low  <- result$conf_low
    out$conf.high <- result$conf_high
  }
  out
}


# ------------------------------------------------------------------------------
# get_or()   [old first arg: data; or_age is required via missing()]
# ------------------------------------------------------------------------------

#' Deprecated: use phs_metrics() instead
#'
#' `get_or()` is deprecated as of hazrd 0.2.0. Please use
#' [phs_metrics()] with `metrics = "OR"` instead.
#'
#' @param data a data.frame containing PHS data
#' @param phs column name for PHS values. Default `"phs"`
#' @param age column name for time-to-event. Default `"age"`
#' @param status column name for event indicator. Default `"status"`
#' @param or_age age at which to compute the OR. Required.
#' @param numerator length-2 numeric vector for numerator band.
#'   Default `c(0.8, 1.0)`
#' @param denominator length-2 numeric vector for denominator band.
#'   Default `c(0.0, 0.2)`
#' @param bootstrap.iterations number of bootstrap replicates, or `NULL`
#' @param conf.level confidence level. Default `0.95`
#'
#' @return A list with element `value` and optionally `conf.low`/`conf.high`.
#'
#' @seealso [phs_metrics()]
#' @export
get_or <- function(data                 = NULL,
                   phs                  = "phs",
                   age                  = "age",
                   status               = "status",
                   or_age,
                   numerator            = c(0.8, 1.0),
                   denominator          = c(0.0, 0.2),
                   bootstrap.iterations = NULL,
                   conf.level           = 0.95) {
  if (missing(or_age) || is.null(or_age)) {
    stop("Argument 'or_age' is required. Please specify the age at which to compute the OR.")
  }

  .Deprecated(
    new = "phs_metrics",
    msg = paste0(
      "'get_or()' is deprecated as of hazrd 0.2.0.\n",
      "Use phs_metrics(data, metrics = 'OR', or_age = ", or_age, ") instead."
    )
  )

  do_boot <- !is.null(bootstrap.iterations)

  result <- phs_metrics(
    data      = data,
    phs       = phs,
    time      = age,
    event     = status,
    metrics   = "OR",
    or_age    = or_age,
    or_pairs  = list(list(numerator = numerator, denominator = denominator)),
    bootstrap = do_boot,
    n_boot    = if (do_boot) bootstrap.iterations else 1000L,
    ci_level  = conf.level
  )

  out <- list(value = result$estimate)
  if (do_boot) {
    out$conf.low  <- result$conf_low
    out$conf.high <- result$conf_high
  }
  out
}


# ------------------------------------------------------------------------------
# km_curve()   [old args: data, phs, age, status, interval, age_range, scale,
#               inverse]
# age / status map to time / event in phs_km_curve().
# interval, age_range, scale, and inverse have no equivalent in phs_km_curve() —
# warnings are emitted and they are ignored.
# ------------------------------------------------------------------------------

#' Deprecated: use phs_km() instead
#'
#' `km_curve()` is deprecated as of hazrd 0.2.0. Please use
#' [phs_km_curve()] instead.
#'
#' @param data a data.frame containing PHS data
#' @param phs column name for PHS values. Default `"phs"`
#' @param age column name for time-to-event. Default `"age"`
#' @param status column name for event indicator. Default `"status"`
#' @param interval numeric vector of length 2 specifying the lower and upper
#'   quantiles for filtering subjects. Default `c(0, 1)`. Has no direct
#'   equivalent in `phs_km()` — a warning is emitted and the argument is
#'   ignored; all subjects are included in the stratified output.
#' @param age_range ages over which to calculate curves. Default `40:100`.
#'   Not supported in `phs_km()` — a warning is emitted and the argument
#'   is ignored.
#' @param scale logical. If `TRUE`, PHS is centred and scaled to unit
#'   variance. Not supported in `phs_km()` — a warning is emitted and the
#'   argument is ignored. Scale your PHS column before calling `phs_km()`.
#' @param inverse logical. If `TRUE`, PHS is multiplied by `-1`. Not
#'   supported in `phs_km()` — a warning is emitted and the argument is
#'   ignored. Multiply your PHS column manually before calling `phs_km()`.
#'
#' @return A data.frame as returned by [phs_km_curve()] with `output = "data"`.
#'
#' @seealso [phs_km_curve()]
#' @export
km_curve <- function(data      = NULL,
                     phs       = "phs",
                     age       = "age",
                     status    = "status",
                     interval  = c(0, 1),
                     age_range = 40:100,
                     scale     = FALSE,
                     inverse   = FALSE) {

  scale   <- as.logical(scale)
  inverse <- as.logical(inverse)

  if (isTRUE(scale)) {
    warning(
      "The 'scale' argument is not supported in phs_km(). ",
      "It will be ignored. Scale your PHS column manually before calling phs_km()."
    )
  }
  if (isTRUE(inverse)) {
    warning(
      "The 'inverse' argument is not supported in phs_km(). ",
      "It will be ignored. Multiply your PHS column by -1 before calling phs_km()."
    )
  }
  if (!isTRUE(all.equal(age_range, 40:100, check.attributes = FALSE))) {
    warning(
      "The 'age_range' argument is not supported in phs_km(). ",
      "It will be ignored."
    )
  }
  if (!isTRUE(all.equal(as.numeric(interval), c(0, 1)))) {
    warning(
      "The 'interval' argument is not directly supported in phs_km(). ",
      "phs_km() stratifies subjects into groups rather than filtering to a single band. ",
      "The interval argument will be ignored; all subjects are included."
    )
  }

  phs_name    <- if (is.character(phs)    && length(phs)    == 1L) phs    else "phs"
  age_name    <- if (is.character(age)    && length(age)    == 1L) age    else "age"
  status_name <- if (is.character(status) && length(status) == 1L) status else "status"

  .Deprecated(
    new = "phs_km_curve",
    msg = paste0(
      "'km_curve()' is deprecated as of hazrd 0.2.0.\n",
      "Use phs_km_curve(data, phs = \"", phs_name, "\", time = \"", age_name,
      "\", event = \"", status_name, "\") instead."
    )
  )

  phs_km_curve(
    data   = data,
    phs    = phs,
    time   = age,
    event  = status,
    output = "data"
  )
}
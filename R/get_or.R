#' Return odds ratios (OR) from a PHS model at a specified age
#'
#' `get_or()` calculates odds ratios from either a fitted PHS object (`phsfit`) 
#' or directly from a data.frame at a specified age. It returns a list containing 
#' the point estimate and optionally bootstrap confidence intervals.
#'
#' @param object An object for which an odds ratio is desired. Typically a 
#'   data.frame or a `phsfit` object.
#' @param or_age Integer specifying the age at which the odds ratio should be calculated.
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject. The default is "phs" 
#' @param age an optional string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age" 
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). The default is "status"
#' @param numerator Numeric vector of quantiles for the numerator (e.g., `c(0.80, 1.0)`).
#' @param denominator Numeric vector of quantiles for the denominator (e.g., `c(0.0, 0.2)`).
#' @param bootstrap.iterations Number of bootstrap iterations (used only in data.frame method).
#' @param conf.level Confidence level for intervals (default 0.95).
#' @param ... Additional arguments (currently ignored)
#'
#' @return A list containing:
#' \describe{
#'   \item{value}{Point estimate of the OR}
#'   \item{conf.low}{Lower bound of confidence interval (if bootstrapped)}
#'   \item{conf.high}{Upper bound of confidence interval (if bootstrapped)}
#' }
#'
#' @examples
#' # Using a data.frame
#' OR <- get_or(test_data, or_age = 70, bootstrap.iterations = 300)
#'
#' # Using a phsfit object
#' phsfit <- fit_phs(test_data, bootstrap.iterations = 300)
#' OR <- get_or(phsfit, or_age = 70)
#'
#' @export
get_or <- function(object, or_age, ...) UseMethod("get_or")

#' @rdname get_or
#' @export
get_or.data.frame <- function(object,
                              or_age,
                              phs = "phs",
                              age = "age",
                              status = "status",
                              numerator = c(0.8, 1.0),
                              denominator = c(0.0, 0.2),
                              bootstrap.iterations = NULL,
                              conf.level = 0.95,
                              ...) {  
    if (missing(or_age) || is.null(or_age)) {
        stop("Argument 'or_age' is required. Please specify the age at which to compute the OR.")
    }
    
    data <- object
    for (col in c(phs, age, status)) {
        if (!(col %in% names(data))) stop("Column `", col, "` not found in `data`.")
    }
    
    phs_vec <- if (is.character(phs)) data[[phs]] else phs
    age_vec <- if (is.character(age)) data[[age]] else age
    status_vec <- if (is.character(status)) data[[status]] else status
    
    df <- data.frame(phs = phs_vec, age = age_vec, status = status_vec)
    OR <- calc_or(df, or_age, numerator, denominator)
    res <- list(value = OR)
    
    if (!is.null(bootstrap.iterations)) {
        boot_out <- boot_conf(df,
                              bootstrap.iterations,
                              conf.level,
                              calc_or,
                              or_age,
                              numerator,
                              denominator)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    
    return(res)
}

#' @rdname get_or
#' @export
get_or.phsfit <- function(object,
                          or_age,
                          numerator = c(0.8, 1.0),
                          denominator = c(0.0, 0.2),
                          bootstrap.iterations = NULL,
                          conf.level = 0.95,
                          ...) {
    if (missing(or_age) || is.null(or_age)) {
        stop("Argument 'or_age' is required. Please specify the age at which to compute the OR.")
    }
    
    phsfit <- object
    data <- phsfit$data
    bootstrap.iterations <- phsfit$bootstrap.iterations
    
    OR <- calc_or(data, or_age, numerator, denominator)
    res <- list(value = OR)
    
    if (is.null(bootstrap.iterations)) {
        warning("No bootstrap resamples stored in 'phsfit'; returning point estimate only.")
    } else {
        boot_out <- boot_conf(data,
                              bootstrap.iterations,
                              conf.level,
                              calc_or,
                              or_age,
                              numerator,
                              denominator)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    
    return(res)
}

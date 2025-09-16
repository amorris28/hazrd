#' Return hazard ratios (HR) from a PHS model
#'
#' `get_hr()` calculates hazard ratios from either a fitted PHS object (`phsfit`) 
#' or directly from a data.frame. It returns a list containing the point estimate 
#' and optionally bootstrap confidence intervals.
#'
#' @param object An object for which a hazard ratio is desired. Typically a 
#'   data.frame or a `phsfit` object.
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
#'   \item{value}{Point estimate of the HR}
#'   \item{conf.low}{Lower bound of confidence interval (if bootstrapped)}
#'   \item{conf.high}{Upper bound of confidence interval (if bootstrapped)}
#' }
#'
#' @examples
#' # Using a data.frame
#' HR <- get_hr(test_data, bootstrap.iterations = 300)
#'
#' # Using a phsfit object
#' phsfit <- fit_phs(test_data, bootstrap.iterations = 300)
#' HR <- get_hr(phsfit)
#'
#' @export
get_hr <- function(object, ...) UseMethod("get_hr")

#' @rdname get_hr
#' @export
get_hr.data.frame <- function(object,
                              phs = "phs",
                              age = "age",
                              status = "status",
                              numerator = c(0.80, 1.0),
                              denominator = c(0.0, 0.20),
                              bootstrap.iterations = NULL,
                              conf.level = 0.95,
                              ...) {
    data <- object
    
    for (col in c(phs, age, status)) {
        if (!(col %in% names(data))) stop("Column `", col, "` not found in `data`.")
    }
    
    phs_vec <- if (is.character(phs)) data[[phs]] else phs
    age_vec <- if (is.character(age)) data[[age]] else age
    status_vec <- if (is.character(status)) data[[status]] else status
    
    df <- data.frame(phs = phs_vec, age = age_vec, status = status_vec)
    HR <- calc_hr(df, numerator, denominator)
    res <- list(value = HR)
    
    if (!is.null(bootstrap.iterations)) {
        boot_out <- boot_conf(df,
                              bootstrap.iterations,
                              conf.level,
                              calc_hr,
                              numerator,
                              denominator)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    
    return(res)
}

#' @rdname get_hr
#' @export
get_hr.phsfit <- function(object,
                          numerator = c(0.80, 1.0),
                          denominator = c(0.0, 0.20),
                          conf.level = 0.95,
                          ...) {
    phsfit <- object
    df <- phsfit$data
    bootstrap.iterations <- phsfit$bootstrap.iterations
    
    HR <- calc_hr(df, numerator, denominator)
    res <- list(value = HR)
    alpha <- (1 - conf.level) / 2
    
    if (is.null(bootstrap.iterations)) {
        warning("No bootstrap resamples stored in 'phsfit'; returning point estimate only.")
    } else {
        boot_out <- boot_conf(df,
                              bootstrap.iterations,
                              conf.level,
                              calc_hr,
                              numerator,
                              denominator)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    
    return(res)
}

    # if (is.null(phsfit$boot)) {
    #     warning("No bootstrap resamples stored in 'phsfit'; returning point estimate only.")
    # } else {
    #     # reuse your bootstrapped coxph models to compute HR quickly
    #     # (or bootstrap indices saved)
    #     boot_values <- sapply(phsfit$boot, function(m) {
    #         calc_hr(m$data, numerator, denominator)
    #     })
    #     qs <- quantile(boot_values, c(alpha, 1 - alpha))
    #     boot_out <- list(quantiles = qs)
    #     res$conf.low <- boot_out$quantiles[[1]]
    #     res$conf.high <- boot_out$quantiles[[2]]
    # }


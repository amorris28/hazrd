#' Return the hazard ratio for a 1 standard deviation increase in PHS
#'
#' `get_hrsd()` calculates the hazard ratio for a 1-SD increase in polygenic hazard scores 
#' from either a fitted PHS object (`phsfit`) or directly from a data.frame. 
#' Optionally, bootstrap confidence intervals can be computed.
#'
#' @param object An object for which the HRSD is desired. Typically a data.frame or a `phsfit` object.
#' @param phs Optional string specifying the column in `data` containing polygenic hazard scores. Default: "phs".
#' @param age Optional string specifying the column in `data` containing ages. Default: "age".
#' @param status Optional string specifying the column in `data` containing case-control status (0=censored, 1=event). Default: "status".
#' @param bootstrap.iterations Number of bootstrap iterations (used only in data.frame method or if not precomputed in phsfit).
#' @param conf.level Confidence level for intervals (default 0.95).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A list containing:
#' \describe{
#'   \item{value}{Point estimate of the hazard ratio for 1-SD increase in PHS}
#'   \item{conf.low}{Lower bound of confidence interval (if bootstrapped)}
#'   \item{conf.high}{Upper bound of confidence interval (if bootstrapped)}
#' }
#'
#' @examples
#' # Using a data.frame
#' HRSD <- get_hrsd(test_data, bootstrap.iterations = 300)
#'
#' # Using a phsfit object
#' phsfit <- fit_phs(test_data, bootstrap.iterations = 300)
#' HRSD <- get_hrsd(phsfit)
#'
#' @export
get_hrsd <- function(object, ...) UseMethod("get_hrsd")

#' @rdname get_hrsd
#' @export
get_hrsd.data.frame <- function(object,
                                phs = "phs",
                                age = "age",
                                status = "status",
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
    
    HR <- calc_hrsd(df)
    res <- list(value = HR)
    
    if (!is.null(bootstrap.iterations)) {
        boot_out <- boot_conf(df,
                              bootstrap.iterations,
                              conf.level,
                              calc_hrsd)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    } else {
        warning("No bootstrap resamples provided; returning point estimate only.")
    }
    
    return(res)
}

#' @rdname get_hrsd
#' @export
get_hrsd.phsfit <- function(object,
                            bootstrap.iterations = NULL,
                            conf.level = 0.95,
                            ...) {
    phsfit <- object
    data <- phsfit$data
    bootstrap.iterations <- phsfit$bootstrap.iterations
    
    HR <- calc_hrsd(data)
    res <- list(value = HR)
    
    if (!is.null(bootstrap.iterations)) {
        boot_out <- boot_conf(data,
                              bootstrap.iterations,
                              conf.level,
                              calc_hrsd)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    } else {
        warning("No bootstrap resamples stored in 'phsfit'; returning point estimate only.")
    }
    
    return(res)
}

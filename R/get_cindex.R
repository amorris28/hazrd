#' Return concordance index (C-index) from a PHS model
#'
#' `get_cindex()` calculates the concordance index from either a fitted PHS object
#' (`phsfit`) or directly from a data.frame. Optionally, bootstrap confidence intervals 
#' can be computed.
#'
#' @param object An object for which a concordance index is desired. Typically a 
#'   data.frame or a `phsfit` object.
#' @param phs Optional string specifying the column in `data` containing polygenic hazard scores. Default: "phs".
#' @param age Optional string specifying the column in `data` containing ages. Default: "age".
#' @param status Optional string specifying the column in `data` containing case-control status (0=censored, 1=event). Default: "status".
#' @param bootstrap.iterations Number of bootstrap iterations (used only in data.frame method or if not precomputed in phsfit).
#' @param conf.level Confidence level for intervals (default 0.95).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A list containing:
#' \describe{
#'   \item{value}{Point estimate of the concordance index}
#'   \item{conf.low}{Lower bound of confidence interval (if bootstrapped)}
#'   \item{conf.high}{Upper bound of confidence interval (if bootstrapped)}
#' }
#'
#' @examples
#' # Using a data.frame
#' c_index <- get_cindex(test_data, bootstrap.iterations = 300)
#'
#' # Using a phsfit object
#' phsfit <- fit_phs(test_data, bootstrap.iterations = 300)
#' c_index <- get_cindex(phsfit)
#'
#' @export
get_cindex <- function(object, ...) UseMethod("get_cindex")

#' @rdname get_cindex
#' @export
get_cindex.data.frame <- function(object,
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
    
    c_index <- calc_cindex(df)
    res <- list(value = c_index)
    
    if (!is.null(bootstrap.iterations)) {
        boot_out <- boot_conf(df,
                              bootstrap.iterations,
                              conf.level,
                              calc_cindex)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    
    return(res)
}

#' @rdname get_cindex
#' @export
get_cindex.phsfit <- function(object,
                              bootstrap.iterations = NULL,
                              conf.level = 0.95,
                              ...) {
    phsfit <- object
    data <- phsfit$data
    bootstrap.iterations <- phsfit$bootstrap.iterations
    
    c_index <- calc_cindex(data)
    res <- list(value = c_index)
    
    if (is.null(bootstrap.iterations)) {
        warning("No bootstrap resamples stored in 'phsfit'; returning point estimate only.")
    } else {
        boot_out <- boot_conf(data,
                              bootstrap.iterations,
                              conf.level,
                              calc_cindex)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    
    return(res)
}

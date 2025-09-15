#' Return the hazard ratio (HR) from a PHS model
#'
#' `get_hr()` is a generic function that calculate the HR from a coxph fit or 
#' directly from a data frame with specified columns. It returns a list with 
#' the HR point estimate and optional bootstrap confidence intervals
#'
#' @param object An object for which a hazard ratio is desired. See the method documentation
#' @param ... Arguments passed to specific methods. See the method documentation
#' for details.
#'
#' @return A list.
#' @seealso [get_hr.phsfit()], [get_hr.data.frame()]
#' @export
#'
#' @examples
#' # phsfit interface:
#' \dontrun{
#'     get_hr(phsfit)
#' }
#' # Data frame interface:
#' get_hr(test_data, age = "age", status = "status", phs = "phs")
get_hr <- function(object, ...) UseMethod("get_hr")

#' Returns a hazard ratio and 95% CI for Polygenic Hazard Scores
#' 
#' This function calculates the hazard ratio and optionally performs
#' bootstrap resampling to return 95% confidence intervals.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param object a data.frame object containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). The default is "status"
#' @param numerator a vector specifying the quantiles of the numerator (e.g., `c(0.80, 0.98)`). The default is `c(0.8, 1.0)`. 
#' @param denominator a vector specifying the quantiles of the denominator (e.g., `c(0.30, 0.70)`). The default is `c(0.0, 0.2)`. 
#' @param bootstrap.iterations Number of bootstrap iterations to run to generate confidence intervals.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Additional arguments (currently ignored)
#' 
#' @return A numeric hazard ratio or a list containing HR and the 95% confidence intervals from bootstrap
#' 
#' @examples
#' 
#' HR80_20 <- get_hr(test_data, bootstrap.iterations = 300)
#' 
#' @export
#' @rdname get_hr
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
    
    HR = calc_hr(df, numerator, denominator)
    res <- list(value = HR)
    if (!is.null(bootstrap.iterations)) {
        boot_out = boot_conf(df,
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
#' Returns a hazard ratio and 95% CI for Polygenic Hazard Scores
#' 
#' This function calculates the hazard ratio and optionally performs
#' bootstrap resampling to return 95% confidence intervals.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values.
#'
#' @param object a `phsfit` object
#' @param numerator a vector specifying the quantiles of the numerator (e.g., `c(0.80, 0.98)`). The default is `c(0.8, 1.0)`. 
#' @param denominator a vector specifying the quantiles of the denominator (e.g., `c(0.30, 0.70)`). The default is `c(0.0, 0.2)`. 
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Additional arguments (currently ignored)
#' 
#' @return A numeric hazard ratio or a list containing HR and the 95% confidence intervals from bootstrap
#' 
#' @examples
#' 
#' HR80_20 <- get_hr(test_data, bootstrap.iterations = 300)
#' 
#' @export
#' @rdname get_hr
get_hr.phsfit <- function(object,
                          numerator = c(0.80, 1.0),
                          denominator = c(0.0, 0.20),
                          conf.level = 0.95,
                          ...) {
    phsfit <- object
    # phsfit$model is the fitted coxph
    # phsfit$boot contains bootstraps if available
    # derive HR from the saved model and bootstraps
    df <- phsfit$data
    HR <- calc_hr(df, numerator, denominator)
    res <- list(value = HR)
    alpha = (1 - conf.level) / 2
    if (!is.null(phsfit$boot)) {
        # reuse your bootstrapped coxph models to compute HR quickly
        # (or bootstrap indices saved)
        boot_values <- sapply(phsfit$boot, function(m) {
            calc_hr(m$data, numerator, denominator)
        })
        qs <- quantile(boot_values, c(alpha, 1 - alpha))
        boot_out <- list(quantiles = qs)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }

    return(res)
}

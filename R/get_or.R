#' Return odds ratio for Polygenic Hazard Score at a particular age
#'
#' This function calculate the odds ratio at a particular age.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). The default is "status"
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param numerator a vector specifying the quantiles of the numerator (e.g., `c(0.80, 0.98)`). The default is `c(0.8, 1.0)`. 
#' @param denominator a vector specifying the quantiles of the denominator (e.g., `c(0.30, 0.70)`). The default is `c(0.0, 0.2)`. 
#' @param bootstrap.iterations Number of bootstrap iterations to run.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' 
#' @return A numeric odds ratio
#' 
#' @importFrom scales label_percent
#' 
#' @examples
#' 
#' OR80_20 <- get_or(test_data, or_age = 70)
#' 
#' @export
get_or <- function(data = NULL, 
                   phs = "phs", 
                   age = "age", 
                   status = "status", 
                   or_age,
                   numerator = c(0.8, 1.0), 
                   denominator = c(0.0, 0.2),
                   bootstrap.iterations = NULL,
                   conf.level = 0.95) {  
    if (missing(or_age) || is.null(or_age)) {
        stop("Argument 'or_age' is required. Please specify the age at which to compute the OR.")
    }
    for (col in c(phs, age, status)) {
        if (!(col %in% names(data))) stop("Column `", col, "` not found in `data`.")
    }
    
    phs_vec <- if (is.character(phs)) data[[phs]] else phs
    age_vec <- if (is.character(age)) data[[age]] else age
    status_vec <- if (is.character(status)) data[[status]] else status
    
    df <- data.frame(phs = phs_vec, age = age_vec, status = status_vec)
    
    OR = calc_or(df = df, 
                 or_age = or_age, 
                 numerator = numerator,
                 denominator = denominator)
    res <- list(value = OR)
    boot_out = NULL
    if (!is.null(bootstrap.iterations)) {
        boot_out = boot_conf(df,
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
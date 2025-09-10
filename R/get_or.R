#' Return odds ratio for Polygenic Hazard Score at a particular age
#'
#' This function calculate the odds ratio at a particular age.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param numerator a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be `Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.80, 0.98)`). The default is `0.8`. 
#' @param denominator a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be `-Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.30, 0.70)`). The default is `0.2`. 
#' @param conf.int logical. If \code{TRUE} performs bootstrap and returns 95% confidence intervals. Default = \code{FALSE}.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param bootstrap.iterations Number of bootstrap iterations to run. Required if boot = `TRUE`.
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
                   numerator = 0.80, 
                   denominator = 0.20,
                   conf.int = FALSE,
                   conf.level = 0.95,
                   bootstrap.iterations) {  
    if (missing(or_age)) {
        stop("An age must be provided via 'or_age'")
    }
    if (is.character(phs)) {
        phs = data[[phs]]
    }
    if (is.character(age)) {
        age = data[[age]]
    }
    if (is.character(status)) {
        status = data[[status]]
    }
    
    df <- data.frame(age, status, phs)
    
    OR = calc_or(df = df, 
                 or_age = or_age,
                 denominator = denominator, 
                 numerator = numerator)
    boot_out = NULL
    if (conf.int) {
        boot_out = boot_conf(df,
                             bootstrap.iterations,
                             conf.level,
                             calc_or,
                             or_age,
                             denominator,
                             numerator)
    }
    return(list("value" = OR, 
                "conf.low" = boot_out$quantiles[[1]], 
                "conf.high" = boot_out$quantiles[[2]],
                "age" = or_age#,
                #"iters" = boot_out$iters
                )
           )
    
}
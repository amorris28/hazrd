#' Returns a concordance index from a coxph fit
#' 
#' This function returns the concordance index from a coxph fit and optionally
#' performs bootstrap resampling to return 95% confidence intervals.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' @param conf.int logical. if \code{TRUE} performs bootstrap and returns 95% confidence intervals. Default = \code{FALSE}.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param bootstrap.iterations Number of bootstrap iterations to run. Required if boot = `TRUE`. Default = 1000.
#' 
#' @return A numeric hazard ratio or a list containing HR and the 95% confidence intervals from bootstrap
#' 
#' @examples
#' 
#' c_index <- get_cindex(test_data, conf.int = TRUE, bootstrap.iterations = 300)
#' 
#' @export
get_cindex <- function(data = NULL,
                       phs = "phs",
                       age = "age",
                       status = "status",
                       conf.int = FALSE,
                       conf.level = 0.95,
                       bootstrap.iterations) {
    
    if (is.character(phs)) {
        phs = data[[phs]]
    }
    if (is.character(age)) {
        age = data[[age]]
    }
    if (is.character(status)) {
        status = data[[status]]
    }
    
    df <- data.frame(age = age, 
                     status = status, 
                     phs = phs)
    
    c_index = calc_cindex(df)
    boot_out = NULL
    if (conf.int) {
        boot_out = boot_conf(df,
                             bootstrap.iterations,
                             conf.level,
                             calc_cindex)
    }
    return(list("value" = c_index, 
                "conf.low" = boot_out$quantiles[[1]], 
                "conf.high" = boot_out$quantiles[[2]]#,
                #"iters" = boot_out$iters
                )
           )
    
}

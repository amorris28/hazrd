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
#' @param upper_quantile an optional vector specifying the upper quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`). If only one value is provided, then the PHS scores between that number and Infinite are included. The default is `0.80`. 
#' @param lower_quantile an optional vector specifying the lower quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`). If only one value is provided, then the PHS scores between -Infinite and that number are included. The default is `0.20`. 
#' @param boot logical. if \code{TRUE} performs bootstrap and returns 95% confidence intervals. Default = \code{FALSE}.
#' @param B Number of bootstrap iterations to run. Required if boot = `TRUE`. Default = 1000.
#' 
#' @return A numeric odds ratio
#' 
#' @examples
#' 
#' OR80_20 <- get_or(data, or_age = 70)
#' 
#' @export
get_or <- function(data = NULL, 
                   phs = "phs", 
                   age = "age", 
                   status = "status", 
                   or_age,
                   upper_quantile = 0.80, 
                   lower_quantile = 0.20,
                   boot = FALSE,
                   B = 1000) {  
    
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
    
    OR = calc_or(df, 
                 or_age,
                 upper_quantile, 
                 lower_quantile)
    
    if (boot == TRUE) {
        iters = matrix(NA, nrow = B)
        for (b in (1:B)){
            indices = sample(nrow(df), replace = TRUE)
            tmp_df = df[indices, ]
            iters[b] = calc_or(tmp_df, 
                               or_age,
                               upper_quantile, 
                               lower_quantile)
        }
        quantiles = quantile(iters, c(0.025, 0.975))
        return(list("OR" = OR, "lower_CI" = quantiles[[1]], "upper_CI" = quantiles[[2]]))
    }
    return(OR)
    
}
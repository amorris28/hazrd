#' Returns a hazard ratio and 95% CI for Polygenic Hazard Scores
#' 
#' This function calculates the hazard ratio and optionally performs
#' bootstrap resampling to return 95% confidence intervals.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' @param upper_quantile an optional vector specifying the upper quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`). If only one value is provided, then the PHS scores between that number and Infinite are included. The default is `0.80`. 
#' @param lower_quantile an optional vector specifying the lower quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`). If only one value is provided, then the PHS scores between -Infinite and that number are included. The default is `0.20`. 
#' @param swc logical. if `TRUE` performs sample weight correction
#' @param swc_popnumcases an optional integer specifying the number of cases in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @param swc_popnumcontrols an optional integer specifying the number of controls in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @param boot logical. if \code{TRUE} performs bootstrap and returns 95% confidence intervals. Default = \code{FALSE}.
#' @param B Number of bootstrap iterations to run. Required if boot = `TRUE`. Default = 1000.
#' 
#' @return A numeric hazard ratio or a list containing HR and the 95% confidence intervals from bootstrap
#' 
#' @examples
#' 
#' HR80_20 <- get_hr(df, boot = TRUE, B = 300)
#' 
#' @export
get_hr <- function(data = NULL,
                   phs = "phs",
                   age = "age",
                   status = "status",
                   upper_quantile = 0.80,
                   lower_quantile = 0.20,
                   swc = FALSE,
                   swc_popnumcases = NULL,
                   swc_popnumcontrols = NULL,
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
    
    df <- data.frame(age = age, 
                     status = status, 
                     phs = phs)
    
    HR = calc_hr(df, 
                 upper_quantile, 
                 lower_quantile,
                 swc,
                 swc_popnumcases,
                 swc_popnumcontrols)
    
    if (boot == TRUE) {
        quantiles = boot_conf(df,
                              B,
                              calc_hr,
                              upper_quantile,
                              lower_quantile,
                              swc,
                              swc_popnumcases,
                              swc_popnumcontrols)
        return(list("HR" = HR, "conf.low" = quantiles[[1]], "conf.high" = quantiles[[2]]))
    }
    return(HR)
    
}

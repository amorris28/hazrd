#' Returns the concordance index for a dataset using a coxph fit
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param upper_quantile a vector specifying the upper quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`). If only one value is provided, then the PHS scores between that number and Infinite are included. The default is `0.80`. 
#' @param lower_quantile a vector specifying the lower quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`). If only one value is provided, then the PHS scores between -Infinite and that number are included. The default is `0.20`. 
#' @param swc logical. if `TRUE` performs sample weight correction
#' @param swc_popnumcases an optional integer specifying the number of cases in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @param swc_popnumcontrols an optional integer specifying the number of controls in a reference population for sample weight correction. Required if swc = `TRUE`.
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
#' 
#' @examples
#' 
#' c_index <- calc_cindex(df)
#' 
#' @export
calc_cindex = function(df, 
                   swc = FALSE,
                   swc_popnumcases = NULL,
                   swc_popnumcontrols = NULL) {
    if (swc) {
        if (is.null(swc_popnumcases) || is.null(swc_popnumcontrols)) {
            stop("Sample weight correction requires 'swc_popnumcases' and 'swc_popnumcontrols'.")
        }
        swc_numcases <- sum(df$status == 1)
        swc_numcontrols <- sum(df$status == 0)
        
        swc_wvec <- df$status * (swc_popnumcases / swc_numcases) + (!df$status) * (swc_popnumcontrols / swc_numcontrols)
    }
    
    if (swc) {
        cxph <- coxph(Surv(age, status) ~ phs, data = df, weights = swc_wvec)
    } else {
        cxph <- coxph(Surv(age, status) ~ phs, data = df)
    }
    
    c_index = as.numeric(cxph$concordance['concordance'])
    
    return(c_index)
}

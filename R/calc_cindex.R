#' Returns the concordance index for a dataset using a coxph fit
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param swc logical. if `TRUE` performs sample weight correction
#' @param swc_popnumcases an optional integer specifying the number of cases in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @param swc_popnumcontrols an optional integer specifying the number of controls in a reference population for sample weight correction. Required if swc = `TRUE`.
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
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

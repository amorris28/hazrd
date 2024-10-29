#' Calculates the hazard ratio for a dataset using a coxph fit
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
#' @importFrom stats quantile
#' 
#' @export
calc_hr = function(df, 
                   upper_quantile, 
                   lower_quantile,
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
    
    if (length(upper_quantile) == 1) {
        num_critvals <- c(quantile(df$phs, upper_quantile), Inf)
    } else if (length(upper_quantile) == 2) {
        num_critvals <- c(quantile(df$phs, upper_quantile[1]), quantile(df$phs, upper_quantile[2]))
    } else {
        stop("'upper_quantile' must be length 1 or 2")
    }
    
    if (length(lower_quantile) == 1) {
        den_critvals <- c(-Inf, quantile(df$phs, lower_quantile))
    } else if (length(lower_quantile) == 2) {
        den_critvals <- c(quantile(df$phs, lower_quantile[1]), quantile(df$phs, lower_quantile[2]))
    } else {
        stop("'lower_quantile' must be length 1 or 2")
    }
    
    if (swc) {
        cxph <- coxph(Surv(age, status) ~ phs, data = df, weights = swc_wvec)
    } else {
        cxph <- coxph(Surv(age, status) ~ phs, data = df)
    }
    
    beta = as.numeric(cxph$coefficients)
    
    ix_num <- which(df$phs >= num_critvals[1] & df$phs <= num_critvals[2])
    ix_den <- which(df$phs >= den_critvals[1] & df$phs <= den_critvals[2])
    
    beta_phs <- beta * df$phs
    beta_phs_num <- mean(beta_phs[ix_num])
    beta_phs_den <- mean(beta_phs[ix_den])
    HR <- exp(beta_phs_num - beta_phs_den)
    return(HR)
}

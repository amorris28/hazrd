#' Calculates the hazard ratio for a dataset using a coxph fit
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param lower_interval a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be `-Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.30, 0.70)`). The default is `0.2`. 
#' @param upper_interval a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be `Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.80, 0.98)`). The default is `0.80`. 
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
                   lower_interval, 
                   upper_interval,
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
    
    if (length(lower_interval) == 1) {
        den_critvals <- c(-Inf, quantile(df$phs, lower_interval))
    } else if (length(lower_interval) == 2) {
        den_critvals <- c(ifelse(lower_interval[1] == Inf, Inf, quantile(df$phs, lower_interval[1])), 
                          ifelse(lower_interval[2] == Inf, Inf, quantile(df$phs, lower_interval[2])))
    } else {
        stop("'lower_interval' must be length 1 or 2")
    }

        if (length(upper_interval) == 1) {
        num_critvals <- c(quantile(df$phs, upper_interval), Inf)
    } else if (length(upper_interval) == 2) {
        num_critvals <- c(ifelse(upper_interval[1] == Inf, Inf, quantile(df$phs, upper_interval[1])), 
                          ifelse(upper_interval[2] == Inf, Inf, quantile(df$phs, upper_interval[2])))
    } else {
        stop("'upper_interval' must be length 1 or 2")
    }
    
    if (swc) {
        cxph <- coxph(Surv(age, status) ~ phs, data = df, weights = swc_wvec)
    } else {
        cxph <- coxph(Surv(age, status) ~ phs, data = df)
    }
    
    beta = as.numeric(cxph$coefficients)
    
    ix_den <- which(df$phs >= den_critvals[1] & df$phs <= den_critvals[2])
    ix_num <- which(df$phs >= num_critvals[1] & df$phs <= num_critvals[2])
    
    beta_phs <- beta * df$phs
    beta_phs_num <- mean(beta_phs[ix_num])
    beta_phs_den <- mean(beta_phs[ix_den])
    HR <- exp(beta_phs_num - beta_phs_den)
    return(HR)
}

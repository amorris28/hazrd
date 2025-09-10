#' Calculates the hazard ratio for a dataset using a coxph fit
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param numerator a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be `Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.80, 0.98)`). The default is `0.80`. 
#' @param denominator a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be `-Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.30, 0.70)`). The default is `0.2`. 
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_hr = function(df, 
                   numerator, 
                   denominator) {

    
    if (length(denominator) == 1) {
        den_critvals <- c(-Inf, quantile(df$phs, denominator))
    } else if (length(denominator) == 2) {
        den_critvals <- c(ifelse(denominator[1] == Inf, Inf, quantile(df$phs, denominator[1])), 
                          ifelse(denominator[2] == Inf, Inf, quantile(df$phs, denominator[2])))
    } else {
        stop("'denominator' must be length 1 or 2")
    }

        if (length(numerator) == 1) {
        num_critvals <- c(quantile(df$phs, numerator), Inf)
    } else if (length(numerator) == 2) {
        num_critvals <- c(ifelse(numerator[1] == Inf, Inf, quantile(df$phs, numerator[1])), 
                          ifelse(numerator[2] == Inf, Inf, quantile(df$phs, numerator[2])))
    } else {
        stop("'numerator' must be length 1 or 2")
    }

    cxph <- coxph(Surv(age, status) ~ phs, data = df)
    
    beta = as.numeric(cxph$coefficients)
    
    ix_den <- which(df$phs >= den_critvals[1] & df$phs <= den_critvals[2])
    ix_num <- which(df$phs >= num_critvals[1] & df$phs <= num_critvals[2])
    
    beta_phs <- beta * df$phs
    beta_phs_num <- mean(beta_phs[ix_num])
    beta_phs_den <- mean(beta_phs[ix_den])
    exp(beta_phs_num - beta_phs_den)
}

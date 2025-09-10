#' Calculates the odds ratio for a dataset
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param numerator a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be `Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.80, 0.98)`). The default is `0.80`. 
#' @param denominator a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be `-Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.30, 0.70)`). The default is `0.2`. 
#' 
#' @return A numeric odds ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_or = function(df, 
                   or_age, 
                   numerator,
                   denominator) {
    
    if (length(numerator) == 1) {
        lower_critvals <- c(-Inf, quantile(df$phs, numerator))
    } else if (length(numerator) == 2) {
        lower_critvals <- c(ifelse(numerator[1] == Inf, Inf, quantile(df$phs, numerator[1])), 
                            ifelse(numerator[2] == Inf, Inf, quantile(df$phs, numerator[2])))
    } else {
        stop("'numerator' must be length 1 or 2")
    }
    
    if (length(denominator) == 1) {
        upper_critvals <- c(quantile(df$phs, denominator), Inf)
    } else if (length(denominator) == 2) {
        upper_critvals <- c(ifelse(denominator[1] == Inf, Inf, quantile(df$phs, denominator[1])), 
                            ifelse(denominator[2] == Inf, Inf, quantile(df$phs, denominator[2])))
    } else {
        stop("'denominator' must be length 1 or 2")
    }
    
    ix_lower <- which(df$phs >= lower_critvals[1] & df$phs <= lower_critvals[2])
    ix_upper <- which(df$phs >= upper_critvals[1] & df$phs <= upper_critvals[2])
    
    lower_model <- coxph(Surv(age, status) ~ 1, data = df[ix_lower,])
    upper_model <- coxph(Surv(age, status) ~ 1, data = df[ix_upper,])
    
    lower_fit <- survfit(lower_model)
    upper_fit <- survfit(upper_model)
    
    pp1 = approx(lower_fit$time, lower_fit$surv, xout = or_age)$y
    odds1 = pp1 / (1 - pp1)
    pp2 = approx(upper_fit$time, upper_fit$surv, xout = or_age)$y
    odds2 = pp2 / (1 - pp2)
    odds1/odds2 
}

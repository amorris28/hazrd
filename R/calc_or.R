#' Calculates the odds ratio for a dataset
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param lower_interval a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be `-Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.30, 0.70)`). The default is `0.2`. 
#' @param upper_interval a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be `Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.80, 0.98)`). The default is `0.80`. 
#' 
#' @return A numeric odds ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_or = function(df, 
                   or_age, 
                   lower_interval,
                   upper_interval) {
    
    if (length(lower_interval) == 1) {
        lower_critvals <- c(-Inf, quantile(df$phs, lower_interval))
    } else if (length(lower_interval) == 2) {
        lower_critvals <- c(ifelse(lower_interval[1] == Inf, Inf, quantile(df$phs, lower_interval[1])), 
                            ifelse(lower_interval[2] == Inf, Inf, quantile(df$phs, lower_interval[2])))
    } else {
        stop("'lower_interval' must be length 1 or 2")
    }
    
    if (length(upper_interval) == 1) {
        upper_critvals <- c(quantile(df$phs, upper_interval), Inf)
    } else if (length(upper_interval) == 2) {
        upper_critvals <- c(ifelse(upper_interval[1] == Inf, Inf, quantile(df$phs, upper_interval[1])), 
                            ifelse(upper_interval[2] == Inf, Inf, quantile(df$phs, upper_interval[2])))
    } else {
        stop("'upper_interval' must be length 1 or 2")
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
    OR = odds1/odds2
    return(OR)
        
}

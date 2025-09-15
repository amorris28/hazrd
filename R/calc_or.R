#' Calculate a odds ratio between two percentile groups (low-level)
#'
#' This function performs the actual OR calculation used internally by
#' \code{\link{get_or}}. Most users should call \code{get_or()} directly.
#' This function is exported to provide transparency and reproducibility.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param numerator a vector specifying the quantiles of the numerator (e.g., `c(0.80, 0.98)`). 
#' @param denominator a vector specifying the quantiles of the denominator (e.g., `c(0.30, 0.70)`).
#' 
#' @return A numeric odds ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_or = function(df, or_age, numerator, denominator) {
    if (missing(or_age) || is.null(or_age)) {
        stop("Argument 'or_age' is required. Please specify the age at which to compute the OR.")
    }
    num_critvals <- c(quantile(df$phs, numerator[1]), quantile(df$phs, numerator[2]))
    den_critvals <- c(quantile(df$phs, denominator[1]), quantile(df$phs, denominator[2]))
    
    ix_num <- which(df$phs >= num_critvals[1] & df$phs <= num_critvals[2])
    ix_den <- which(df$phs >= den_critvals[1] & df$phs <= den_critvals[2])
    
    num_model <- coxph(Surv(age, status) ~ 1, data = df[ix_num,])
    den_model <- coxph(Surv(age, status) ~ 1, data = df[ix_den,])
    
    num_fit <- survfit(num_model)
    den_fit <- survfit(den_model)
    
    p_num = approx(num_fit$time, num_fit$surv, xout = or_age)$y
    event_odds_num = (1 - p_num) / p_num
    
    p_den = approx(den_fit$time, den_fit$surv, xout = or_age)$y
    event_odds_den = (1 - p_den) / p_den
    
    event_odds_num/event_odds_den
}

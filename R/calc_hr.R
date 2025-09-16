#' Calculate a hazard ratio between two percentile groups (low-level)
#'
#' This function performs the actual HR calculation used internally by
#' \code{\link{get_hr}}. Most users should call \code{get_hr()} directly.
#' This function is exported to provide transparency and reproducibility.
#'
#' @param data a data.frame containing the columns phs, age, and status
#' @param numerator a vector specifying the quantiles of the numerator (e.g., `c(0.80, 0.98)`). 
#' @param denominator a vector specifying the quantiles of the denominator (e.g., `c(0.30, 0.70)`). 
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_hr = function(data, numerator, denominator) {

    num_critvals <- c(quantile(data$phs, numerator[1]), quantile(data$phs, numerator[2]))
    den_critvals <- c(quantile(data$phs, denominator[1]), quantile(data$phs, denominator[2]))
    
    cxph <- coxph(Surv(age, status) ~ phs, data = data)
    
    beta = as.numeric(cxph$coefficients)
    
    ix_num <- which(data$phs >= num_critvals[1] & data$phs <= num_critvals[2])
    ix_den <- which(data$phs >= den_critvals[1] & data$phs <= den_critvals[2])
    
    beta_phs <- beta * data$phs
    beta_phs_num <- mean(beta_phs[ix_num])
    beta_phs_den <- mean(beta_phs[ix_den])
    exp(beta_phs_num - beta_phs_den)
}

#' Calculate a hazard ratio for a one standard deviation increase in PHS (low-level)
#'
#' This function performs the actual HR calculation used internally by
#' \code{\link{get_hrsd}}. Most users should call \code{get_hrsd()} directly.
#' This function is exported to provide transparency and reproducibility.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export

calc_hrsd = function(df) {
    cxph <- coxph(Surv(age, status) ~ phs, data = df)
    as.numeric(exp(coef(cxph)))
}

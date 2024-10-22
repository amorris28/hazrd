#' Return hazard ratio for Polygenic Hazard Score
#'
#' @param phs a vector of polygenic hazard scores for subjects or a string specifying the column name in `data` containing these values
#' @param age a vector of ages or a string specifying the column name in `data` containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation).
#' @param status a vector of case-control status (0 = censored, 1 = event) or a string specifying the column name in `data` containing these values
#' @param data an optional data.frame containing the variables phs, age, and status
#' @param upper_quantile an optional vector specifying the upper quantile of the hazard ratio. The default is `0.80`. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`).
#' @param lower_quantile an optional vector specifying the lower quantile of the hazard ratio. The default is `0.20`. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`).
#' @param swc logical. if `TRUE` performs sample weight correction
#' @param swc_popnumcases an optional integer specifying the number of cases in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @param swc_popnumcontrols an optional integer specifying the number of controls in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @return A numeric hazard ratio
#' @import survival
#' @examples
#' HR80_20 <- get_hr(phs, age, status)
#' @export
get_hr <- function(phs, 
                   age, 
                   status, 
                   data = NULL, 
                   upper_quantile = 0.80, 
                   lower_quantile = 0.20, 
                   swc = FALSE, 
                   swc_popnumcases = NULL, 
                   swc_popnumcontrols = NULL) {  

  if (is.character(phs)) {
    phs = data[[phs]]
  }
  if (is.character(age)) {
    age = data[[age]]
  }
  if (is.character(status)) {
    status = data[[status]]
  }

  if (swc) {
    if (is.null(swc_popnumcases) || is.null(swc_popnumcontrols)) {
     stop("Sample weight correction requires 'swc_popnumcases' and 'swc_popnumcontrols'.")
    }
    swc_numcases <- sum(status == 1)
    swc_numcontrols <- sum(status == 0)
  
    swc_wvec <- status * (swc_popnumcases / swc_numcases) + (!status) * (swc_popnumcontrols / swc_numcontrols)
  }

  if (length(upper_quantile) == 1) {
    num_critvals <- c(quantile(phs, upper_quantile), Inf)
  } else if (length(upper_quantile) == 2) {
    num_critvals <- c(quantile(phs, upper_quantile[1]), quantile(phs, upper_quantile[2]))
  } else {
    stop("'upper_quantile' must be length 1 or 2")
  }

  if (length(lower_quantile) == 1) {
    den_critvals <- c(-Inf, quantile(phs, lower_quantile))
  } else if (length(lower_quantile) == 2) {
    den_critvals <- c(quantile(phs, lower_quantile[1]), quantile(phs, lower_quantile[2]))
  } else {
    stop("'lower_quantile' must be length 1 or 2")
  }

  tmp_df <- data.frame(age = age, status = status, phs = phs)
  
  if (swc) {
    cxph <- coxph(Surv(age, status) ~ phs, data = tmp_df, weights = swc_wvec)
  } else {
    cxph <- coxph(Surv(age, status) ~ phs, data = tmp_df)
  }
  
  beta = as.numeric(cxph$coefficients)
  
  ix_num <- which(phs >= num_critvals[1] & phs <= num_critvals[2])
  ix_den <- which(phs >= den_critvals[1] & phs <= den_critvals[2])
    
  beta_phs <- beta * phs
  beta_phs_num <- mean(beta_phs[ix_num])
  beta_phs_den <- mean(beta_phs[ix_den])
  HR <- exp(beta_phs_num - beta_phs_den)
  
  return(HR)
}
#' Return hazard ratio for Polygenic Hazard Score
#'
#' @param phs a vector of polygenic hazard scores for subjects or a string specifying the column name in `data` containing these values
#' @param age a vector of age of either an event (e.g., diagnosis) in cases or of censoring (e.g., last observation) in controls or a string specifying the column name in `data` containing these values
#' @param status a vector of case-control status (0 = censored, 1 = event) or a string specifying the column name in `data` containing these values
#' @param data an optional data.frame containing the variables phs, age, and status
#' @param upper_quantile an optional vector specifying the upper quantile of the hazard ratio. The default is `0.80`.
#' @param lower_quantile an optional vector specifying the lower quantile of the hazard ratio. The default is `0.20`.
#' @param swc logical. if `TRUE` performs sample weight correction
#' @param swc_popnumcases an optional integer specifying the number of cases in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @param swc_popnumcontrols an optional integer specifying the number of controls in a reference population for sample weight correction. Required if swc = `TRUE`.
#' @return A numeric hazard ratio
#' @import survival
#' @examples
#' HR80_20 <- get_hr(phs, age, status)
#' @export
get_hr <- function(phs, age, status, data = NULL, upper_quantile = 0.80, lower_quantile = 0.20, swc = FALSE, swc_popnumcases = NULL, swc_popnumcontrols = NULL) {  


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

  num_critvals <- c(quantile(phs, upper_quantile), Inf)
  den_critvals <- c(-Inf, quantile(phs, lower_quantile))
  
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
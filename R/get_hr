#' Return hazard ratio for Polygenic Hazard Score
#'
#' @param phs Polygenic hazard score vector
#' @param age Age vector
#' @param status Status vector (0 = censored, 1 = event)
#' @param upper_quantile Status vector (0 = censored, 1 = event)
#' @param lower_quantile Status vector (0 = censored, 1 = event)
#' @param swc Boolean to enable sample weight correction
#' @param swc_popnumcases Number of cases in reference population for sample weight correction. Required if swc = `TRUE`.
#' @param swc_popnumcontrols Number of controls in reference population for sample weight correction. Required if swc = `TRUE`.
#' @return A numeric hazard ratio
#' @import survival
#' @examples
#' HR80_20 <- get_hr(phs, age, status)
#' @export
get_hr <- function(phs, age, status, upper_quantile = 0.80, lower_quantile = 0.20, swc = FALSE, swc_popnumcases = NULL, swc_popnumcontrols = NULL) {  

  if (swc) {
    if (is.null(swc_popnumcases) || is.null(swc_popnumcontrols)) {
     stop("Sample weight correction requires 'swc_popnumcases' and 'swc_popnumcontrols'.")
    }
  }

  num_critvals <- c(quantile(phs, upper_quantile), Inf)
  den_critvals <- c(-Inf, quantile(phs, lower_quantile))
  
  swc_numcases <- sum(status == 1)
  swc_numcontrols <- sum(status == 0)
  
  swc_wvec <- status * (swc_popnumcases / swc_numcases) + (!status) * (swc_popnumcontrols / swc_numcontrols)
  
  tmp_df <- data.frame(age = age, status = status, phs = phs, wvec = swc_wvec)
  
  if (swc_switch == TRUE) {
    cxph <- coxph(Surv(Age, status) ~ phs, data = tmp_df, weights = swc_wvec)
  } else {
    cxph <- coxph(Surv(Age, status) ~ phs, data = tmp_df)
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
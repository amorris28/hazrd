#' Performance Metrics for Polygenic Hazard Score
#'
#' @param lp Linear predictor
#' @param Age Age vector
#' @param status Status vector (0 = censored, 1 = event)
#' @param ref Reference values (optional)
#' @param swc_switch Boolean to enable weighting
#' @param swc_caco Vector indicating case control status (optional)
#' @return A list of performance metrics
#' @examples
#' perf_metrics <- RK_get_perf(lp, Age, status)
#' @export
RK_get_perf <- function(lp, Age, status, ref = NULL, swc_switch = TRUE, swc_caco = NULL) {
  if (is.null(swc_caco)) {
    swc_caco <- status
  }
  
  hr_names <- c('HR98_50', 'HR20_50', 'HR80_20', 'HR95_50')
  
  if (is.null(ref)) {
    num_critvals <- matrix(c(10.097845, Inf, -Inf, 9.004659, 9.639069, Inf, 9.946332, Inf), ncol = 2, byrow = TRUE)
    den_critvals <- matrix(c(9.123500, 9.519703, 9.123500, 9.519703, -Inf, 9.004659, 9.123500, 9.519703), ncol = 2, byrow = TRUE)
  } else {
    num_critvals <- matrix(c(quantile(ref, 0.98), Inf, -Inf, quantile(ref, 0.2), quantile(ref, 0.8), Inf, quantile(ref, 0.95), Inf), ncol = 2, byrow = TRUE)
    den_critvals <- matrix(c(quantile(ref, 0.3), quantile(ref, 0.7), quantile(ref, 0.3), quantile(ref, 0.7), -Inf, quantile(ref, 0.2), quantile(ref, 0.3), quantile(ref, 0.7)), ncol = 2, byrow = TRUE)
  }
  
  require(survival)
  
  swc_numcases <- sum(swc_caco == 1)
  swc_numcontrols <- sum(swc_caco == 0)
  
  swc_popnumcases <- 9024
  swc_popnumcontrols <- 1953203
  swc_wvec <- swc_caco * (swc_popnumcases / swc_numcases) + (!swc_caco) * (swc_popnumcontrols / swc_numcontrols)
  
  tmp_df <- data.frame(Age = Age, status = status, lp = lp, wvec = swc_wvec)
  
  if (swc_switch == TRUE) {
    cxph <- coxph(Surv(Age, status) ~ lp, data = tmp_df, weights = swc_wvec)
  } else {
    cxph <- coxph(Surv(Age, status) ~ lp, data = tmp_df)
  }
  
  perf_list <- list(beta = as.numeric(cxph$coefficients), p = as.numeric(summary(cxph)$coefficients[length(summary(cxph)$coefficients)]), cindex = as.numeric(cxph$concordance['concordance']), swc = as.logical(swc_switch))
  
  hr_list <- list()
  
  for (id1 in 1:nrow(num_critvals)) {
    ix_num <- which(lp >= num_critvals[id1, 1] & lp <= num_critvals[id1, 2])
    ix_den <- which(lp >= den_critvals[id1, 1] & lp <= den_critvals[id1, 2])
    
    beta_lp <- perf_list$beta * lp
    beta_lp_num <- mean(beta_lp[ix_num])
    beta_lp_den <- mean(beta_lp[ix_den])
    HR <- exp(beta_lp_num - beta_lp_den)
    hr_list[[id1]] <- HR
  }
  
  names(hr_list) <- hr_names
  perf_list <- c(perf_list, hr_list)
  
  return(perf_list)
}


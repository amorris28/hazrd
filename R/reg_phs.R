#' Regularized Polygenic Hazard Score
#'
#' @param Age Vector of ages
#' @param status Vector indicating event status (0 = censored, 1 = event)
#' @param Xvars Matrix of predictors
#' @return Vector of coefficients from the lasso-regularized Cox regression model
#' @examples
#' beta <- RK_get_regPHS(Age, status, Xvars)
#' @export
RK_get_regPHS <- function(Age, status, Xvars) {
  require(survival)
  require(glmnet)
  
  Xvars <- as.matrix(Xvars)
  
  cv_fit <- cv.glm(Xvars, Surv(Age, status), family = 'cox', maxit = 1000)
  beta <- coef(cv_fit, s = cv_fit$lambda.min)
  
  return(beta)
}


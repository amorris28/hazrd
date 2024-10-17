#' Polygenic Hazard Score Age Calculation
#'
#' @param age Vector of ages
#' @param ref Reference scores
#' @param riskgroup Vector of risk group scores
#' @param estimate_age Age to estimate
#' @return Estimated polygenic hazard score age
#' @examples
#' phs_age <- RK_get_phsAge(age, ref, riskgroup, estimate_age)
#' @export
RK_get_phsAge <- function(age, ref, riskgroup, estimate_age) {
  pfit_ref <- approx(age, ref, estimate_age)$y
  phsAge <- approx(riskgroup, age, pfit_ref)$y
  return(phsAge)
}


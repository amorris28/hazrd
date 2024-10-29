#' Polygenic Hazard Score Age Calculation
#'
#' @param age Vector of ages
#' @param ref Reference scores
#' @param riskgroup Vector of risk group scores
#' @param estimate_age Age to estimate
#' @return Estimated polygenic hazard score age
#' @importFrom stats approx
#' @examples
#' \dontrun{
#' phs_age <- get_phs_age(age, ref, riskgroup, estimate_age)
#' }
#' @export
get_phs_age <- function(age, ref, riskgroup, estimate_age) {
  pfit_ref <- approx(age, ref, estimate_age)$y
  phsAge <- approx(riskgroup, age, pfit_ref)$y
  return(phsAge)
}


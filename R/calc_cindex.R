#' Calculate a C-index for a PHS model (low-level)
#'
#' This function performs the actual C-index calculation used internally by
#' \code{\link{get_cindex}}. Most users should call \code{get_cindex()} directly.
#' This function is exported to provide transparency and reproducibility.
#'
#' @param data a data.frame containing the columns phs, age, and status
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
#' 
#' @export
calc_cindex = function(data) {
    cxph <- coxph(Surv(age, status) ~ phs, data = data)
    as.numeric(cxph$concordance['concordance'])
}

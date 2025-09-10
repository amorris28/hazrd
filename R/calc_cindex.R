#' Returns the concordance index for a dataset using a coxph fit
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' 
#' @return A numeric hazard ratio
#' 
#' @import survival
#' 
#' @export
calc_cindex = function(df) {
    cxph <- coxph(Surv(age, status) ~ phs, data = df)
    as.numeric(cxph$concordance['concordance'])
}

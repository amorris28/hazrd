#' Return Kaplan-Meier curve
#'
#' This function returns a single Kaplan-Meier curve for plotting.
#' Output includes age, the K-M estimate at each age, and the upper and lower confidence intervals
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' @param interval a vector of length two specifying the lower  and upper quantiles of the interval. The default is `c(-Inf, Inf)`.
#' @param age_range a vector of ages over which curves should be calculated. Default = 40:100
#' @param scale logical. if `TRUE` centers and scales the PHS scores to unit variance. Default = `FALSE`.
#' @param inverse logical. if `TRUE` calculates the inverse (x * -1) the PHS scores to reverse the direction of effect. Default = `FALSE`.
#' @importFrom tidyr pivot_longer
#' @importFrom survival coxph Surv survfit
#' @importFrom stats quantile
#' @import dplyr
#' @import tibble
#' @return A data.frame containing ages, the K-M curve, and the upper and lower confidence intervals
#' @examples
#' 
#' km_curve <- km_curve(test_data)
#' \dontrun{
#' ggplot(km_curve, aes(x = time,
#'                      y = estimate,
#'                      ymin = conf.low,
#'                      ymax = conf.high)) +
#'                      geom_ribbon() +
#'                      geom_step()
#' }
#' @export
km_curve <- function(data = NULL,
                     phs = "phs",
                     age = "age",
                     status = "status",
                     interval = c(0, 1),
                     age_range = 40:100,
                     scale = FALSE,
                     inverse = FALSE) {

    scale <- as.logical(scale)
    inverse <- as.logical(inverse)
    
    if (is.character(phs)) {
        phs = data[[phs]]
    }
    if (is.character(age)) {
        age = data[[age]]
    }
    if (is.character(status)) {
        status = data[[status]]
    }
    
    if (scale) { phs <- scale(phs, center = TRUE, scale = TRUE) }
    if (inverse) { phs <- phs * -1 }
    
    critvals <- c(quantile(phs, interval[1]), quantile(phs, interval[2]))
    
    ix <- which(phs >= critvals[1] & phs <= critvals[2])
    
    tmp_df <- data.frame(age, status, phs)
    
    cox_model <- coxph(Surv(age, status) ~ phs, data = tmp_df)
    
    bt <- cox_model$coefficients[["phs"]]
    
    quantile_data <- tmp_df[ix, ]
    
    # Plot K-M curves for centiles
    mod <- survfit(Surv(age, status) ~ 1, data = quantile_data)
    
    mod_data <- data.frame(time = mod$time,
                           n.risk = mod$n.risk,
                           n.event = mod$n.event,
                           n.censor = mod$n.censor,
                           estimate = mod$surv,
                           std.error = mod$std.err,
                           conf.high = mod$upper,
                           conf.low = mod$lower,
                           cumhaz = mod$cumhaz)
    
    kmcurve = select(mod_data, .data$time, .data$estimate, .data$conf.low, 
                     .data$conf.high, .data$cumhaz)
    return(kmcurve)
    
}


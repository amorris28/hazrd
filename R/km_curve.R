#' Return Kaplan-Meier (K-M) curves from a PHS model
#'
#' `km_curve()` is a generic function that calculates Kaplan-Meier survival curves
#' from a coxph fit or directly from a data.frame. It returns a data.frame with
#' time, survival estimates, and upper/lower confidence intervals.
#'
#' @param object An object for which a KM curve is desired. For method details, 
#'   see the specific method.
#' @param phs String specifying the column name in `data` containing the polygenic
#'   hazard score. Default `"phs"`.
#' @param age String specifying the column name in `data` containing the age of
#'   each subject. Default `"age"`.
#' @param status String specifying the column name in `data` containing case/control
#'   status (0 = censored, 1 = event). Default `"status"`.
#' @param interval Vector of length 2 giving lower and upper quantiles of PHS to 
#'   define the interval. By default includes all PHSes: `c(0, 1)`.
#' @param age_range Vector of ages over which curves should be calculated. Default `40:100`.
#' @param scale Logical; if TRUE, centers and scales PHS to unit variance. Default FALSE.
#' @param inverse Logical; if TRUE, reverses PHS direction. Default FALSE.
#' @param ... Additional arguments (currently ignored)
#'
#' @return A data.frame with columns `time`, `estimate`, `conf.low`, `conf.high`, and `cumhaz`.
#'
#' @examples
#' phsfit <- fit_phs(test_data)
#' km_df <- km_curve(phsfit)
#' 
#' # data.frame interface
#' km_df2 <- km_curve(test_data)
#' 
#' \dontrun{
#' library(ggplot2)
#' ggplot(km_df, aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high)) +
#'   geom_ribbon() +
#'   geom_step()
#' }
#'
#' @export
km_curve <- function(object, ...) UseMethod("km_curve")

#' @rdname km_curve
#' @export
km_curve.data.frame <- function(object,
                                phs = "phs",
                                age = "age",
                                status = "status",
                                interval = c(0, 1),
                                age_range = 40:100,
                                scale = FALSE,
                                inverse = FALSE,
                                ...) {
    data <- object
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
    
    
    data.frame(time = mod$time,
               # n.risk = mod$n.risk,
               # n.event = mod$n.event,
               # n.censor = mod$n.censor,
               estimate = mod$surv,
               # std.error = mod$std.err,
               conf.low = mod$lower,
               conf.high = mod$upper,
               cumhaz = mod$cumhaz)
    
}

#' @rdname km_curve
#' @export
km_curve.phsfit <- function(object,
                            phs = "phs",
                            age = "age",
                            status = "status",
                            interval = c(0, 1),
                            age_range = 40:100,
                            scale = FALSE,
                            inverse = FALSE,
                            ...) {
    phsfit <- object
    data <- phsfit$data
    km_curve.data.frame(data,
                        phs,
                        age,
                        status,
                        interval,
                        age_range,
                        scale,
                        inverse)
    
}


#' Calculates the 95% confidence interval using boostrapping
#' 
#' Internal function. Not intended for users. This function can return the 95%
#' confidence intervals for any statistic so long as the function passed to `f`
#' returns a single value of that statistic.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param B number of bootstrap iterations to run.
#' @param f the name of a function to perform bootstrapping on. 
#' @param ... arguments to be passed on to `f()`.
#' 
#' @return A list of confidence intervals
#' 
#' @examples
#' 
#' quantiles <- boot_conf(df, B, calc_or, or_age, upper_quantile, lower_quantile)
#' 
#' @export
boot_conf = function(df, 
                     B,
                     f,
                     ...) {
    # f = match.fun(function_to_bootstrap)
    iters = matrix(NA, nrow = B)
    for (b in (1:B)){
        indices = sample(nrow(df), replace = TRUE)
        tmp_df = df[indices, ]
        iters[b] = f(tmp_df, ...)
    }
    quantiles = quantile(iters, c(0.025, 0.975))
    return(quantiles)
}

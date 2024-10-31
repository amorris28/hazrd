#' Calculates the 95% confidence interval using boostrapping
#' 
#' Internal function. Not intended for users. This function can return the 95%
#' confidence intervals for any statistic so long as the function passed to `f`
#' returns a single value of that statistic.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param bootstrap_iterations number of bootstrap iterations to run.
#' @param f the name of a function to perform bootstrapping on. 
#' @param ... arguments to be passed on to `f()`.
#' 
#' @importFrom stats quantile
#' 
#' @return A list of confidence intervals
#' 
#' @export
boot_conf = function(df, 
                     bootstrap_iterations,
                     f,
                     ...) {
    # f = match.fun(function_to_bootstrap)
    iters = matrix(NA, nrow = bootstrap_iterations)
    for (b in (1:bootstrap_iterations)){
        indices = sample(nrow(df), replace = TRUE)
        tmp_df = df[indices, ]
        iters[b] = f(tmp_df, ...)
    }
    quantiles = quantile(iters, c(0.025, 0.975))
    return(quantiles)
}

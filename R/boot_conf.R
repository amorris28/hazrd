#' Bootstrap confidence intervals for a custom statistic
#'
#' This function allows advanced users to compute bootstrap confidence
#' intervals for any statistic so long as the function passed to `f`
#' returns a single value of that statistic. For most users, the built-in
#' functions such as \code{\link{get_hr}} are recommended.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' @param bootstrap.iterations number of bootstrap iterations to run.
#' @param conf.level The confidence level to use for the confidence interval. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval. 
#' @param f the name of a function to perform bootstrapping on. 
#' @param ... arguments to be passed on to `f()`.
#' 
#' @importFrom stats quantile
#' 
#' @return A list of confidence intervals
#' 
#' @export
boot_conf = function(df, 
                     bootstrap.iterations,
                     conf.level = 0.95,
                     f,
                     ...) {

    iters = numeric(bootstrap.iterations)
    for (b in seq_len(bootstrap.iterations)){
        tmp_df = df[sample(nrow(df), replace = TRUE), ]
        iters[b] = f(tmp_df, ...)
    }
    
    alpha = (1 - conf.level) / 2
    list(quantiles = quantile(iters, probs = c(alpha, 1 - alpha)), iters = iters)
}

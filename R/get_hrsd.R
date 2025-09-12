#' Returns a hazard ratio and 95% CI for Polygenic Hazard Scores
#' 
#' This function calculates the hazard ratio for a 1 standard deviation increase
#' in PHS and optionally performs
#' bootstrap resampling to return 95% confidence intervals.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject . The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). The default is "status"
#' @param conf.int logical. if \code{TRUE} performs bootstrap and returns 95% confidence intervals. Default = \code{FALSE}.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param bootstrap.iterations Number of bootstrap iterations to run. Required if boot = `TRUE`. Default = 1000.
#' 
#' @return A numeric hazard ratio or a list containing HR and the 95% confidence intervals from bootstrap
#' 
#' @examples
#' 
#' HR80_20 <- get_hr(test_data, conf.int = TRUE, bootstrap.iterations = 300)
#' 
#' @export
get_hrsd <- function(data = NULL,
                   phs = "phs",
                   age = "age",
                   status = "status",
                   conf.int = FALSE,
                   conf.level = 0.95,
                   bootstrap.iterations) {
    
    for (col in c(phs, age, status)) {
        if (!(col %in% names(data))) stop("Column `", col, "` not found in `data`.")
    }
    
    phs_vec <- if (is.character(phs)) data[[phs]] else phs
    age_vec <- if (is.character(age)) data[[age]] else age
    status_vec <- if (is.character(status)) data[[status]] else status
    
    df <- data.frame(phs = phs_vec, age = age_vec, status = status_vec)
    
    HR = calc_hrsd(df)
    boot_out = NULL
    if (conf.int) {
        boot_out = boot_conf(df,
                             bootstrap.iterations,
                             conf.level,
                             calc_hrsd)
    }
    res <- list(value = HR)
    if (conf.int) {
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    return(res)
}

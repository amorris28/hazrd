#' Returns a concordance index from a coxph fit
#' 
#' This function returns the concordance index from a coxph fit and optionally
#' performs bootstrap resampling to return 95% confidence intervals.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). The default is "status"
#' @param bootstrap.iterations Number of bootstrap iterations to run.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' 
#' @return A numeric hazard ratio or a list containing HR and the 95% confidence intervals from bootstrap
#' 
#' @examples
#' 
#' c_index <- get_cindex(test_data, bootstrap.iterations = 300)
#' 
#' @export
get_cindex <- function(data = NULL,
                       phs = "phs",
                       age = "age",
                       status = "status",
                       bootstrap.iterations = NULL,
                       conf.level = 0.95) {
    
    for (col in c(phs, age, status)) {
        if (!(col %in% names(data))) stop("Column `", col, "` not found in `data`.")
    }
    
    phs_vec <- if (is.character(phs)) data[[phs]] else phs
    age_vec <- if (is.character(age)) data[[age]] else age
    status_vec <- if (is.character(status)) data[[status]] else status
    
    df <- data.frame(phs = phs_vec, age = age_vec, status = status_vec)
    
    c_index = calc_cindex(df)
    res <- list(value = c_index)
    boot_out = NULL
    if (!is.null(bootstrap.iterations)) {
        boot_out = boot_conf(df,
                             bootstrap.iterations,
                             conf.level,
                             calc_cindex)
        res$conf.low <- boot_out$quantiles[[1]]
        res$conf.high <- boot_out$quantiles[[2]]
    }
    return(res)
}

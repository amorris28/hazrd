#' Creates a lookup table for individual prediction of risk based on PHS
#'
#' This function takes a data set of phs, age, and status and returns two
#' matrices: one is the PHS score at each of 99 percentiles (1-99\%) and the other
#' is predicted K-M curve (plus confidence intervals) for each of these
#' percentiles. These two tables contain now individualized subject information
#' and can be exported to predict risk for new subjects for which you have PHS
#' scores.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' 
#' @import survival
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr full_join
#' 
#' @return A list containing two tables
#' 
#' @examples
#' 
#' lookup_table <- create_lookup_table(test_data)
#' 
#' @export
create_lookup_table <- function(data = NULL, 
                   phs = "phs", 
                   age = "age", 
                   status = "status") {  
    if (is.character(phs)) {
        phs = data[[phs]]
    }
    if (is.character(age)) {
        age = data[[age]]
    }
    if (is.character(status)) {
        status = data[[status]]
    }
    
    df <- data.frame(age, status, phs)

    # fit the model
    cxph <- coxph(Surv(age, status) ~ phs, data = df, model = TRUE)
    cxph_surv = survfit(cxph)
    original_model = data.frame(time = cxph_surv$time,
                                surv = cxph_surv$surv,
                                lower = cxph_surv$lower,
                                upper = cxph_surv$upper)
    
    # fit the prediction for each percentile
    probs = 1:99/100
    percentiles = quantile(df$phs, probs)
    phs_percentiles = data.frame(percentile = probs,
                                 phs = percentiles)
    rownames(phs_percentiles) = NULL
    est <- survfit(cxph, newdata = phs_percentiles)
    
    pred_model = data.frame(time = est$time,
                            est$surv)
    pred_upper = data.frame(time = est$time,
                            est$upper)
    pred_lower = data.frame(time = est$time,
                            est$lower)
    pred_model_long = tidyr::pivot_longer(pred_model, cols = X1:X99, values_to = "surv")
    pred_upper_long = tidyr::pivot_longer(pred_upper, cols = X1:X99, values_to = "upper")
    pred_lower_long = tidyr::pivot_longer(pred_lower, cols = X1:X99, values_to = "lower")
    pred_model_long = dplyr::full_join(pred_model_long, pred_upper_long, by = c("time", "name"))
    pred_model_long = dplyr::full_join(pred_model_long, pred_lower_long, by = c("time", "name"))
    
    pred_model_long = data.frame(pred_model_long,
                               percentile = phs_percentiles$percentile,
    				phs = phs_percentiles$phs)
    lookup_table = subset(pred_model_long, select=-name)

    return(lookup_table)
}

utils::globalVariables(c("X1", "X99", "name"))

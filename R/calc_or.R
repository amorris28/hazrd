#' Calculates the odds ratio for a dataset
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param lower_interval a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be `-Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.30, 0.70)`). The default is `0.2`. 
#' @param upper_interval a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be `Inf`. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., `c(0.80, 0.98)`). The default is `0.80`. 
#' 
#' @return A numeric odds ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_or = function(df, 
                   or_age, 
                   lower_interval,
                   upper_interval) {
    
    if (length(lower_interval) == 1) {
        lower_critvals <- c(-Inf, quantile(df$phs, lower_interval))
    } else if (length(lower_interval) == 2) {
        lower_critvals <- c(quantile(df$phs, lower_interval[1]), quantile(df$phs, lower_interval[2]))
    } else {
        stop("'lower_interval' must be length 1 or 2")
    }
    
    if (length(upper_interval) == 1) {
        upper_critvals <- c(quantile(df$phs, upper_interval), Inf)
    } else if (length(upper_interval) == 2) {
        upper_critvals <- c(quantile(df$phs, upper_interval[1]), quantile(df$phs, upper_interval[2]))
    } else {
        stop("'upper_interval' must be length 1 or 2")
    }
    
    ix_lower <- which(df$phs >= lower_critvals[1] & df$phs <= lower_critvals[2])
    ix_upper <- which(df$phs >= upper_critvals[1] & df$phs <= upper_critvals[2])
    
    df %>% 
        # Filter samples within the intervals of interest
        filter(row_number() %in% c(ix_lower, ix_upper)) %>%
        mutate(interval = ifelse(row_number() %in% ix_lower, "lower", "upper")) %>% 
        # Remove samples that were censored before the OR age
        filter(!(.data$status == 0 & .data$age < or_age)) %>% 
        # Re-categorize diagnoses after OR age as controls
        mutate(status = if_else(.data$status == 1 & .data$age > or_age, 0, .data$status)) %>% 
        # Categorize individuals into a contingency table
        mutate(upper_event = ifelse(.data$status == 1 & .data$interval == "upper", 1, 0),
               upper_censor = ifelse(.data$status == 0 & .data$interval == "upper", 1, 0),
               lower_event = ifelse(.data$status == 1 & .data$interval == "lower", 1, 0),
               lower_censor = ifelse(.data$status == 0 & .data$interval == "lower", 1, 0)) %>%
        select(.data$upper_event, .data$upper_censor, .data$lower_event, .data$lower_censor) %>%
        summarize(upper_event = sum(.data$upper_event),
                  upper_censor = sum(.data$upper_censor),
                  lower_event = sum(.data$lower_event),
                  lower_censor = sum(.data$lower_censor)) -> contingency_table
    
    OR = with(contingency_table, (upper_event / upper_censor) / (lower_event / lower_censor))
    return(OR)
}

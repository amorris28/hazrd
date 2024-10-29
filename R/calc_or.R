#' Calculates the odds ratio for a dataset
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the columns phs, age, and status
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param upper_quantile a vector specifying the upper quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`). If only one value is provided, then the PHS scores between that number and Infinite are included. The default is `0.80`. 
#' @param lower_quantile a vector specifying the lower quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`). If only one value is provided, then the PHS scores between -Infinite and that number are included. The default is `0.20`. 
#' 
#' @return A numeric odds ratio
#' 
#' @import survival
#' @importFrom stats quantile
#' 
#' @export
calc_or = function(df, 
                   or_age,
                   upper_quantile, 
                   lower_quantile) {
    if (length(upper_quantile) == 1) {
        num_critvals <- c(quantile(df$phs, upper_quantile), Inf)
    } else if (length(upper_quantile) == 2) {
        num_critvals <- c(quantile(df$phs, upper_quantile[1]), quantile(df$phs, upper_quantile[2]))
    } else {
        stop("'upper_quantile' must be length 1 or 2")
    }
    
    if (length(lower_quantile) == 1) {
        den_critvals <- c(-Inf, quantile(df$phs, lower_quantile))
    } else if (length(lower_quantile) == 2) {
        den_critvals <- c(quantile(df$phs, lower_quantile[1]), quantile(df$phs, lower_quantile[2]))
    } else {
        stop("'lower_quantile' must be length 1 or 2")
    }
    
    
    ix_num <- which(df$phs >= num_critvals[1] & df$phs <= num_critvals[2])
    ix_den <- which(df$phs >= den_critvals[1] & df$phs <= den_critvals[2])
    
    df %>% 
        filter(row_number() %in% c(ix_num, ix_den)) %>%
        mutate(quantile = ifelse(row_number() %in% ix_num, "upper", "lower")) %>% 
        filter((.data$status == 0 & .data$age < or_age) | (.data$status == 1 & .data$age > or_age)) %>% 
        mutate(uq_event = ifelse(.data$status == 1 & .data$quantile == "upper", 1, 0),
               uq_censor = ifelse(.data$status == 0 & .data$quantile == "upper", 1, 0),
               lq_event = ifelse(.data$status == 1 & .data$quantile == "lower", 1, 0),
               lq_censor = ifelse(.data$status == 0 & .data$quantile == "lower", 1, 0)) %>%
        select(.data$uq_event, .data$uq_censor, .data$lq_event, .data$lq_censor) %>%
        summarize(uq_event = sum(.data$uq_event),
                  uq_censor = sum(.data$uq_censor),
                  lq_event = sum(.data$lq_event),
                  lq_censor = sum(.data$lq_censor)) -> contingency_table
    
    OR = with(contingency_table, (uq_event / uq_censor) / (lq_event / lq_censor))
    return(OR)
}

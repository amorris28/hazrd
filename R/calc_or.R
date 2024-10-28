#' Calculates the odds ratio for a dataset
#' 
#' Internal function. Not intended for users.
#'
#' @param df a data.frame containing the clumnes phs, age, and status
#' @param upper_quantile a vector specifying the upper quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`). If only one value is provided, then the PHS scores between that number and Infinite are included. The default is `0.80`. 
#' @param lower_quantile a vector specifying the lower quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`). If only one value is provided, then the PHS scores between -Infinite and that number are included. The default is `0.20`. 
#' 
#' @return A numeric odds ratio
#' 
#' @import survival
#' 
#' @examples
#' 
#' HR80_20 <- calc_or(df)
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
        filter((status == 0 & age < or_age) | (status == 1 & age > or_age)) %>% 
        mutate(uq_event = ifelse(status == 1 & quantile == "upper", 1, 0),
               uq_censor = ifelse(status == 0 & quantile == "upper", 1, 0),
               lq_event = ifelse(status == 1 & quantile == "lower", 1, 0),
               lq_censor = ifelse(status == 0 & quantile == "lower", 1, 0)) %>%
        select(uq_event, uq_censor, lq_event, lq_censor) %>%
        summarize(uq_event = sum(uq_event),
                  uq_censor = sum(uq_censor),
                  lq_event = sum(lq_event),
                  lq_censor = sum(lq_censor)) -> contingency_table
    
    OR = with(contingency_table, (uq_event / uq_censor) / (lq_event / lq_censor))
    return(OR)
}

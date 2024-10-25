#' Return odds ratio for Polygenic Hazard Score at a particular age
#'
#' This function calculate the odds ratio at a particular age.
#' The data can either be provided as a data.frame with columns containing
#' the phs, age, and status of each individual or separate vectors containing
#' each of these values. The columns in `data` default to 'phs', 'age', and
#' 'status', but any arbitrary column names can be used if named explicitly.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' @param or_age an integer specifying the age at which the odds ratio should be calculated
#' @param upper_quantile an optional vector specifying the upper quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.80, 0.98)`). If only one value is provided, then the PHS scores between that number and Infinite are included. The default is `0.80`. 
#' @param lower_quantile an optional vector specifying the lower quantile of the hazard ratio. Can also be supplied as a vector of length 2 to specify both the upper and lower limits of the quantile (e.g., `c(0.3, 0.7)`). If only one value is provided, then the PHS scores between -Infinite and that number are included. The default is `0.20`. 
#' @return A numeric odds ratio
#' @examples
#' OR80_20 <- get_hr(data, or_age = 70)
#' @export
get_or <- function(data = NULL, 
                   phs = "phs", 
                   age = "age", 
                   status = "status", 
                   or_age,
                   upper_quantile = 0.80, 
                   lower_quantile = 0.20) {  

  if (is.character(phs)) {
    phs = data[[phs]]
  }
  if (is.character(age)) {
    age = data[[age]]
  }
  if (is.character(status)) {
    status = data[[status]]
  }

  if (length(upper_quantile) == 1) {
    num_critvals <- c(quantile(phs, upper_quantile), Inf)
  } else if (length(upper_quantile) == 2) {
    num_critvals <- c(quantile(phs, upper_quantile[1]), quantile(phs, upper_quantile[2]))
  } else {
    stop("'upper_quantile' must be length 1 or 2")
  }

  if (length(lower_quantile) == 1) {
    den_critvals <- c(-Inf, quantile(phs, lower_quantile))
  } else if (length(lower_quantile) == 2) {
    den_critvals <- c(quantile(phs, lower_quantile[1]), quantile(phs, lower_quantile[2]))
  } else {
    stop("'lower_quantile' must be length 1 or 2")
  }

  tmp_df <- data.frame(age, status, phs)

  ix_num <- which(phs >= num_critvals[1] & phs <= num_critvals[2])
  ix_den <- which(phs >= den_critvals[1] & phs <= den_critvals[2])

  tmp_df %>% 
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
#' Compute Cumulative Incidence
#'
#' @param country The country name
#' @param inc_file The incidence file name
#' @param prop_file The proportions file name
#' @return A list with cumulative incidence and incidence data frames
#' @examples
#' cuminc_data <- RK_get_cuminc("USA")
#' @export
RK_get_cuminc <- function(country, inc_file = "prostate_cancer_incidence_country.csv", prop_file = "intermediate_high_proportions_CAP.csv") {
  incTable <- read.csv(inc_file)
  propTable <- read.csv(prop_file)
  
  if (!any(incTable$Country == country)) {
    stop('Cannot find country in incidence table!')
  }
  
  ix <- incTable$Country == country
  timeRange <- 40:70
  cuminc <- matrix(NA, nrow = nrow(incTable), ncol = length(timeRange))
  inc <- matrix(NA, nrow = nrow(incTable), ncol = length(timeRange))
  
  for (id1 in 1:nrow(incTable)) {
    tmp <- incTable[id1, ]
    tmp_inc <- exp(tmp$a) * exp(tmp$b * (timeRange - 40))
    tmp_cuminc <- (exp(tmp$a) / tmp$b) * exp(tmp$b * (timeRange - 40))
    tmp_cuminc <- tmp_cuminc - tmp_cuminc[1]
    cuminc[id1, ] <- tmp_cuminc
    inc[id1, ] <- tmp_inc
  }
  
  cuminc_df <- data.frame(age = timeRange, total = cuminc[ix, ])
  inc_df <- data.frame(age = timeRange, total = inc[ix, ])
  
  propTable$low <- 1 - propTable$IntermediatePlus
  propTable$intermediate <- propTable$IntermediatePlus - propTable$High
  propTable$high <- propTable$High
  propTable$intermediatehigh <- propTable$IntermediatePlus
  
  ctype <- c('low', 'intermediate', 'high', 'intermediatehigh')
  
  for (id1 in ctype) {
    mod <- lm(propTable[[id1]] ~ propTable$Age)
    cuminc_df[[id1]] <- cuminc_df$total * predict(mod, data.frame(Age = cuminc_df$age))
  }
  
  return(list(cuminc = cuminc_df, inc = inc_df))
}


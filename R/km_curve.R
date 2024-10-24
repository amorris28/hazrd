#' Return Kaplan-Meier curve
#'
#' This function returns a single Kaplan-Meier curve for plotting.
#'
#' @param data a data.frame containing a column for `phs`, `age`, and `status`
#' @param upper a single number specifying the upper end of the quantile. e.g., 1.0
#' @param lower a single number specifying the lower end of the quantile. e.g., 0.8
#' @param age_range a vector of ages over which curves should be calculated. Default = 40:100
#' @param scale logical. if `TRUE` centers and scales the PHS scores to unit variance. Default = `FALSE`.
#' @param inverse logical. if `TRUE` calculates the inverse (x * -1) the PHS scores to reverse the direction of effect. Default = `FALSE`.
#' @importFrom tidyr pivot_longer
#' @importFrom survival coxph Surv survfit
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @return A ggplot2 plot of a K-M curve
#' @examples
#' km_curve <- km_curve(model_file, metadata_file, inverse = TRUE, ideal = FALSE)
#' @export
km_curve <- function(data, upper, lower, age_range = 40:100, scale = FALSE, inverse = FALSE, ideal = FALSE) {
  scale <- as.logical(scale)
  inverse <- as.logical(inverse)
    
  if (scale) { data$phs <- scale(data$phs, center = TRUE, scale = TRUE) }
  if (inverse) { data$phs <- data$phs * -1 }
  
  critvals <- c(quantile(data$phs, lower), quantile(data$phs, upper))

  ix <- which(data$phs >= critvals[1] & data$phs <= critvals[2])
  
  cox_model <- coxph(Surv(age, status) ~ phs, data = data)
  
  bt <- cox_model$coefficients[["phs"]]
  
  quantile_data <- data[ix, ]
  
  median_phs_quantile = quantile(quantile_data$phs, 0.50)
  median_phs_all <- quantile(data$phs, 0.50)

  #########################
  # Calculate idealized curves
  #
  # Basically copied this from:
  # https://github.com/cmig-research-group/phs/blob/master/phs_model_example/phs_generate_new_demo.m
  a <- 0.0700 / 100
  b <- 0.0753
  pcainc <- a * exp(b * (age_range - min(age_range))) # this is the population incidence rate for PCa
  H0 <- a / b * exp(b * (age_range - min(age_range))) # compute baseline hazard
  H0 <- H0 - H0[1] # assume cumulative hazard == 0 before age 40
  
  H <- vector(length = length(H0))
  S <- vector(length = length(H0))

  H <- H0 * exp(bt * median_phs_quantile - bt * median_phs_all)
  S <- exp(-H)
  
  S_tibble <- as_tibble(S)
  S_tibble$age_range <- age_range
  # S_tibble <- S_tibble %>%
  #   pivot_longer(-agevals, names_to = "percentile")
  
  ##########################
  
  # Plot K-M curves for centiles
  mod <- survfit(Surv(age, status) ~ 1, data = quantile_data)
  
  tidy_strata <- function(strata) {
    temp <- c()
    for (i in seq_along(strata)) {
      temp <- c(temp, rep_len(names(strata)[i], strata[i]))
    }
    temp
  }
  
  mod_data <- tibble(time = mod$time,
                     n.risk = mod$n.risk,
                     n.event = mod$n.event,
                     n.censor = mod$n.censor,
                     estimate = mod$surv,
                     std.error = mod$std.err,
                     conf.high = mod$upper,
                     conf.low = mod$lower)
  
ggplot(mod_data, aes(x = time, y = estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.5) +
    geom_step() +
    theme_minimal() +
    xlim(min(age_range), max(age_range)) + 
    ylim(0, 1.1) +
    labs(x = "Age", y = "PCa-free Survival")

  if ( ideal ) {
    kmcurve = kmcurve + 
	geom_line(aes(x = age_range, y = value), S_tibble)
  }
  kmcurve =
      select(mod_data, time, estimate, conf.high, conf.low)
  return(kmcurve)
  
#  combined_data <- combined_data %>% 
#    group_by(status) %>% 
#    summarize(n = n())
#  
#  cases_controls <- tibble(status = combined_data$status, count = combined_data$n)
#  
#  ## Hazard ratios
#  perform <- with(combined_data, 
#                  RK_get_perf(phs, 
#                              age, 
#                              status, 
#                              ref = phs, 
#                              swc_popnumcases = cases_controls[2, 2], 
#                              swc_popnumcontrols = cases_controls[1, 2]))
#  perform <- stack(perform)[, c(2, 1)]
#  
#  write_tsv(perform, paste0(output, model, "_performance.txt"))
}


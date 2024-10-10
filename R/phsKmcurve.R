#' Plot Kaplan-Meier curve
#'
#' This function processes PHS data and generates a K-M plot.
#'
#' @param model_file Path to the PHS file (tab delimited, no header, first column ID, second column PHS score)
#' @param metadata_file Path to the metadata file (tab delimited with column names bfile_id, age, status)
#' @param inverse Boolean indicating whether to inverse (x * -1) the PHS scores to reverse the direction of effect.
#' @param ideal Boolean indicating whether to add the idealized survival curve.
#' @importFrom readr read_tsv
#' @importFrom survival coxph Surv
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @return A ggplot2 plot of a K-M curve
#' @examples
#' km_curve <- kmCurve(model_file, metadata_file, inverse = TRUE, ideal = FALSE)
#' @export
kmCurve <- function(model_file, metadata_file, inverse = FALSE, ideal = FALSE) {
  
  model <- basename(model_file) # filename for output
  metadata <- read_tsv(metadata_file) # Phenotype data
  inverse <- as.logical(inverse)
  
  #########################################
  # Import PHS data
  
  phs <- read_tsv(model_file, col_names = c("bfile_id", "phs"))
  
  phs$phs <- scale(phs$phs, center = TRUE, scale = TRUE)
  
  if (inverse) { phs$phs <- phs$phs * -1 }
  
  combined_data <- metadata %>%
    left_join(phs, by = "bfile_id") %>%
    filter(!(is.na(phs)))
  
  # Fit model
  
  agevals <- 40:100
  reference_PHS <- combined_data$phs
  prctile_list <- c(0, 0.2, 0.7, 0.8, 0.98, 1)
  prctile_labels <- c('0-20th', '20-70th', '70-80th', '80-98th', '98-100th')
  
  combined_data$percentile <- with(combined_data, 
                                   cut(phs, breaks = quantile(phs, probs = prctile_list), 
                                       labels = prctile_labels, 
                                       include.lowest = TRUE))
  
  cox_model <- coxph(Surv(age, status) ~ phs, data = combined_data)
  
  bt <- cox_model$coefficients[["phs"]]
  
  combined_data <- combined_data %>% 
    filter(percentile %in% c('0-20th', '20-70th', '80-98th', '98-100th'))
  
  median_tibble <- combined_data %>% 
    group_by(percentile) %>% 
    summarize(median_phs = median(phs))
  
  median_phs_individual <- quantile(reference_PHS, 0.50)
  critvalvec <- median_tibble$median_phs
  
  #########################
  # Calculate idealized curves
  #
  # Basically copied this from:
  # https://github.com/cmig-research-group/phs/blob/master/phs_model_example/phs_generate_new_demo.m
  a <- 0.0700 / 100
  b <- 0.0753
  pcainc <- a * exp(b * (agevals - 40)) # this is the population incidence rate for PCa
  H0 <- a / b * exp(b * (agevals - 40)) # compute baseline hazard
  H0 <- H0 - H0[1] # assume cumulative hazard == 0 before age 40
  
  H <- matrix(nrow = length(H0), ncol = length(critvalvec))
  S <- matrix(nrow = length(H0), ncol = length(critvalvec))
  
  for (cnt in seq_along(critvalvec)) {
    H[, cnt] <- H0 * exp(bt * critvalvec[cnt] - bt * median_phs_individual[[1]])
    S[, cnt] <- exp(-H[, cnt])
  }
  
  S_tibble <- as_tibble(S, .name_repair = ~ as.character(median_tibble$percentile))
  S_tibble$agevals <- agevals
  S_tibble <- S_tibble %>%
    pivot_longer(-agevals, names_to = "percentile")
  
  ##########################
  
  # Plot K-M curves for centiles
  mod <- survfit(Surv(age, status) ~ percentile, data = combined_data)
  
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
                     conf.low = mod$lower,
                     strata = tidy_strata(mod$strata))
  
  key <- tibble(strata = unique(mod_data$strata),
                percentile = unique(S_tibble$percentile))
  
  mod_data <- mod_data %>% 
    left_join(key, by = 'strata')
  
  kmcurve <- ggplot(mod_data, aes(x = time, y = estimate, color = percentile)) +
    geom_step() +
    theme_minimal() +
    xlim(40, 100) + 
    scale_color_brewer(palette = "Set1") +
    labs(x = "Age", y = "PCa-free Survival")

  if ( ideal ) {
    kmcurve = kmcurve + 
	geom_line(aes(x = agevals, y = value, color = percentile), data = mod_data, S_tibble)
  }
  
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


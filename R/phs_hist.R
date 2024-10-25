#' Plot Histogram of Cases and Controls by PHS
#'
#' This function takes PHS scores and case/control status and plots a histogram.
#'
#' @param data an optional data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"
#' @param normalize Boolean indicating whether to normalize the histogram to area = 1. This replaces the y axis with denisty instead of count. Helpful to compare distributions with very different counts. Default = `FALSE`.
#' @param scale Boolean indicating whether to center and scale the PHS scores to unit variance. Default = `FALSE`.
#' @param inverse Boolean indicating whether to inverse (x * -1) the PHS scores to reverse the direction of effect. Default = `FALSE`.
#' @import ggplot2
#' @return A ggplot object
#' @examples
#' phs_hist <- phs_hist(df, normalize = TRUE)
#' @export
phs_hist <- function(data = NULL,
                     phs = "phs",
                     status = "status",
                     normalize = FALSE,
                     scale = FALSE,
                     inverse = FALSE) {
  scale <- as.logical(scale)
  inverse <- as.logical(inverse)
  normalize <- as.logical(normalize)

  if (is.character(phs)) {
    phs = data[[phs]]
  }
  if (is.character(status)) {
    status = data[[status]]
  }

  if (scale) { phs <- scale(phs, center = TRUE, scale = TRUE) }

  if (inverse) { phs <- phs * -1 }

  tmp_df <- data.frame(status, phs)

  if (normalize) {
    phs_hist = ggplot(tmp_df, aes(phs, after_stat(density), fill = as.factor(status))) +
        geom_histogram(binwidth = 0.5, alpha = 0.8, position = "identity") +
      scale_fill_manual(values = c("#132B43", "#56B1F7"), name = "Status") +
        theme_minimal() +
        labs(x = "PHS", y = "Density")
} else {
    phs_hist = ggplot(tmp_df, aes(phs, fill = as.factor(status))) +
        geom_histogram(binwidth = 0.5, alpha = 0.8, position = "identity") +
        scale_fill_manual(values = c("#132B43", "#56B1F7"), name = "Status") +
        theme_minimal() +
        labs(x = "PHS", y = "Count")
}

  return(phs_hist)
}


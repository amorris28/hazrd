#' Plot Histogram of Cases and Control by PHS
#'
#' This function takes PHS scores and case/control status and plots a histogram.
#'
#' @param df Data.frame or tibble containing a column for `phs` and a column for `status`. All other columns ignored.
#' @param normalize Boolean indicating whether to normalize the histogram to area = 1. This is replaces the y axis with denisty instead of count. Helpful to compare distributions with very different counts. Default = `FALSE`.
#' @param scale Boolean indicating whether to center and scale the PHS scores to unit variance. Default = `FALSE`.
#' @param inverse Boolean indicating whether to inverse (x * -1) the PHS scores to reverse the direction of effect. Default = `FALSE`.
#' @import ggplot2
#' @return A ggplot object
#' @examples
#' phs_hist <- phs_hist(df)
#' @export
phs_hist <- function(df, normalize = FALSE, scale = FALSE, inverse = FALSE) {

  if (scale) { df$phs <- scale(df$phs, center = TRUE, scale = TRUE) }

  if (inverse) { df$phs <- df$phs * -1 }

  if (normalize) {
    phs_hist = ggplot(df, aes(phs, after_stat(density), fill = status)) +
        geom_histogram(binwidth = 0.5, alpha = 0.8, position = "identity") +
      scale_fill_manual(values = c("#132B43", "#56B1F7"), name = "Status") +
        theme_minimal() +
        labs(x = "PHS", y = "Density")
} else {
    phs_hist ggplot(df, aes(phs, fill = status)) +
        geom_histogram(binwidth = 0.5, alpha = 0.8, position = "identity") +
        scale_fill_manual(values = c("#132B43", "#56B1F7"), name = "Status") +
        theme_minimal() +
        labs(x = "PHS", y = "Count")
}

  return(phs_hist)
}


#' Plot Histogram of Cases and Control by PHS
#'
#' This function takes PHS scores and case/control status and plots a histogram..
#'
#' @param model_file Path to the PHS file (tab delimited, no header, first column ID, second column PHS score)
#' @param metadata_file Path to the metadata file (tab delimited with column names bfile_id, age, status)
#' @param inverse Boolean indicating whether to inverse (x * -1) the PHS scores to reverse the direction of effect.
#' @importFrom readr read_tsv
#' @import ggplot2
#' @return A ggplot object
#' @examples
#' phs_hist <- phsHist(model_file, metadata_file, inverse)
#' @export
phsHist <- function(model_file, metadata_file, inverse) {
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

  phs_hist <- ggplot(combined_data, aes(x = phs, group = status, fill = as.character(status))) +
    geom_histogram() +
    scale_fill_manual(values = c("#132B43", "#56B1F7")) +
    theme_minimal()

  return(phs_hist)
}


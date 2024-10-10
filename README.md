
# hazRd

The goal of hazRd is to simplify and standardize the development and testing of polygenic hazard score models.

## Installation

You can install the development version of hazRd from [GitHub](https://github.com/) with:

``` r
devtools::install_github("amorris28/hazRd")
```

## Example

This is a basic example of some of the main functions:

``` r
library(hazRd)
## Plot a histogram of case and controls by PHS score
phs_hist <- phsHist(model_file, metadata_file, inverse = TRUE)
## Plot a Kaplain-Meier curve
km_curve <- kmCurve(model_file, metadata_file, inverse = TRUE, ideal = FALSE)
## Get standard performance metrics (such as concordance index and hazard ratio)
perf_metrics <- RK_get_perf(lp, Age, status)
```


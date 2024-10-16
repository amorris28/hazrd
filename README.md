
# hazRd

The goal of hazRd is to simplify and standardize the development and testing of polygenic hazard score models.

## Installation

You can install the development version of hazRd from [GitHub](https://github.com/) with:

``` r
devtools::install_github("amorris28/hazRd")
```

## Getting Started

First, generate some test data.

``` r
library(tidyverse)
library(hazRd)

n = 1000
status = rbinom(n, 1 ,0.2)

test_data = tibble(id = as.factor(seq_len(n)),
                  phs  = rnorm(n) + (1 * status),
                  status = as.factor(status),
                  Age = sample(60:100, n, replace = TRUE))

```

Next, plot the histogram of PHSes by case/control status.

``` r

ggplot(test_data, aes(phs, after_stat(density), fill = status)) +
    geom_histogram(binwidth = 0.5, alpha = 0.8, position = "identity") +
    scale_fill_manual(values = c("#132B43", "#56B1F7"), name = "Status") +
    theme_minimal() +
    labs(x = "PHS", y = "Density")

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

## Developer Instructions

These are general instructions for how to create and document an R package.

``` r
install.packages(c("usethis", "devtools", "roxygen2"))
```

First, navigate to where you want to create the R package project directory. Open up an `R` console and run the `create_package` command. The first argument will be the name of the package and the name of the directory that is created within your current working directory.

``` r
usethis::create_package("my_package")
```

Next, navigate to your package directory (`cd my_package`) and develop your package by adding code to the `R/` directory. Here is a simple example of how to structure an `.R` script within the `R/` directory called `my_function.R`.

``` r
#' This is the Title of the Help Page for my_function
#'
#' This is a description of what this function does.
#'
#' @param x Description of what the "x" parameter expects, default value, whether it is optional
#' @param y Description of what the "y" parameter expects, etc.
#' @return Description of what this function returns
#' @examples
#' my_results <- my_function(x = 1, y = 2)
#' @export
my_function <- function(x, y) {
 # Some code
}
```

Once you have created some files in the `R/` directory, you can automatically generate documentation using either `roxygen2::roxygenise` or `devtools::document`. `document` is generally preferred and actually calls `roxygenise` as part of its testing. You can either call it without any arguments from the root directory of your R package or you can specify the path to the package as the first argument.

``` r
devtools::document("path_to_your_package")
```

Now, you can install the package locally using `devtools`.

``` r
devtools::install("path_to_your_package")
```

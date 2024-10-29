hazrd
================

The goal of hazrd is to simplify and standardize the development and
testing of polygenic hazard score models in an opinionated way based
mainly on past work in the [PHS
repo](https://github.com/cmig-research-group/phs). Currently, most of
the functionality focuses on external validation of PHS models. This
includes calculation of standard statistics (80-20 hazard ratios, 80-20
odds ratios, and concordance index) and plotting of Kaplan-Meier curves
and cumulative incidence curves. In the future, this will be expanded to
model development, internal validation, imaging risk scores, multimodal
hazard scores, and functions related to Digital Avatar.

## Installation

You can install the development version of hazrd from
[GitHub](https://github.com/) by cloning the repository:

``` bash
git clone git@github.com:amorris28/hazrd.git
```

and then opening `R` and installing the package:

``` r
devtools::install("hazrd")
```

Replace `"hazrd"` with the path to the repository on your local
computer.

## Getting Started

First, generate some test data.

``` r
library(ggplot2)
library(hazrd)
set.seed(46495809)

n = 1000
status = rbinom(n, 1, 0.2)

test_data = data.frame(phs  = rnorm(n) + (1 * status),
                       status = status,
                       age = sample(40:100, n, replace = TRUE))
```

Next, plot the histogram of PHSes by case/control status.

``` r
phs_hist(test_data, normalize = TRUE)
```

![](README_files/figure-gfm/phs_hist-1.png)<!-- -->

Then, calculate the hazard ratio comparing the mean of the top 20% of
PHSes to the mean of the bottom 20% (i.e., `HR80_20`). We can also
generate 95% confidence intervals using bootstrapping.

``` r
HR80_20 = get_hr(test_data, boot = TRUE, B = 300)
print(HR80_20)
```

    ## $HR
    ## [1] 7.347802
    ## 
    ## $conf.low
    ## [1] 5.009186
    ## 
    ## $conf.high
    ## [1] 11.59092

Similarly, calculate the odds ratio at age 70 between the top 20% and
bottom 20% of PHSes.

``` r
OR80_20 = get_or(test_data, or_age = 70, boot = TRUE, B = 300)
print(OR80_20)
```

    ## $OR
    ## [1] 0.858156
    ## 
    ## $conf.low
    ## [1] 0.4219104
    ## 
    ## $conf.high
    ## [1] 1.869373

Return the concordance index with 95% confidence intervals from a coxph
fit:

``` r
c_index = get_cindex(test_data,  boot = TRUE, B = 300)
print(c_index)
```

    ## $c_index
    ## [1] 0.7125285
    ## 
    ## $conf.low
    ## [1] 0.6734051
    ## 
    ## $conf.high
    ## [1] 0.7486092

Finally, plot the Kaplan-Meier curves with confidence intervals for
centiles of interest.

``` r
curves = data.frame(lower = c(0,   0.2, 0.8,  0.98),
                    upper = c(0.2, 0.7, 0.98, 1))

label_generator = function(x, y) {
    x = x * 100
    y = y * 100
    out = paste0("PHS ", x, "-", y, "th centile")
    return(out)
}

km_curves = data.frame()
for (i in seq_len(nrow(curves))) {
    curven <- km_curve(data = test_data,  
                       lower = curves$lower[i],
                       upper = curves$upper[i], 
                       age_range = 40:100, 
                       scale = FALSE, 
                       inverse = FALSE)
    curven$label = label_generator(curves$lower[i], curves$upper[i])
    km_curves = rbind(km_curves, curven)
}


ggplot(km_curves, aes(x = time, 
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high,
                      col = label,
                      fill = label)) +
    geom_ribbon(alpha = 0.1,
                color = 0) +
    geom_step() +
    theme_minimal() +
    xlim(min(40), max(100)) + 
    ylim(0, 1) +
    labs(x = "Age", y = "Disease-free Survival") +
    scale_color_brewer(palette = "Set1",
                       name = "Centile") +
    scale_fill_brewer(palette = "Set1",
                       name = "Centile")
```

![](README_files/figure-gfm/km_curve-1.png)<!-- -->

## Developer Instructions

These are general instructions for how to create and document an R
package.

``` r
install.packages(c("usethis", "devtools", "roxygen2"))
```

First, navigate to where you want to create the R package project
directory. Open up an `R` console and run the `create_package` command.
The first argument will be the name of the package and the name of the
directory that is created within your current working directory.

``` r
usethis::create_package("my_package")
```

Next, navigate to your package directory (`cd my_package`) and develop
your package by adding code to the `R/` directory. Here is a simple
example of how to structure an `.R` script within the `R/` directory
called `my_function.R`.

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

Once you have created some files in the `R/` directory, you can
automatically generate documentation using either `roxygen2::roxygenise`
or `devtools::document`. `document` is generally preferred and actually
calls `roxygenise` as part of its testing. You can either call it
without any arguments from the root directory of your R package or you
can specify the path to the package as the first argument.

``` r
devtools::document("path_to_your_package")
```

Now, you can install the package locally using `devtools`.

``` r
devtools::install("path_to_your_package")
```

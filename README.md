hazrd
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/amorris28/hazRd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/amorris28/hazRd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

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

### Github

To install the most recent release from Github, go to the
[release](https://github.com/amorris28/hazrd/releases) page, scroll down
to Assets, and download the “Source code (tar.gz)” file. Then, in `R`,
run:

``` r
install.packages("hazrd-0.1.0.tar.gz", repos = NULL, type="source")
```

### Using `devtools`

For a more up-to-date installation with all of the changes since the
last release, install the development version of hazrd from
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

#### On a remote cluster

To install the development version on a remote cluster, such as TSD,
build a tarball from your local clone of the Github repo:

``` r
devtools::build("hazrd")
```

You can then install the `.tar.gz` with `install.packages` in `R`:

``` r
install.packages("hazrd_0.1.0.tar.gz", repos = NULL, type="source")
```

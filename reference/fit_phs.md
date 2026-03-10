# Fit a polygenic hazard score (PHS) model

`fit_phs()` fits a PHS model from either a formula interface (similar to
`coxph()`) or directly from a data frame with specified columns. It
returns an object of class `"phsfit"`, which can then be passed to
[`get_hr()`](https://amorris28.github.io/hazrd/reference/get_hr.md),
[`get_or()`](https://amorris28.github.io/hazrd/reference/get_or.md),
[`get_cindex()`](https://amorris28.github.io/hazrd/reference/get_cindex.md),
etc.

## Usage

``` r
fit_phs(object, ...)

# S3 method for class 'formula'
fit_phs(object, data, bootstrap.iterations = NULL, conf.level = 0.95, ...)

# S3 method for class 'data.frame'
fit_phs(
  object,
  age = "age",
  status = "status",
  phs = "phs",
  bootstrap.iterations = NULL,
  conf.level = 0.95,
  ...
)
```

## Arguments

- object:

  Either

  - a survival formula, e.g. `Surv(age, status) ~ phs` (formula
    interface), or

  - a `data.frame` containing the variables for `phs`, `age`, and
    `status` (data-frame interface).

- ...:

  Additional arguments (currently ignored).

- data:

  A `data.frame` containing variables in the formula (formula interface
  only).

- bootstrap.iterations:

  Number of bootstrap iterations to run to generate confidence
  intervals.

- conf.level:

  The confidence level to use for the confidence interval if conf.int =
  TRUE. Must be strictly greater than 0 and less than 1. Defaults to
  0.95, which corresponds to a 95 percent confidence interval.

- age:

  A string specifying the column name in `data` containing the age of
  each subject. For cases, this should be the age at event (e.g.,
  diagnosis) and for controls this should be age of censoring (e.g.,
  last observation). Default `"age"`.

- status:

  A string specifying the column name in `data` containing case-control
  status (0 = censored, 1 = event). Default `"status"`.

- phs:

  A string specifying the column name in `data` containing the polygenic
  hazard score for each subject (data-frame interface only). Default
  `"phs"`.

## Value

An object of class `"phsfit"` containing the base Cox model, any
bootstrap fits and indices, the formula, the original data, and
bootstrap metadata.

## See also

`fit_phs.formula()`, `fit_phs.data.frame()`

## Examples

``` r
# Formula interface:
fit <- fit_phs(Surv(age, status) ~ phs, data = test_data)
#> Error in fit_phs(Surv(age, status) ~ phs, data = test_data): could not find function "fit_phs"

# Data frame interface:
fit <- fit_phs(test_data, age = "age", status = "status", phs = "phs")
#> Error in fit_phs(test_data, age = "age", status = "status", phs = "phs"): could not find function "fit_phs"
```

# Deprecated: use phs_metrics() instead

`get_hr()` is deprecated as of hazrd 0.2.0. Please use
[`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
with `metrics = "HR"` instead.

## Usage

``` r
get_hr(
  data = NULL,
  phs = "phs",
  age = "age",
  status = "status",
  lower_interval = 0.2,
  upper_interval = 0.8,
  CI = FALSE,
  bootstrap_iterations = 1000,
  swc = FALSE,
  swc_popnumcases = NULL,
  swc_popnumcontrols = NULL
)
```

## Arguments

- data:

  a data.frame containing PHS data

- phs:

  column name for PHS values. Default `"phs"`

- age:

  column name for time-to-event. Default `"age"`

- status:

  column name for event indicator. Default `"status"`

- lower_interval:

  single quantile (upper bound of denominator band, lower bound defaults
  to 0) or length-2 vector `c(lower, upper)`. Default `0.20`

- upper_interval:

  single quantile (lower bound of numerator band, upper bound defaults
  to 1) or length-2 vector `c(lower, upper)`. Default `0.80`

- CI:

  logical. If `TRUE`, bootstrap CIs are computed. Default `FALSE`

- bootstrap_iterations:

  number of bootstrap replicates. Default `1000`

- swc:

  logical. Sample weight correction. Not supported in
  [`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
  — a warning is emitted if `TRUE`. Default `FALSE`

- swc_popnumcases:

  ignored (swc not yet implemented in
  [`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md))

- swc_popnumcontrols:

  ignored (swc not yet implemented in
  [`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md))

## Value

A list with elements `index`, `value`, `conf.low`, `conf.high`, and
`iters`, matching the return structure of the previous release.

## See also

[`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)

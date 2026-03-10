# Deprecated: use phs_metrics() instead

`get_or()` is deprecated as of hazrd 0.2.0. Please use
[`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
with `metrics = "OR"` instead.

## Usage

``` r
get_or(
  data = NULL,
  phs = "phs",
  age = "age",
  status = "status",
  or_age,
  numerator = c(0.8, 1),
  denominator = c(0, 0.2),
  bootstrap.iterations = NULL,
  conf.level = 0.95
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

- or_age:

  age at which to compute the OR. Required.

- numerator:

  length-2 numeric vector for numerator band. Default `c(0.8, 1.0)`

- denominator:

  length-2 numeric vector for denominator band. Default `c(0.0, 0.2)`

- bootstrap.iterations:

  number of bootstrap replicates, or `NULL`

- conf.level:

  confidence level. Default `0.95`

## Value

A list with element `value` and optionally `conf.low`/`conf.high`.

## See also

[`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)

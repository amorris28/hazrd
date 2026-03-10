# Deprecated: use phs_metrics() instead

`get_cindex()` is deprecated as of hazrd 0.2.0. Please use
[`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
with `metrics = "C_index"` instead.

## Usage

``` r
get_cindex(object, ...)

# S3 method for class 'data.frame'
get_cindex(
  object,
  phs = "phs",
  age = "age",
  status = "status",
  bootstrap.iterations = NULL,
  conf.level = 0.95,
  ...
)

# S3 method for class 'phsfit'
get_cindex(object, bootstrap.iterations = NULL, conf.level = 0.95, ...)
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

- bootstrap.iterations:

  number of bootstrap replicates, or `NULL`

- conf.level:

  confidence level. Default `0.95`

## Value

A list with element `value` and optionally `conf.low`/`conf.high`.

## See also

[`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)

# Numbers-at-risk table for PHS percentile-stratified groups

Builds a ggplot2 numbers-at-risk strip intended for combining with a
[`phs_abs_risk()`](https://amorris28.github.io/hazrd/reference/phs_abs_risk.md)
or
[`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md)
plot using patchwork. Accepts the same data and interval arguments as
those functions and computes the survival data internally.

## Usage

``` r
phs_risk_table(
  data,
  phs = "phs",
  time = "age",
  event = "status",
  competing_event = NULL,
  intervals = list(c(0.95, 1), c(0.8, 1), c(0.3, 0.7), c(0, 0.2)),
  ref_data = NULL,
  method = "km",
  palette = "hazrd",
  time_points = NULL,
  ...
)
```

## Arguments

- data:

  data.frame with columns specified by `phs`, `time`, and `event`

- phs:

  string; column name of the continuous PHS values

- time:

  string; column name of the time-to-event variable

- event:

  string; column name of the event indicator (0 = censored, 1 = event of
  interest)

- competing_event:

  string or NULL; column name of the competing event indicator. Required
  when `method = "aalen_johansen"`.

- intervals:

  list of `c(lo, hi)` pairs defining percentile bands. Should match the
  `intervals` used in the accompanying
  [`phs_abs_risk()`](https://amorris28.github.io/hazrd/reference/phs_abs_risk.md)
  or
  [`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md)
  call.

- ref_data:

  optional data.frame used as the reference population for computing
  percentile cutpoints.

- method:

  string; `"km"` (default) or `"aalen_johansen"`. Should match the
  method used in the accompanying curve function.

- palette:

  string; color palette name. Should match the palette used in the
  accompanying curve plot (default `"hazrd"`).

- time_points:

  numeric vector or NULL; time values at which to display numbers at
  risk. For example, `c(50, 60, 70, 80, 90)`. When NULL (default),
  values are chosen automatically using
  [`pretty()`](https://rdrr.io/r/base/pretty.html).

- ...:

  additional arguments (reserved for future use).

## Value

A ggplot2 object displaying the numbers at risk over time for each
stratum. Combine with the output of
[`phs_abs_risk()`](https://amorris28.github.io/hazrd/reference/phs_abs_risk.md)
or
[`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md)
using patchwork, e.g.:
`p / phs_risk_table(data) + patchwork::plot_layout(heights = c(3, 1))`.

## Examples

``` r
data(test_data)

p  <- phs_abs_risk(test_data)
rt <- phs_risk_table(test_data)

# Specify exact ages for the at-risk columns:
rt <- phs_risk_table(test_data, time_points = c(50, 60, 70, 80))

# Combine with patchwork:
# p / rt + patchwork::plot_layout(heights = c(3, 1))
```

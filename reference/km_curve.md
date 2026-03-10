# Deprecated: use phs_km() instead

`km_curve()` is deprecated as of hazrd 0.2.0. Please use
[`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md)
instead.

## Usage

``` r
km_curve(
  data = NULL,
  phs = "phs",
  age = "age",
  status = "status",
  interval = c(0, 1),
  age_range = 40:100,
  scale = FALSE,
  inverse = FALSE
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

- interval:

  numeric vector of length 2 specifying the lower and upper quantiles
  for filtering subjects. Default `c(0, 1)`. Has no direct equivalent in
  `phs_km()` — a warning is emitted and the argument is ignored; all
  subjects are included in the stratified output.

- age_range:

  ages over which to calculate curves. Default `40:100`. Not supported
  in `phs_km()` — a warning is emitted and the argument is ignored.

- scale:

  logical. If `TRUE`, PHS is centred and scaled to unit variance. Not
  supported in `phs_km()` — a warning is emitted and the argument is
  ignored. Scale your PHS column before calling `phs_km()`.

- inverse:

  logical. If `TRUE`, PHS is multiplied by `-1`. Not supported in
  `phs_km()` — a warning is emitted and the argument is ignored.
  Multiply your PHS column manually before calling `phs_km()`.

## Value

A data.frame as returned by
[`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md)
with `output = "data"`.

## See also

[`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md)

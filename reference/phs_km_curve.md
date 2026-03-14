# Kaplan-Meier curves stratified by PHS percentile

Compute Kaplan-Meier survival curves stratified by PHS percentile
groups. Returns either a ggplot object (output = "plot") or a tidy data
frame (output = "data"). Preset strata and custom cutpoints are
supported; percentiles are computed from `ref_data` when supplied so a
training-\>validation workflow is possible.

## Usage

``` r
phs_km_curve(
  data,
  phs = "phs",
  time = "age",
  event = "status",
  intervals = list(c(0.95, 1), c(0.8, 1), c(0.3, 0.7), c(0, 0.2)),
  breaks = NULL,
  ref_data = NULL,
  output = "plot",
  conf_int = TRUE,
  conf_int_alpha = 0.15,
  palette = "hazrd",
  risk_table = FALSE,
  ...
)
```

## Arguments

- data:

  data.frame with columns specified by `phs`, `time`, `event`

- phs:

  string or numeric vector; column name or vector of PHS values

- time:

  string or numeric vector; column name or vector of event time

- event:

  string or numeric vector; column name or vector of event indicator
  (0/1)

- intervals:

  list of `c(lo, hi)` pairs defining percentile bands, e.g.
  `list(c(0.80, 1), c(0, 0.20))`. Bands may overlap. Takes precedence
  over `breaks` when non-`NULL`. Default produces four bands: top 5
  (0.95-1), top 20 (0.80-1), middle 40 (0.30-0.70), and bottom 20
  (0-0.20).

- breaks:

  numeric vector of percentile cutpoints strictly in (0, 1) used to form
  exclusive bands (legacy); ignored when `intervals` is non-`NULL`.
  Retained for backward compatibility.

- ref_data:

  optional data.frame used to compute percentile cutpoints (training
  reference)

- output:

  'plot' (default) or 'data'

- conf_int:

  logical; include confidence intervals in output/plot

- conf_int_alpha:

  numeric; alpha for confidence ribbons when plotting

- palette:

  string; name of color palette to use for plots (default: 'hazrd')

- risk_table:

  logical; if TRUE returns numbers-at-risk in the data output
  (experimental)

- ...:

  additional args (reserved)

## Value

ggplot object (when output = 'plot') or data.frame (when output =
'data')

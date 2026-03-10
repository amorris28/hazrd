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
  breaks = c(0.2, 0.8),
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

- breaks:

  numeric vector of percentile cutpoints strictly in (0, 1); default
  `c(0.20, 0.80)` (bottom 20% / middle 60% / top 20%)

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

# Cox model survival curves at specified PHS percentiles

Fit a Cox proportional-hazards model with `phs` as the sole predictor
and return predicted survival curves for individuals at specified PHS
percentiles. Unlike
[`phs_km_curve()`](https://amorris28.github.io/hazrd/reference/phs_km_curve.md),
these are smooth model-based predictions rather than empirical group
estimates.

## Usage

``` r
phs_cox_curve(
  data,
  phs = "phs",
  time = "age",
  event = "status",
  percentiles = c(0.01, 0.05, 0.2, 0.5, 0.8, 0.95, 0.99),
  ref_data = NULL,
  output = "plot",
  conf_int = TRUE,
  conf_int_alpha = 0.15,
  palette = "hazrd",
  ...
)
```

## Arguments

- data:

  data.frame with columns specified by `phs`, `time`, `event`

- phs:

  string or numeric vector; column name or vector of PHS values

- time:

  string or numeric vector; column name or vector of event times

- event:

  string or numeric vector; column name or vector of event indicators
  (0/1)

- percentiles:

  numeric vector of percentiles strictly in (0, 1) at which to compute
  Cox-predicted survival curves; default
  `c(0.01, 0.05, 0.20, 0.50, 0.80, 0.95, 0.99)`

- ref_data:

  optional data.frame used to compute the PHS value at each requested
  percentile (training reference); if `NULL`, percentiles are computed
  from `data`

- output:

  `'plot'` (default) or `'data'`

- conf_int:

  logical; include confidence intervals in output/plot

- conf_int_alpha:

  numeric; alpha for confidence ribbons when plotting

- palette:

  string; colour palette for plots (default: `'hazrd'`)

- ...:

  additional args (reserved)

## Value

ggplot object (when `output = 'plot'`) or data.frame (when
`output = 'data'`). The data frame has columns `time`, `estimate`,
`conf.low`, `conf.high`, `percentile`, and `percentile_value`.

## Details

Percentile values are computed from `ref_data` when supplied, which
supports a training→validation workflow where the reference distribution
comes from a training cohort.

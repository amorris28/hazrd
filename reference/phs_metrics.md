# Calculate evaluation metrics for Polygenic Hazard Scores

Calculate evaluation metrics for Polygenic Hazard Scores

## Usage

``` r
phs_metrics(
  data,
  phs = "phs",
  time = "age",
  event = "status",
  metrics = c("HR", "C_index"),
  hr_method = "continuous_group",
  hr_numerator = NULL,
  hr_denominator = NULL,
  hr_pairs = NULL,
  cindex_method = "harrell",
  or_age = NULL,
  or_numerator = NULL,
  or_denominator = NULL,
  or_pairs = NULL,
  bootstrap = FALSE,
  n_boot = 999L,
  ci_level = 0.95,
  boot_method = "percentile",
  seed = NULL,
  parallel = "no",
  n_cores = 1L,
  strata = NULL
)
```

## Arguments

- data:

  a data.frame containing the columns specified by `phs`, `time`, and
  `event`

- phs:

  a string specifying the column name containing the polygenic hazard
  score

- time:

  a string specifying the column name containing the time to event or
  censoring

- event:

  a string specifying the column name containing the event indicator (0
  = censored, 1 = event)

- metrics:

  a character vector specifying which metrics to compute. Options are
  `"HR"`, `"C_index"`, `"HR_SD"`, `"OR"`.

- hr_method:

  the method to use for HR calculation. One of `"continuous_group"`
  (default), `"continuous_point"`, or `"categorical"`. Only
  `"continuous_group"` is currently implemented.

- hr_numerator:

  numeric. The lower boundary of the numerator group as a percentile
  (e.g., 0.80 for the top 20%). The numerator group is defined as
  \[hr_numerator, 1.0\]. Ignored if `hr_pairs` is provided. Default is
  0.80.

- hr_denominator:

  numeric. The upper boundary of the denominator group as a percentile
  (e.g., 0.20 for the bottom 20%). The denominator group is defined as
  \[0.0, hr_denominator\]. Ignored if `hr_pairs` is provided. Default is
  0.20.

- hr_pairs:

  a list of HR specifications for computing multiple HRs in one call.
  Each element should be a named list with `numerator` and
  `denominator`, each a length-2 numeric vector specifying the \[lower,
  upper\] percentile boundaries of each band. Cannot be combined with
  `hr_numerator`/`hr_denominator`. Example:

      hr_pairs = list(
          list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
          list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
        )

- cindex_method:

  the method to use for C-index calculation. One of `"harrell"`
  (default) or `"uno"` (not yet implemented).

- or_age:

  an integer or numeric vector specifying the age(s) at which the odds
  ratio should be calculated. Required when `"OR"` is in `metrics`. One
  row is returned per age.

- or_numerator:

  numeric. Lower boundary of the numerator band for OR calculation.
  Default is 0.80.

- or_denominator:

  numeric. Upper boundary of the denominator band for OR calculation.
  Default is 0.20.

- or_pairs:

  Same structure as `hr_pairs` but for OR calculation.

- bootstrap:

  logical. Whether to compute bootstrapped confidence intervals. Default
  is `FALSE`. When `TRUE`, `conf_low`, `conf_high`, and `se` are
  populated in the returned tibble.

- n_boot:

  integer. Number of bootstrap replicates. Default is 1000.

- ci_level:

  numeric. Confidence level for bootstrap CIs. Default is 0.95.

- boot_method:

  character. Method for deriving CIs from the bootstrap distribution.
  One of `"percentile"` (default), `"bca"` (bias-corrected accelerated),
  or `"normal"`.

- seed:

  optional integer. Random seed for reproducibility.

- parallel:

  character. Parallelisation backend passed to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). One of
  `"no"` (default), `"multicore"`, or `"snow"`. Only `"no"` is currently
  implemented.

- n_cores:

  integer. Number of cores for parallel bootstrapping. Passed as `ncpus`
  to [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Default
  is 1L.

- strata:

  optional string. Column name to stratify resampling on (preserves
  case/control ratio within each level). Default is `NULL`.

## Value

a tibble with one row per metric and columns:

- metric:

  Full explicit metric name, e.g. `"HR[80-100]_[0-20]"`, `"C_index"`,
  `"OR[80-100]_[0-20]_age70"`

- estimate:

  Point estimate computed on the full dataset

- conf_low:

  Lower CI bound (`NA` if `bootstrap = FALSE`)

- conf_high:

  Upper CI bound (`NA` if `bootstrap = FALSE`)

- se:

  Bootstrap standard error (`NA` if `bootstrap = FALSE`)

- n_numerator:

  Sample size in the numerator group (HR and OR only)

- n_denominator:

  Sample size in the denominator group (HR and OR only)

- method:

  Method flag used, e.g. `"continuous_group"`, `"harrell"`; `NA` for OR

- adjusted:

  Whether covariates were used (always `FALSE` until covariate support
  is added)

## Details

### HR Arguments

`hr_numerator` and `hr_denominator` are convenience arguments for the
common case of a single HR comparing the top and bottom of the PHS
distribution. For multiple HRs or non-standard reference bands (e.g., a
middle reference group), use `hr_pairs` instead. Providing both will
raise an error.

### Bootstrapping

When `bootstrap = TRUE`, `phs_metrics()` calls
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) internally. On
each replicate the Cox model is re-fitted on the resampled data so that
all metrics derived from it (HR, HR_SD, C-index) are jointly consistent.
Failures in individual replicates (e.g., degenerate resamples) produce
`NA` for that replicate and are excluded from the SE/CI calculation with
a warning.

## Examples

``` r
# Simple case – HR[80-100]_[0-20]
phs_metrics(test_data, metrics = "HR", hr_numerator = 0.80, hr_denominator = 0.20)
#> # A tibble: 1 × 9
#>   metric      estimate conf_low conf_high    se n_numerator n_denominator method
#>   <chr>          <dbl>    <dbl>     <dbl> <dbl>       <int>         <int> <chr> 
#> 1 HR[80-100]…     8.17       NA        NA    NA         200           200 conti…
#> # ℹ 1 more variable: adjusted <lgl>

# Multiple HRs with custom bands
phs_metrics(test_data, metrics = "HR",
  hr_pairs = list(
    list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
    list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
  ))
#> # A tibble: 2 × 9
#>   metric      estimate conf_low conf_high    se n_numerator n_denominator method
#>   <chr>          <dbl>    <dbl>     <dbl> <dbl>       <int>         <int> <chr> 
#> 1 HR[80-100]…     8.17       NA        NA    NA         200           200 conti…
#> 2 HR[80-100]…     3.05       NA        NA    NA         200           200 conti…
#> # ℹ 1 more variable: adjusted <lgl>

# With bootstrapped CIs
phs_metrics(test_data, metrics = c("HR", "C_index"),
  bootstrap = TRUE, n_boot = 500, seed = 42)
#> # A tibble: 2 × 9
#>   metric     estimate conf_low conf_high     se n_numerator n_denominator method
#>   <chr>         <dbl>    <dbl>     <dbl>  <dbl>       <int>         <int> <chr> 
#> 1 HR[80-100…    8.17     5.95     11.4   1.36           200           200 conti…
#> 2 C_index       0.708    0.680     0.734 0.0136          NA            NA harre…
#> # ℹ 1 more variable: adjusted <lgl>
```

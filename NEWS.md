# hazrd 0.2.0

## New functions

- `phs_metrics()` — single entry point for all discrimination metrics (HR,
  C-index, OR, HR_SD). Returns a tidy tibble with one row per metric.
  Supports built-in bootstrapping via `boot::boot()` with `"percentile"`,
  `"bca"`, and `"normal"` CI methods.
- `phs_km_curve()` — Kaplan-Meier curves stratified by PHS percentile group.
  Returns a `ggplot` object by default (`output = "plot"`) or a tidy data
  frame (`output = "data"`). Accepts an `intervals` list for overlapping
  strata (e.g. 95–100 %, 80–100 %) or a `breaks` vector for exclusive bins.
  Supports a `ref_data` argument for train/validation percentile workflows.
- `phs_cox_curve()` — Cox model-based survival curves at specified PHS
  percentiles. Fits a Cox PH model with `phs` as the sole predictor and
  returns smooth predicted curves at user-supplied percentile values.
  Supports `ref_data` for train/validation workflows.

## Deprecated functions

The following functions are deprecated as of 0.2.0 and will be removed in
0.3.0. They continue to work and emit a deprecation warning pointing to the
replacement call.

- `get_hr()` → use `phs_metrics(data, metrics = "HR")`
- `get_or()` → use `phs_metrics(data, metrics = "OR", or_age = ...)`
- `get_cindex()` → use `phs_metrics(data, metrics = "C_index")`
- `get_hrsd()` → use `phs_metrics(data, metrics = "HR_SD")`
- `km_curve()` → use `phs_km_curve()`

## Removed functions

- `create_lookup_table()` — superseded by the upcoming `phs_abs_risk()`.
- `cum_inc()` — out of scope for the package API.
- `get_phs_age()` — internal helper; will be re-exposed via `phs_abs_risk()`.

## Bug fixes

- `phs_metrics()` with `metrics = "OR"`: `or_age` values outside the observed
  KM time range now return `NA` with an informative warning instead of
  propagating silent `NA` or infinite odds (#or-edge-case).
- `phs_metrics()` with `metrics = "HR_SD"` and `bootstrap = TRUE`: HR_SD is
  now correctly included in the bootstrap statistic vector so that `conf_low`,
  `conf_high`, and `se` are populated.

# hazrd 0.1.0

Initial release.

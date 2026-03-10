# Changelog

## hazrd 0.2.0

### New functions

- [`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
  — single entry point for all discrimination metrics (HR, C-index, OR,
  HR_SD). Returns a tidy tibble with one row per metric. Supports
  built-in bootstrapping via
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) with
  `"percentile"`, `"bca"`, and `"normal"` CI methods.
- `phs_km()` — Kaplan-Meier curves stratified by PHS percentile group.
  Returns a `ggplot` object by default (`output = "plot"`) or a tidy
  data frame (`output = "data"`). Includes seven stratification presets
  (`quantile_3`, `quantile_4`, `quantile_5`, `quantile_10`, `clinical`,
  `binary_80_20`, `binary_top_bottom`) and a `custom_cuts` override.
  Supports a `ref_data` argument for train/validation percentile
  workflows.

### Deprecated functions

The following functions are deprecated as of 0.2.0 and will be removed
in 0.3.0. They continue to work and emit a deprecation warning pointing
to the replacement call.

- [`get_hr()`](https://amorris28.github.io/hazrd/reference/get_hr.md) →
  use `phs_metrics(data, metrics = "HR")`
- [`get_or()`](https://amorris28.github.io/hazrd/reference/get_or.md) →
  use `phs_metrics(data, metrics = "OR", or_age = ...)`
- [`get_cindex()`](https://amorris28.github.io/hazrd/reference/get_cindex.md)
  → use `phs_metrics(data, metrics = "C_index")`
- [`get_hrsd()`](https://amorris28.github.io/hazrd/reference/get_hrsd.md)
  → use `phs_metrics(data, metrics = "HR_SD")`
- [`km_curve()`](https://amorris28.github.io/hazrd/reference/km_curve.md)
  → use `phs_km()`

### Removed functions

- `create_lookup_table()` — superseded by the upcoming `phs_abs_risk()`.
- `cum_inc()` — out of scope for the package API.
- `get_phs_age()` — internal helper; will be re-exposed via
  `phs_abs_risk()`.

### Bug fixes

- [`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
  with `metrics = "OR"`: `or_age` values outside the observed KM time
  range now return `NA` with an informative warning instead of
  propagating silent `NA` or infinite odds (#or-edge-case).
- [`phs_metrics()`](https://amorris28.github.io/hazrd/reference/phs_metrics.md)
  with `metrics = "HR_SD"` and `bootstrap = TRUE`: HR_SD is now
  correctly included in the bootstrap statistic vector so that
  `conf_low`, `conf_high`, and `se` are populated.

## hazrd 0.1.0

Initial release.

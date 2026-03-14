# hazrd 0.2.2

## New features

- New `phs_abs_risk()` function — cumulative incidence (absolute risk) curves
  stratified by PHS percentile group. Supports `method = "km"` (1 - KM
  estimate) and `method = "aalen_johansen"` (Aalen-Johansen competing-risk
  estimator). Returns either a ggplot object or a tidy data frame. Addresses
  issue #20.
  - `strata` argument controls percentile grouping via named presets
    (`"quantile_2"` through `"quantile_5"`, `"classic"`); `custom_cuts`
    accepts an arbitrary numeric vector of cutpoints.
  - When `output = "data"` and `time_points` is supplied, the returned tibble
    has one row per stratum x time_point.
  - `method = "fine_gray"` is stubbed and will error with an informative
    message until implemented.

- `phs_km_curve()` now implements the `risk_table` argument — when
  `risk_table = TRUE` and `output = "plot"`, a numbers-at-risk strip is
  attached below the curve using the **patchwork** package (added to
  `Suggests`). Falls back to a warning and the plain plot when patchwork is
  not installed. Addresses issue #49.

- `phs_abs_risk()` also supports `risk_table = TRUE` with identical behavior
  to `phs_km_curve()`.

# hazrd 0.2.1

## New features

- `phs_km_curve()`: new `intervals` parameter accepts a list of `c(lo, hi)`
  percentile pairs, enabling overlapping KM strata (e.g. top 5%, top 20%,
  bottom 20%). Default produces four bands: 0.95-1, 0.80-1, 0.30-0.70,
  and 0-0.20. The legacy `breaks` parameter is retained for backward
  compatibility.

## Bug fixes

- `phs_km_curve()`: fixed malformed `man/phs_km_curve.Rd` caused by percent
  signs in roxygen `@param` text corrupting the `\arguments{}` block and
  producing R CMD check warnings.

## Documentation

- Vignette updated with overlapping intervals examples and `breaks` legacy usage.
- README installation section updated to use `devtools::install_github()`;
  dev-branch instructions added (`@dev`).
- `NEWS.md` corrected: function name `phs_km` -> `phs_km_curve`, stale preset
  list removed, `phs_cox_curve` entry added.

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

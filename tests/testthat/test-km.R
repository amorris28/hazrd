# ── phs_km_curve() ─────────────────────────────────────────────────────────────────────────────

test_that("phs_km_curve returns a ggplot by default", {
  p <- phs_km_curve(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("phs_km_curve output='data' returns a data.frame with required columns", {
  result <- phs_km_curve(test_data, output = "data")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "estimate", "conf.low", "conf.high", "stratum") %in% names(result)))
})

test_that("phs_km_curve default intervals produce exactly 4 strata levels", {
  result <- phs_km_curve(test_data, output = "data")
  expect_equal(length(unique(result$stratum)), 4L)
})

test_that("phs_km_curve default interval labels match expected order", {
  result <- phs_km_curve(test_data, output = "data")
  expect_equal(levels(result$stratum), c("95-100%", "80-100%", "30-70%", "0-20%"))
})

test_that("phs_km_curve overlapping intervals produce correct subset counts", {
  result <- phs_km_curve(test_data,
                         intervals = list(c(0.80, 1), c(0, 0.20)),
                         output = "data")
  expect_equal(levels(result$stratum), c("80-100%", "0-20%"))
})

test_that("phs_km_curve estimate and CI columns are clamped to [0, 1]", {
  result <- phs_km_curve(test_data, output = "data")
  expect_true(all(result$estimate >= 0 & result$estimate <= 1, na.rm = TRUE))
  expect_true(all(result$conf.low  >= 0 & result$conf.low  <= 1, na.rm = TRUE))
  expect_true(all(result$conf.high >= 0 & result$conf.high <= 1, na.rm = TRUE))
})

test_that("phs_km_curve breaks= (legacy) produces correct number of strata", {
  result <- phs_km_curve(test_data, intervals = NULL, breaks = c(0.33, 0.67), output = "data")
  expect_equal(length(unique(result$stratum)), 3L)
})

test_that("phs_km_curve 3 numeric breaks produce 4 strata", {
  result <- phs_km_curve(test_data, intervals = NULL, breaks = c(0.25, 0.50, 0.75), output = "data")
  expect_equal(length(unique(result$stratum)), 4L)
})

test_that("phs_km_curve 4 numeric breaks produce 5 strata", {
  result <- phs_km_curve(test_data, intervals = NULL, breaks = c(0.20, 0.40, 0.60, 0.80), output = "data")
  expect_equal(length(unique(result$stratum)), 5L)
})

test_that("phs_km_curve conf_int=FALSE omits ribbon layer from plot", {
  p_with    <- phs_km_curve(test_data, conf_int = TRUE)
  p_without <- phs_km_curve(test_data, conf_int = FALSE)
  layers_with    <- sapply(p_with$layers,    function(l) class(l$geom)[1])
  layers_without <- sapply(p_without$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layers_with)
  expect_false("GeomRibbon" %in% layers_without)
})

test_that("phs_km_curve ref_data shifts stratum cutpoints relative to reference population", {
  ref <- test_data[test_data$phs <= median(test_data$phs), ]
  result_ref  <- phs_km_curve(test_data, ref_data = ref, output = "data")
  result_self <- phs_km_curve(test_data, output = "data")
  # Stratum assignments differ when reference population differs from data
  expect_false(identical(as.character(result_ref$stratum), as.character(result_self$stratum)))
})


# ── km_curve() deprecation wrapper ────────────────────────────────────────────

test_that("km_curve emits a deprecation warning", {
  expect_warning(km_curve(test_data), regexp = "deprecated")
})

test_that("km_curve returns a data.frame with expected columns", {
  result <- suppressWarnings(km_curve(test_data))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "estimate", "conf.low", "conf.high", "stratum") %in% names(result)))
})

test_that("km_curve scale=TRUE warns about unsupported argument", {
  suppressWarnings(expect_warning(
    km_curve(test_data, scale = TRUE),
    regexp = "scale"
  ))
})

test_that("km_curve inverse=TRUE warns about unsupported argument", {
  suppressWarnings(expect_warning(
    km_curve(test_data, inverse = TRUE),
    regexp = "inverse"
  ))
})

test_that("km_curve interval != c(0,1) warns about unsupported argument", {
  suppressWarnings(expect_warning(
    km_curve(test_data, interval = c(0.2, 0.8)),
    regexp = "interval"
  ))
})


# ── phs_cox_curve() ────────────────────────────────────────────────────────────

test_that("phs_cox_curve returns a ggplot by default", {
  p <- phs_cox_curve(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("phs_cox_curve output='data' returns a data.frame with required columns", {
  result <- phs_cox_curve(test_data, output = "data")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "estimate", "conf.low", "conf.high", "percentile") %in% names(result)))
})

test_that("phs_cox_curve produces one curve per percentile", {
  pcts <- c(0.20, 0.50, 0.80)
  result <- phs_cox_curve(test_data, percentiles = pcts, output = "data")
  expect_equal(length(unique(result$percentile)), 3L)
})

test_that("phs_cox_curve estimate and CI columns are clamped to [0, 1]", {
  result <- phs_cox_curve(test_data, output = "data")
  expect_true(all(result$estimate >= 0 & result$estimate <= 1, na.rm = TRUE))
  expect_true(all(result$conf.low  >= 0 & result$conf.low  <= 1, na.rm = TRUE))
  expect_true(all(result$conf.high >= 0 & result$conf.high <= 1, na.rm = TRUE))
})

test_that("phs_cox_curve conf_int=FALSE omits ribbon layer from plot", {
  p_with    <- phs_cox_curve(test_data, conf_int = TRUE)
  p_without <- phs_cox_curve(test_data, conf_int = FALSE)
  layers_with    <- sapply(p_with$layers,    function(l) class(l$geom)[1])
  layers_without <- sapply(p_without$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layers_with)
  expect_false("GeomRibbon" %in% layers_without)
})


# ── phs_km_curve() risk table ─────────────────────────────────────────────────

test_that("phs_km_curve risk_table=FALSE returns a plain ggplot", {
  p <- phs_km_curve(test_data, risk_table = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("phs_km_curve risk_table=TRUE without patchwork warns and returns ggplot", {
  skip_if(requireNamespace("patchwork", quietly = TRUE),
          "patchwork is installed; skipping no-patchwork fallback test")
  expect_warning(
    result <- phs_km_curve(test_data, risk_table = TRUE),
    regexp = "patchwork"
  )
  expect_s3_class(result, "ggplot")
})

test_that("phs_km_curve risk_table=TRUE with patchwork returns a patchwork object", {
  skip_if_not_installed("patchwork")
  result <- phs_km_curve(test_data, risk_table = TRUE)
  expect_true(inherits(result, "patchwork"))
})

test_that("phs_km_curve risk_table=TRUE does not affect output='data'", {
  result <- phs_km_curve(test_data, risk_table = TRUE, output = "data")
  expect_s3_class(result, "data.frame")
  expect_true("n.risk" %in% names(result))
})

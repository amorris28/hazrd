# ── phs_abs_risk() ────────────────────────────────────────────────────────────

test_that("phs_abs_risk returns a ggplot by default", {
  p <- phs_abs_risk(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("phs_abs_risk output='data' returns a data.frame with required columns", {
  result <- phs_abs_risk(test_data, output = "data")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "stratum",
                    "risk_estimate", "conf.low", "conf.high") %in% names(result)))
  expect_false("stratum_label" %in% names(result))
})

test_that("phs_abs_risk default intervals produce 4 strata levels", {
  result <- phs_abs_risk(test_data, output = "data")
  expect_equal(length(unique(result$stratum)), 4L)
})

test_that("phs_abs_risk risk_estimate is in [0, 1]", {
  result <- phs_abs_risk(test_data, output = "data")
  expect_true(all(result$risk_estimate >= 0 & result$risk_estimate <= 1, na.rm = TRUE))
})

test_that("phs_abs_risk risk_estimate is monotonically non-decreasing over time", {
  result <- phs_abs_risk(test_data, output = "data")
  for (s in unique(result$stratum)) {
    sub <- result[result$stratum == s, ]
    sub <- sub[order(sub$time), ]
    expect_true(all(diff(sub$risk_estimate) >= -1e-10),
                label = paste0("stratum ", s, " is non-decreasing"))
  }
})

test_that("phs_abs_risk time_points returns one row per stratum per time point", {
  tps    <- c(60, 70, 80)
  result <- phs_abs_risk(test_data, time_points = tps, output = "data")
  expect_equal(nrow(result), 4L * length(tps))
})

test_that("phs_abs_risk time_points output has correct time values", {
  tps    <- c(65, 75)
  result <- phs_abs_risk(test_data, time_points = tps, output = "data")
  expect_equal(sort(unique(result$time)), sort(tps))
})

test_that("phs_abs_risk stratum column uses percentile-range labels", {
  result <- phs_abs_risk(test_data, output = "data")
  labels <- as.character(unique(result$stratum))
  expect_true(any(grepl("0-", labels)))
  expect_true(any(grepl("100%", labels)))
})

test_that("phs_abs_risk conf_int=FALSE omits ribbon layer from plot", {
  p_with    <- phs_abs_risk(test_data, conf_int = TRUE)
  p_without <- phs_abs_risk(test_data, conf_int = FALSE)
  layers_w  <- sapply(p_with$layers,    function(l) class(l$geom)[1])
  layers_wo <- sapply(p_without$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layers_w)
  expect_false("GeomRibbon" %in% layers_wo)
})

test_that("phs_abs_risk default interval labels match phs_km_curve defaults", {
  result <- phs_abs_risk(test_data, output = "data")
  expect_equal(levels(result$stratum), c("95-100%", "80-100%", "30-70%", "0-20%"))
})

test_that("phs_abs_risk intervals= list works explicitly", {
  result <- phs_abs_risk(test_data,
                         intervals = list(c(0.80, 1), c(0, 0.20)),
                         output = "data")
  expect_equal(levels(result$stratum), c("80-100%", "0-20%"))
})

test_that("phs_abs_risk invalid intervals list errors informatively", {
  expect_error(phs_abs_risk(test_data, intervals = list(c(1.5, 2))),
               regexp = "intervals")
})

test_that("phs_abs_risk method='fine_gray' errors as not yet implemented", {
  expect_error(phs_abs_risk(test_data, method = "fine_gray"),
               regexp = "not yet implemented")
})

test_that("phs_abs_risk method='aalen_johansen' without competing_event errors", {
  expect_error(phs_abs_risk(test_data, method = "aalen_johansen"),
               regexp = "competing_event")
})

test_that("phs_abs_risk method='aalen_johansen' with competing event works", {
  set.seed(42)
  td <- test_data
  td$comp_event <- ifelse(td$status == 0L & runif(nrow(td)) < 0.3, 1L, 0L)
  result <- phs_abs_risk(td, competing_event = "comp_event",
                         method = "aalen_johansen", output = "data")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "stratum", "risk_estimate") %in% names(result)))
  expect_true(all(result$risk_estimate >= 0 & result$risk_estimate <= 1, na.rm = TRUE))
})

test_that("phs_abs_risk ref_data shifts stratum cutpoints relative to reference", {
  ref         <- test_data[test_data$phs <= median(test_data$phs), ]
  result_ref  <- phs_abs_risk(test_data, ref_data = ref,  output = "data")
  result_self <- phs_abs_risk(test_data,                  output = "data")
  # Sizes differ because different percentile thresholds select different subsets
  expect_false(nrow(result_ref) == nrow(result_self) &&
               isTRUE(all.equal(result_ref$risk_estimate,
                                result_self$risk_estimate)))
})

test_that("phs_abs_risk missing column in data errors informatively", {
  expect_error(phs_abs_risk(test_data, phs = "nonexistent"),
               regexp = "not found in data")
})

# ── phs_risk_table() ─────────────────────────────────────────────────────────

test_that("phs_risk_table returns a ggplot", {
  rt <- phs_risk_table(test_data)
  expect_s3_class(rt, "ggplot")
})

test_that("phs_risk_table respects intervals argument", {
  rt <- phs_risk_table(test_data, intervals = list(c(0.80, 1), c(0, 0.20)))
  expect_s3_class(rt, "ggplot")
})

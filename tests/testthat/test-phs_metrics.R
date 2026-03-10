# ── Regression anchors ────────────────────────────────────────────────────────

test_that("phs_metrics HR continuous_group matches known value", {
  result <- phs_metrics(test_data, metrics = "HR", hr_method = "continuous_group")
  expect_equal(result$estimate, 8.170775, tolerance = 0.001)
})

test_that("phs_metrics Harrell's C-index matches known value", {
  result <- phs_metrics(test_data, metrics = "C_index", cindex_method = "harrell")
  expect_equal(result$estimate, 0.7082444, tolerance = 0.001)
})


# ── Output structure ──────────────────────────────────────────────────────────

test_that("phs_metrics returns a tibble", {
  result <- phs_metrics(test_data)
  expect_s3_class(result, "tbl_df")
})

test_that("phs_metrics output has the required columns in canonical order", {
  canonical <- c("metric", "estimate", "conf_low", "conf_high", "se",
                 "n_numerator", "n_denominator", "method", "adjusted")

  # Column names and order must be identical whether or not bootstrap is run
  result_plain <- phs_metrics(test_data)
  expect_named(result_plain, canonical, ignore.order = FALSE)

  result_boot <- phs_metrics(test_data, bootstrap = TRUE, n_boot = 50, seed = 1)
  expect_named(result_boot, canonical, ignore.order = FALSE)
})

test_that("phs_metrics row count: one row per default pair, more for multiple pairs/ages", {
  # Single default HR + C_index = 2 rows
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"))
  expect_equal(nrow(result), 2L)

  # Two hr_pairs = 2 rows
  pairs <- list(
    list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
    list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
  )
  result_multi_hr <- phs_metrics(test_data, metrics = "HR", hr_pairs = pairs)
  expect_equal(nrow(result_multi_hr), 2L)

  # OR with 3 ages = 3 rows
  result_or <- phs_metrics(test_data, metrics = "OR", or_age = c(60, 70, 80))
  expect_equal(nrow(result_or), 3L)

  # OR with 2 pairs x 2 ages = 4 rows
  or_pairs <- list(
    list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
    list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
  )
  result_or_multi <- phs_metrics(test_data, metrics = "OR",
                                  or_pairs = or_pairs, or_age = c(60, 70))
  expect_equal(nrow(result_or_multi), 4L)
})

test_that("n_numerator and n_denominator are NA for C_index, populated for HR", {
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"))
  hr_row <- result[result$metric != "C_index", ]
  ci_row <- result[result$metric == "C_index", ]

  expect_true(is.na(ci_row$n_numerator))
  expect_true(is.na(ci_row$n_denominator))
  expect_false(is.na(hr_row$n_numerator))
  expect_false(is.na(hr_row$n_denominator))
})

test_that("adjusted column is always FALSE (until covariate support added)", {
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"))
  expect_true(all(result$adjusted == FALSE))
})

test_that("conf_low, conf_high, se are NA when bootstrap = FALSE", {
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"))
  expect_true(all(is.na(result$conf_low)))
  expect_true(all(is.na(result$conf_high)))
  expect_true(all(is.na(result$se)))
})


# ── HR: metric naming ─────────────────────────────────────────────────────────

test_that("default HR produces metric name HR[80-100]_[0-20]", {
  result <- phs_metrics(test_data, metrics = "HR")
  expect_equal(result$metric, "HR[80-100]_[0-20]")
})

test_that("custom hr_numerator / hr_denominator reflected in metric name", {
  result <- phs_metrics(test_data, metrics = "HR",
                        hr_numerator = 0.90, hr_denominator = 0.10)
  expect_equal(result$metric, "HR[90-100]_[0-10]")
})

test_that("hr_pairs produces one row per pair with correct metric names", {
  pairs <- list(
    list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
    list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
  )
  result <- phs_metrics(test_data, metrics = "HR", hr_pairs = pairs)
  expect_equal(nrow(result), 2L)
  expect_equal(result$metric,
               c("HR[80-100]_[0-20]", "HR[80-100]_[40-60]"))
})

test_that("hr_pairs and hr_numerator together raises an error", {
  expect_error(
    phs_metrics(test_data, metrics = "HR",
                hr_numerator = 0.80,
                hr_pairs = list(list(numerator = c(0.80, 1.00),
                                     denominator = c(0.00, 0.20)))),
    regexp = "not both"
  )
})


# ── HR: monotonicity ──────────────────────────────────────────────────────────

test_that("HR estimate is greater than 1 when top > bottom of PHS dist", {
  result <- phs_metrics(test_data, metrics = "HR")
  expect_gt(result$estimate, 1)
})

test_that("narrower bands (90/10) yield higher HR than wider bands (80/20)", {
  r_wide   <- phs_metrics(test_data, metrics = "HR",
                           hr_numerator = 0.80, hr_denominator = 0.20)
  r_narrow <- phs_metrics(test_data, metrics = "HR",
                           hr_numerator = 0.90, hr_denominator = 0.10)
  expect_gt(r_narrow$estimate, r_wide$estimate)
})

test_that("HR is ~1 when numerator and denominator bands are the same", {
  result <- phs_metrics(test_data, metrics = "HR",
                        hr_numerator = 0.40, hr_denominator = 0.60)
  # numerator = [40,100], denominator = [0,60]: heavily overlapping, HR near 1
  # A looser test — just check it's plausibly close to 1 (within an order of
  # magnitude), not the exact degenerate case of identical bands
  result_same <- phs_metrics(
    test_data, metrics = "HR",
    hr_pairs = list(list(numerator   = c(0.40, 0.60),
                         denominator = c(0.40, 0.60)))
  )
  expect_equal(result_same$estimate, 1.0, tolerance = 0.01)
})


# ── C-index: sanity ───────────────────────────────────────────────────────────

test_that("C-index is between 0.5 and 1 for an informative PHS", {
  result <- phs_metrics(test_data, metrics = "C_index")
  expect_gt(result$estimate, 0.5)
  expect_lt(result$estimate, 1.0)
})

test_that("C-index method column equals the method used", {
  result <- phs_metrics(test_data, metrics = "C_index",
                        cindex_method = "harrell")
  expect_equal(result$method, "harrell")
})


# ── OR ────────────────────────────────────────────────────────────────────────

test_that("OR requires or_age when 'OR' is in metrics", {
  expect_error(
    phs_metrics(test_data, metrics = "OR"),
    regexp = "or_age"
  )
})

test_that("OR metric name includes band boundaries and age", {
  result <- phs_metrics(test_data, metrics = "OR", or_age = 70)
  expect_equal(result$metric, "OR[80-100]_[0-20]_age70")
})

test_that("OR returns one row per age in or_age vector", {
  result <- phs_metrics(test_data, metrics = "OR", or_age = c(60, 70, 80))
  expect_equal(nrow(result), 3L)
  expect_true(all(c("OR[80-100]_[0-20]_age60",
                    "OR[80-100]_[0-20]_age70",
                    "OR[80-100]_[0-20]_age80") %in% result$metric))
})

test_that("OR estimate is > 1 for top vs bottom band at a plausible age", {
  result <- phs_metrics(test_data, metrics = "OR", or_age = 70)
  expect_gt(result$estimate, 1)
})

test_that("out-of-range or_age returns NA estimate with a warning", {
  expect_warning(
    result <- phs_metrics(test_data, metrics = "OR", or_age = 999),
    regexp = "outside"
  )
  expect_true(is.na(result$estimate))
})

test_that("bootstrap handles out-of-range or_age without error and returns NA", {
  expect_warning(
    result <- phs_metrics(test_data, metrics = "OR", or_age = 999,
                          bootstrap = TRUE, n_boot = 10, seed = 1),
    regexp = "outside"
  )
  expect_true(is.na(result$estimate))
  expect_true(all(is.na(result$conf_low)))
  expect_true(all(is.na(result$conf_high)))
  expect_true(all(is.na(result$se)))
})

test_that("or_pairs and or_numerator together raises an error", {
  expect_error(
    phs_metrics(test_data, metrics = "OR",
                or_age = 70,
                or_numerator = 0.80,
                or_pairs = list(list(numerator   = c(0.80, 1.00),
                                     denominator = c(0.00, 0.20)))),
    regexp = "not both"
  )
})


# ── Input validation ──────────────────────────────────────────────────────────

test_that("phs_metrics errors on missing column", {
  expect_error(phs_metrics(test_data, phs = "nonexistent"))
})

test_that("unrecognised metric name errors with informative message", {
  expect_error(
    phs_metrics(test_data, metrics = c("HR", "cindex")),
    regexp = "Unknown metric"
  )
  expect_error(
    phs_metrics(test_data, metrics = "completely_wrong"),
    regexp = "Valid options are"
  )
  # Multiple bad names should all appear in the error message
  expect_error(
    phs_metrics(test_data, metrics = c("cindex", "hr")),
    regexp = "cindex"
  )
})

test_that("phs_metrics HR_SD matches known value", {
  result <- phs_metrics(test_data, metrics = "HR_SD")
  # exp(beta * sd(phs)) — verify against previously confirmed value
  expected <- as.numeric(exp(coef(survival::coxph(survival::Surv(test_data$age, test_data$status) ~ test_data$phs)) * sd(test_data$phs)))
  expect_equal(result$estimate, expected, tolerance = 0.001)
})

test_that("HR_SD metric name is 'HR_SD'", {
  result <- phs_metrics(test_data, metrics = "HR_SD")
  expect_equal(result$metric, "HR_SD")
})

test_that("HR_SD estimate is > 1 for an informative PHS", {
  result <- phs_metrics(test_data, metrics = "HR_SD")
  expect_gt(result$estimate, 1)
})

test_that("HR_SD n_numerator and n_denominator are NA", {
  result <- phs_metrics(test_data, metrics = "HR_SD")
  expect_true(is.na(result$n_numerator))
  expect_true(is.na(result$n_denominator))
})
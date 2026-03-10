test_that("bootstrap = FALSE returns NA for conf_low, conf_high, se", {
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"),
                        bootstrap = FALSE)
  expect_true(all(is.na(result$conf_low)))
  expect_true(all(is.na(result$conf_high)))
  expect_true(all(is.na(result$se)))
})


test_that("bootstrap fills conf_low, conf_high, se for HR and C_index", {
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"),
                        bootstrap = TRUE, n_boot = 50, seed = 1)

  expect_false(any(is.na(result$conf_low)))
  expect_false(any(is.na(result$conf_high)))
  expect_false(any(is.na(result$se)))
})


test_that("bootstrap CIs are ordered (conf_low < estimate < conf_high)", {
  result <- phs_metrics(test_data, metrics = c("HR", "C_index"),
                        bootstrap = TRUE, n_boot = 50, seed = 1)

  expect_true(all(result$conf_low  < result$estimate,  na.rm = TRUE))
  expect_true(all(result$conf_high > result$estimate,  na.rm = TRUE))
})


test_that("bootstrap SE is positive", {
  result <- phs_metrics(test_data, metrics = "C_index",
                        bootstrap = TRUE, n_boot = 50, seed = 1)

  expect_true(all(result$se > 0, na.rm = TRUE))
})


test_that("seed makes bootstrap reproducible", {
  r1 <- phs_metrics(test_data, metrics = "HR",
                    bootstrap = TRUE, n_boot = 50, seed = 42)
  r2 <- phs_metrics(test_data, metrics = "HR",
                    bootstrap = TRUE, n_boot = 50, seed = 42)

  expect_equal(r1$conf_low,  r2$conf_low)
  expect_equal(r1$conf_high, r2$conf_high)
  expect_equal(r1$se,        r2$se)
})


test_that("different seeds produce different CIs", {
  r1 <- phs_metrics(test_data, metrics = "HR",
                    bootstrap = TRUE, n_boot = 50, seed = 1)
  r2 <- phs_metrics(test_data, metrics = "HR",
                    bootstrap = TRUE, n_boot = 50, seed = 99)

  # It's astronomically unlikely both CIs are identical
  expect_false(identical(r1$conf_low, r2$conf_low))
})


test_that("bootstrap point estimates match non-bootstrap estimates", {
  r_plain <- phs_metrics(test_data, metrics = c("HR", "C_index"),
                         bootstrap = FALSE)
  r_boot  <- phs_metrics(test_data, metrics = c("HR", "C_index"),
                         bootstrap = TRUE, n_boot = 50, seed = 1)

  # Point estimates must not be altered by bootstrapping
  expect_equal(r_plain$estimate, r_boot$estimate)
  expect_equal(r_plain$metric,   r_boot$metric)
})


test_that("bootstrap works with multiple hr_pairs", {
  pairs <- list(
    list(numerator = c(0.80, 1.00), denominator = c(0.00, 0.20)),
    list(numerator = c(0.80, 1.00), denominator = c(0.40, 0.60))
  )
  result <- phs_metrics(test_data, metrics = "HR", hr_pairs = pairs,
                        bootstrap = TRUE, n_boot = 50, seed = 1)

  expect_equal(nrow(result), 2L)
  expect_false(any(is.na(result$conf_low)))
  expect_false(any(is.na(result$conf_high)))
})


test_that("bootstrap works with OR and multiple or_age values", {
  result <- phs_metrics(test_data, metrics = "OR",
                        or_age = c(60, 70),
                        bootstrap = TRUE, n_boot = 50, seed = 1)

  expect_equal(nrow(result), 2L)
  expect_false(any(is.na(result$conf_low)))
  expect_false(any(is.na(result$conf_high)))
})


test_that("boot_method = 'bca' returns numeric CIs (finite or NA, not error)", {
  # BCA requires a stable jackknife acceleration estimate; with small n_boot it
  # can legitimately return NA on some seeds.  We verify the call succeeds and
  # that the output columns are numeric — not that they are finite.
  result <- phs_metrics(test_data, metrics = "C_index",
                        bootstrap = TRUE, n_boot = 200, seed = 7,
                        boot_method = "bca")

  expect_s3_class(result, "tbl_df")
  expect_true(is.numeric(result$conf_low))
  expect_true(is.numeric(result$conf_high))
  # SE is derived from sd() of replicates and should always be finite
  expect_true(all(is.finite(result$se)))
})


test_that("boot_method = 'normal' returns finite CIs", {
  result <- phs_metrics(test_data, metrics = "C_index",
                        bootstrap = TRUE, n_boot = 50, seed = 1,
                        boot_method = "normal")

  expect_false(any(is.na(result$conf_low)))
  expect_false(any(is.na(result$conf_high)))
})


test_that("ci_level controls CI width (95% wider than 50%)", {
  r95 <- phs_metrics(test_data, metrics = "C_index",
                     bootstrap = TRUE, n_boot = 200, seed = 1, ci_level = 0.95)
  r50 <- phs_metrics(test_data, metrics = "C_index",
                     bootstrap = TRUE, n_boot = 200, seed = 1, ci_level = 0.50)

  width95 <- r95$conf_high - r95$conf_low
  width50 <- r50$conf_high - r50$conf_low
  expect_gt(width95, width50)
})


test_that("strata argument is accepted and returns same structure", {
  # boot::boot(strata = ...) requires an integer or factor vector,
  # so we coerce the character column to factor before passing it in.
  set.seed(1)
  d <- test_data
  d$cohort <- factor(sample(c("A", "B"), nrow(d), replace = TRUE))

  result <- phs_metrics(d, metrics = "HR",
                        bootstrap = TRUE, n_boot = 50, seed = 1,
                        strata = "cohort")

  expect_s3_class(result, "tbl_df")
  expect_false(any(is.na(result$conf_low)))
})


test_that("parallel = non-'no' value errors with informative message", {
  expect_error(
    phs_metrics(test_data, metrics = "HR",
                bootstrap = TRUE, n_boot = 10,
                parallel = "multicore"),
    regexp = "not yet implemented"
  )
})


test_that("invalid boot_method errors", {
  expect_error(
    phs_metrics(test_data, metrics = "HR",
                bootstrap = TRUE, n_boot = 10,
                boot_method = "jackknife"),
    regexp = "should be one of"   # match.arg error message
  )
})


test_that("unrecognised metric name errors with informative message", {
  # match.arg with several.ok = TRUE silently drops unknown names — we guard
  # against this with an explicit check that should error loudly instead.
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


test_that("bootstrap fills CIs and SE for HR_SD", {
  result <- phs_metrics(test_data, metrics = "HR_SD",
                        bootstrap = TRUE, n_boot = 50, seed = 1)
  expect_false(is.na(result$conf_low))
  expect_false(is.na(result$conf_high))
  expect_false(is.na(result$se))
  expect_gt(result$se, 0)
})
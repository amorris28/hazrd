test_that("get_hr forwards correctly and warns", {
  expect_warning(
    hr <- get_hr(test_data),
    regexp = "deprecated"
  )
  expect_equal(hr$value, 8.170775, tolerance = 0.001)
})

test_that("get_cindex forwards correctly and warns", {
  expect_warning(
    cindex <- get_cindex(test_data),
    regexp = "deprecated"
  )
  expect_equal(cindex$value, 0.7082444, tolerance = 0.001)
})

test_that("get_or forwards correctly and warns", {
  expect_warning(
    or <- get_or(test_data, or_age = 70),
    regexp = "deprecated"
  )
  expect_equal(or$value, 8.521204, tolerance = 0.001)
})
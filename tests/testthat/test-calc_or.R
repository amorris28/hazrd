test_that("calc_or returns OR > 1 for known dataset", {
    or_val <- calc_or(
        test_data,
        or_age = 60,
        numerator = c(0.8, 1.0),  # top 20%
        denominator = c(0.0, 0.2) # bottom 20%
    )
    
    expect_gt(or_val, 1)
})

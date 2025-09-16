# tests/testthat/test-phsfit.R

test_that("phsfit + get_* functions behave as expected", {
    # no bootstrap
    phsfit_no_boot <- fit_phs(test_data)
    
    # get_hr no bootstrap
    expect_warning(
        hr_no_boot <- get_hr(phsfit_no_boot),
        regexp = "No bootstrap", # or whatever your warning text is
        fixed = TRUE
    )
    expect_type(hr_no_boot, "list")
    expect_equal(length(hr_no_boot), 1L)
    
    # get_hr with bootstrap
    phsfit_boot <- fit_phs(test_data, bootstrap.iterations = 9)
    hr_boot <- get_hr(phsfit_boot)
    expect_type(hr_boot, "list")
    expect_equal(length(hr_boot), 3L)
    expect_lt(hr_boot$conf.low, hr_boot$value)
    expect_gt(hr_boot$conf.high, hr_boot$value)
    expect_true(hr_boot$value > 0)
    
    # OR
    or_boot <- get_or(phsfit_boot, or_age = 60)
    expect_type(or_boot, "list")
    # if your OR returns similar structure:
    expect_true(or_boot$value > 0) 
    
    # c-index
    cindex_boot <- get_cindex(phsfit_boot)
    expect_type(cindex_boot, "list")
    expect_true(cindex_boot$value >= 0 && cindex_boot$value <= 1)
    
    # HRSD
    hrsd_boot <- get_or(phsfit_boot, or_age = 60)
    expect_type(hrsd_boot, "list")
    # if your OR returns similar structure:
    expect_true(hrsd_boot$value > 0) 
    
})


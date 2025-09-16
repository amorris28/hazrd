#' Fit a polygenic hazard score (PHS) model
#'
#' `fit_phs()` is a generic function that fits a PHS model from either a formula
#' interface (similar to `coxph()`) or directly from a data frame with specified
#' columns. It returns an object of class `"phsfit"`, which can then be passed
#' to `get_hr()`, `get_or()`, `get_cindex()`, etc.
#'
#' @param object An object for which a PHs fit is desired. See the method documentation
#' @param ... Arguments passed to specific methods. See the method documentation
#' for details.
#'
#' @return An object of class `"phsfit"`.
#' @seealso [fit_phs.formula()], [fit_phs.data.frame()]
#' @export
#'
#' @examples
#' # Formula interface:
#' fit_phs(Surv(age, status) ~ phs, data = test_data)
#'
#' # Data frame interface:
#' fit_phs(test_data, age = "age", status = "status", phs = "phs")
fit_phs <- function(object, ...) UseMethod("fit_phs")

#'
#' @param object A survival formula, e.g. Surv(age, status) ~ phs
#' @param data A data.frame containing variables in the formula
#' @param bootstrap.iterations Number of bootstrap iterations to run to generate confidence intervals.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Additional arguments (currently ignored)
#' @export
#' @rdname fit_phs
fit_phs.formula <- function(object, 
                            data, 
                            bootstrap.iterations = NULL,
                            conf.level = 0.95,
                            ...) {
    formula <- object
    # Fit the base model
    model <- survival::coxph(formula, data = data)
    
    # Do bootstrapping if requested
    boot <- NULL
    if (!is.null(bootstrap.iterations)) {
        n <- nrow(data)
        boot_results <- vector("list", bootstrap.iterations)
        for (i in seq_len(bootstrap.iterations)) {
            idx <- sample.int(n, n, replace = TRUE)
            boot_results[[i]] <- survival::coxph(formula, data = data[idx, ])
        }
        boot <- boot_results
    }
    
    structure(
        list(
            model = model,
            boot  = boot,
            formula = formula,
            data = data,
            bootstrap.iterations = bootstrap.iterations
        ),
        class = "phsfit"
    )
}
#' Fit a polygenic hazard score model
#'
#' @param object a data.frame containing the variables for phs, age, and status
#' @param phs an optional string specifying the column name in `data` containing the polygenic hazard score for each subject. The default is "phs"
#' @param age an optional string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"
#' @param status an optional string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). The default is "status"
#' @param bootstrap.iterations Number of bootstrap iterations to run to generate confidence intervals.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Additional arguments (currently ignored)
#' @export
#' @rdname fit_phs
fit_phs.data.frame <- function(object,
                               age = "age",
                               status = "status",
                               phs = "phs",
                               bootstrap.iterations = NULL,
                               conf.level = 0.95,
                               ...) {
    data <- object
    for (col in c(phs, age, status)) {
        if (!(col %in% names(data))) stop("Column `", col, "` not found in `data`.")
    }
    phs_vec <- if (is.character(phs)) data[[phs]] else phs
    age_vec <- if (is.character(age)) data[[age]] else age
    status_vec <- if (is.character(status)) data[[status]] else status
    
    df <- data.frame(phs = phs_vec, age = age_vec, status = status_vec)
    
    formula <- stats::as.formula(
        paste0("survival::Surv(", age, ",", status, ") ~ ", phs)
    )
    fit_phs.formula(formula, df, bootstrap.iterations, conf.level)
}

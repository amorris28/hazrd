#' Fit a polygenic hazard score (PHS) model
#'
#' `fit_phs()` fits a PHS model from either a formula
#' interface (similar to `coxph()`) or directly from a data frame with specified
#' columns. It returns an object of class `"phsfit"`, which can then be passed
#' to `get_hr()`, `get_or()`, `get_cindex()`, etc.
#'
#' @param object Either  
#'   - a survival formula, e.g. `Surv(age, status) ~ phs` (formula interface), or  
#'   - a `data.frame` containing the variables for `phs`, `age`, and `status` (data-frame interface).
#' @param data A `data.frame` containing variables in the formula (formula interface only).
#' @param phs A string specifying the column name in `data` containing the polygenic hazard score for each subject (data-frame interface only). Default `"phs"`.
#' @param age A string specifying the column name in `data` containing the age of each subject. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). Default `"age"`.
#' @param status A string specifying the column name in `data` containing case-control status (0 = censored, 1 = event). Default `"status"`.
#' @param bootstrap.iterations Number of bootstrap iterations to run to generate confidence intervals.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An object of class `"phsfit"` containing the base Cox model, any bootstrap fits and indices, the formula, the original data, and bootstrap metadata.
#' @seealso [fit_phs.formula()], [fit_phs.data.frame()]
#' @export
#'
#' @examples
#' # Formula interface:
#' fit <- fit_phs(Surv(age, status) ~ phs, data = test_data)
#'
#' # Data frame interface:
#' fit <- fit_phs(test_data, age = "age", status = "status", phs = "phs")
fit_phs <- function(object, ...) UseMethod("fit_phs")

#' @export
#' @rdname fit_phs
fit_phs.formula <- function(object, 
                            data, 
                            bootstrap.iterations = NULL,
                            conf.level = 0.95,
                            ...) {
    formula <- object
    model <- survival::coxph(formula, data = data)
    
    boot <- NULL
    if (!is.null(bootstrap.iterations)) {
        n <- nrow(data)
        boot_fits <- vector("list", bootstrap.iterations)
        boot_idx  <- vector("list", bootstrap.iterations)
        for (i in seq_len(bootstrap.iterations)) {
            idx <- sample.int(n, n, replace = TRUE)
            boot_idx[[i]] <- idx
            boot_fits[[i]] <- survival::coxph(formula, data = data[idx, ])
        }
        boot <- list(fits = boot_fits, indices = boot_idx)
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

#' @export
# print.phsfit <- function(x, ...) {
#     cat("<phsfit object>\n")
#     cat("Formula: ", deparse(x$formula), "\n")
#     cat("Number of observations: ", nrow(x$data), "\n")
#     cat("Bootstrap iterations: ",
#         ifelse(is.null(x$bootstrap.iterations), 0, x$bootstrap.iterations),
#         "\n\n")
# 
#     # show base model summary briefly:
#     print(summary(x$model))
# 
#     invisible(x)
# }

# print.phsfit <- function(x, ...) {
#     cat("\nPolygenic Hazard Score (PHS) model\n")
#     cat("==================================\n")
#     
#     ## Show formula
#     if (!is.null(x$formula)) {
#         cat("Formula: ", deparse(x$formula), "\n")
#     }
#     
#     ## Base model info (optional)
#     if (!is.null(x$model$call)) {
#         # For coxph fits, $call stores the call like coxph(formula, data)
#         cat("Base model: coxph fit on", nrow(x$data), "rows\n")
#     }
#     
#     ## Bootstrap info
#     nboot <- x$bootstrap.iterations
#     if (is.null(nboot) || nboot == 0) {
#         cat("Bootstrap: none\n")
#     } else {
#         cat("Bootstrap iterations: ", nboot, "\n")
#         if (!is.null(x$boot) && length(x$boot$fits) > 0) {
#             cat("Stored bootstrap fits: ", length(x$boot$fits), "\n")
#         }
#     }
#     
#     ## You can add more summary info here (like #events)
#     invisible(x)
# }

# print.phsfit <- function(x, ...) {
#     cat("\nPolygenic Hazard Score (PHS) model\n")
#     cat("===================================\n")
# 
#     # Formula
#     if (!is.null(x$formula)) {
#         cat("Formula: ", paste(deparse(x$formula), collapse = ""), "\n")
#     }
# 
#     # Base model info
#     n <- nrow(x$data)
#     nevents <- sum(x$data[[all.vars(x$formula)[2]]])  # crude estimate assuming Surv(time, status)
#     cat("Base model: n = ", n, ", number of events = ", nevents, "\n")
# 
#     # Bootstrap info
#     nboot <- x$bootstrap.iterations
#     if (is.null(nboot) || nboot == 0) {
#         cat("Bootstrap: none\n")
#     } else {
#         cat("Bootstrap iterations:", nboot, "\n")
#         if (!is.null(x$boot$fits)) {
#             cat("Stored bootstrap fits:", length(x$boot$fits), "\n")
#         }
#     }
# 
#     # Show coefficients like coxph()
#     if (!is.null(x$model)) {
#         cat("\nCoefficients:\n")
#         print(coef(x$model))
#     }
# 
#     invisible(x)
# }



#' @export
print.phsfit <- function(x, ...) {
    cat("\nPolygenic Hazard Score (PHS) model\n")
    cat("===================================\n")
    
    # Formula
    if (!is.null(x$formula)) {
        cat("Formula: ", paste(deparse(x$formula), collapse = ""), "\n")
    }
    
    # Base model info
    n <- nrow(x$data)
    status_var <- all.vars(x$formula)[2]  # crude assumption: Surv(time, status) ~ ...
    nevents <- sum(x$data[[status_var]])
    cat("Base model: n = ", n, ", number of events = ", nevents, "\n")
    
    # Bootstrap info
    nboot <- x$bootstrap.iterations
    if (is.null(nboot) || nboot == 0) {
        cat("Bootstrap: none\n")
    } else {
        cat("Bootstrap iterations:", nboot, "\n")
        if (!is.null(x$boot$fits)) {
            cat("Stored bootstrap fits:", length(x$boot$fits), "\n")
        }
    }
    
    # PHS summary indices
    cat("\nPHS Summary Indices:\n")
    hr80_20 <- tryCatch(get_hr(x), error = function(e) NA)
    hrsd    <- tryCatch(get_hrsd(x), error = function(e) NA)
    cindex  <- tryCatch(get_cindex(x), error = function(e) NA)
    
    if (!is.na(hr80_20$value)) {
        cat("HR80_20: ", round(hr80_20$value, 3))
        if (!is.null(hr80_20$conf.low)) {
            cat(" (", round(hr80_20$conf.low, 3), "-", round(hr80_20$conf.high, 3), ")", sep = "")
        }
        cat("\n")
    }
    
    if (!is.na(hrsd$value)) {
        cat("HR per SD: ", round(hrsd$value, 3))
        if (!is.null(hrsd$conf.low)) {
            cat(" (", round(hrsd$conf.low, 3), "-", round(hrsd$conf.high, 3), ")", sep = "")
        }
        cat("\n")
    }
    
    if (!is.na(cindex$value)) {
        cat("C-index: ", round(cindex$value, 3))
        if (!is.null(cindex$conf.low)) {
            cat(" (", round(cindex$conf.low, 3), "-", round(cindex$conf.high, 3), ")", sep = "")
        }
        cat("\n")
    }
    
    invisible(x)
}

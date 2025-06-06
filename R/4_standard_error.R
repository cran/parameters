#' @title Standard Errors
#' @name standard_error
#'
#' @description `standard_error()` attempts to return standard errors of model
#' parameters.
#'
#' @param model A model.
#' @param force Logical, if `TRUE`, factors are converted to numerical
#'   values to calculate the standard error, with the lowest level being the
#'   value `1` (unless the factor has numeric levels, which are converted
#'   to the corresponding numeric value). By default, `NA` is returned for
#'   factors or character vectors.
#' @param vcov Variance-covariance matrix used to compute uncertainty estimates
#' (e.g., for robust standard errors). This argument accepts a covariance
#' matrix, a function which returns a covariance matrix, or a string which
#' identifies the function to be used to compute the covariance matrix.
#'  * A covariance matrix
#'  * A function which returns a covariance matrix (e.g., `stats::vcov()`)
#'  * A string which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
#'      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
#'      `"CR2"`, `"CR3"`. See `?clubSandwich::vcovCR`
#'    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
#'      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
#'      `?sandwich::vcovBS`
#'    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`, `"OPG"`,
#'      `"PL"`.
#' @param vcov_args List of arguments to be passed to the function identified by
#'   the `vcov` argument. This function is typically supplied by the
#'   **sandwich** or **clubSandwich** packages. Please refer to their
#'   documentation (e.g., `?sandwich::vcovHAC`) to see the list of available
#'   arguments. If no estimation type (argument `type`) is given, the default
#'   type for `"HC"` equals the default from the **sandwich** package; for type
#'   `"CR"`, the default is set to `"CR3"`.
#' @param effects Should standard errors for fixed effects (`"fixed"`), random
#'   effects (`"random"`), or both (`"all"`) be returned? Only applies
#'   to mixed models. May be abbreviated. When standard errors for random
#'   effects are requested, for each grouping factor a list of standard errors
#'   (per group level) for random intercepts and slopes is returned.
#' @param component Model component for which standard errors should be shown.
#'   See the documentation for your object's class in [`model_parameters()`] or
#'   [`p_value()`] for further details.
#' @inheritParams simulate_model
#' @inheritParams p_value
#' @param ... Arguments passed to or from other methods.
#'
#' @note For Bayesian models (from **rstanarm** or **brms**), the standard
#'   error is the SD of the posterior samples.
#'
#' @return A data frame with at least two columns: the parameter names and the
#'   standard errors. Depending on the model, may also include columns for model
#'   components etc.
#'
#' @examplesIf require("sandwich") && require("clubSandwich")
#' model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
#' standard_error(model)
#'
#' # robust standard errors
#' standard_error(model, vcov = "HC3")
#'
#' # cluster-robust standard errors
#' standard_error(model,
#'   vcov = "vcovCL",
#'   vcov_args = list(cluster = iris$Species)
#' )
#' @export
standard_error <- function(model, ...) {
  UseMethod("standard_error")
}


# Default methods ---------------------------------------------------------

#' @rdname standard_error
#' @export
standard_error.default <- function(model,
                                   effects = "fixed",
                                   component = "all",
                                   vcov = NULL,
                                   vcov_args = NULL,
                                   verbose = TRUE,
                                   ...) {
  # check for valid input
  .is_model_valid(model)

  dots <- list(...)
  se <- NULL

  # if a vcov is provided, we calculate standard errors based on that matrix
  # this is usually the case for HC (robust) standard errors
  # ------------------------------------------------------------------------

  # vcov: matrix
  if (is.matrix(vcov)) {
    se <- sqrt(diag(vcov))
  }

  # vcov: function which returns a matrix
  if (is.function(vcov)) {
    fun_args <- c(list(model), vcov_args, dots)
    se <- .safe(sqrt(diag(do.call("vcov", fun_args))))
  }

  # vcov: character
  if (is.character(vcov)) {
    .vcov <- insight::get_varcov(
      model,
      component = component,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    )
    se <- sqrt(diag(.vcov))
  }

  # classical SE from summary()
  # ------------------------------------------------------------------------

  if (is.null(se)) {
    se <- .safe({
      if (grepl("Zelig-", class(model)[1], fixed = TRUE)) {
        unlist(model$get_se())
      } else {
        .get_se_from_summary(model)
      }
    })
  }

  # if retrieving SE from summary() failed, we try to calculate SE based
  # on classical se from get_varcov()
  # ------------------------------------------------------------------------

  if (is.null(se)) {
    se <- .safe({
      varcov <- insight::get_varcov(model, component = component)
      se_from_varcov <- sqrt(diag(varcov))
      names(se_from_varcov) <- colnames(varcov)
      se_from_varcov
    })
  }

  # output
  if (is.null(se)) {
    if (isTRUE(verbose)) {
      insight::format_warning("Could not extract standard errors from model object.")
    }
  } else {
    params <- insight::get_parameters(model, component = component)
    if (length(se) == nrow(params) && "Component" %in% colnames(params)) {
      se <- .data_frame(Parameter = params$Parameter, SE = as.vector(se), Component = params$Component)
    } else {
      se <- .data_frame(Parameter = names(se), SE = as.vector(se))
    }
  }

  se
}


# helper -----------------------------------------------------------------


.get_se_from_summary <- function(model, component = NULL) {
  cs <- .safe(suppressWarnings(stats::coef(summary(model))))
  se <- NULL

  if (is.list(cs) && !is.null(component)) {
    cs <- cs[[component]]
  }
  if (!is.null(cs)) {
    # do we have a se column?
    se_col <- which(colnames(cs) == "Std. Error")
    # if not, default to 2
    if (length(se_col) == 0) {
      se_col <- 2
    }
    se <- as.vector(cs[, se_col])
    if (is.null(names(se))) {
      coef_names <- rownames(cs)
      if (length(coef_names) == length(se)) {
        names(se) <- coef_names
      }
    }
  }
  names(se) <- .remove_backticks_from_string(names(se))
  se
}


.check_vcov_args <- function(robust, ...) {
  dots <- list(...)
  isTRUE(isTRUE(robust) || isTRUE(dots$robust) || ("vcov" %in% names(dots) && !is.null(dots[["vcov"]])))
}


# .ranef_se <- function(x) {
# insight::check_if_installed("lme4")
#
#   cc <- stats::coef(model)
#
#   # get names of intercepts
#   inames <- names(cc)
#
#   # variances of fixed effects
#   fixed.vars <- diag(as.matrix(stats::vcov(model)))
#
#   # extract variances of conditional modes
#   r1 <- lme4::ranef(model, condVar = TRUE)
#
#   # we may have multiple random intercepts, iterate all
#   se.merMod <- lapply(1:length(cc), function(i) {
#     cmode.vars <- t(apply(attr(r1[[i]], "postVar"), 3, diag))
#     seVals <- sqrt(sweep(cmode.vars, 2, fixed.vars[names(r1[[i]])], "+", check.margin = FALSE))
#
#     if (length(r1[[i]]) == 1) {
#       seVals <- as.data.frame(t(seVals))
#       stats::setNames(seVals, names(r1[[i]]))
#     } else {
#       seVals <- seVals[, 1:2]
#       stats::setNames(as.data.frame(seVals), names(r1[[i]]))
#     }
#   })
#
#   # set names of list
#   names(se.merMod) <- inames
#
#   se.merMod
# }

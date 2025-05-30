# classes: .cpglm, .bcpglm, .zcpglm, .cpglmm


########## .zcpglm ---------------


#' @title Parameters from Zero-Inflated Models
#' @name model_parameters.zcpglm
#'
#' @description
#' Parameters from zero-inflated models (from packages like **pscl**,
#' **cplm** or **countreg**).
#'
#' @param model A model with zero-inflation component.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @seealso [insight::standardize_names()] to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @section Model components:
#' Possible values for the `component` argument depend on the model class.
#' Following are valid options:
#' - `"all"`: returns all model components, applies to all models, but will only
#'   have an effect for models with more than just the conditional model component.
#' - `"conditional"`: only returns the conditional component, i.e. "fixed effects"
#'   terms from the model. Will only have an effect for models with more than
#'   just the conditional model component.
#' - `"smooth_terms"`: returns smooth terms, only applies to GAMs (or similar
#'   models that may contain smooth terms).
#' - `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.
#' - `"dispersion"`: returns the dispersion model component. This is common
#'   for models with zero-inflation or that can model the dispersion parameter.
#' - `"instruments"`: for instrumental-variable or some fixed effects regression,
#'   returns the instruments.
#' - `"nonlinear"`: for non-linear models (like models of class `nlmerMod` or
#'   `nls`), returns staring estimates for the nonlinear parameters.
#' - `"correlation"`: for models with correlation-component, like `gls`, the
#'   variables used to describe the correlation structure are returned.
#'
#' **Special models**
#'
#' Some model classes also allow rather uncommon options. These are:
#' - **mhurdle**: `"infrequent_purchase"`, `"ip"`, and `"auxiliary"`
#' - **BGGM**: `"correlation"` and `"intercept"`
#' - **BFBayesFactor**, **glmx**: `"extra"`
#' - **averaging**:`"conditional"` and `"full"`
#' - **mjoint**: `"survival"`
#' - **mfx**: `"precision"`, `"marginal"`
#' - **betareg**, **DirichletRegModel**: `"precision"`
#' - **mvord**: `"thresholds"` and `"correlation"`
#' - **clm2**: `"scale"`
#' - **selection**: `"selection"`, `"outcome"`, and `"auxiliary"`
#' - **lavaan**: One or more of `"regression"`, `"correlation"`, `"loading"`,
#'   `"variance"`, `"defined"`, or `"mean"`. Can also be `"all"` to include
#'   all components.
#'
#' For models of class `brmsfit` (package **brms**), even more options are
#' possible for the `component` argument, which are not all documented in detail
#' here.
#'
#' @examplesIf require("pscl")
#' data("bioChemists", package = "pscl")
#' model <- pscl::zeroinfl(
#'   art ~ fem + mar + kid5 + ment | kid5 + phd,
#'   data = bioChemists
#' )
#' model_parameters(model)
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.zcpglm <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = "all",
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    include_info = getOption("parameters_info", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- insight::validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))

  # fix argument, if model has no zi-part
  if (!insight::model_info(model, verbose = FALSE)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    params <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      standardize = standardize,
      p_adjust = p_adjust,
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      ...
    )
  }

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    include_info = include_info,
    verbose = verbose,
    ...
  )
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.zcpglm <- function(model, component = "all", ...) {
  insight::check_if_installed("cplm")

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated")
  )
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    SE = as.vector(stats$tweedie[, "Std. Error"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    SE = as.vector(stats$zero[, "Std. Error"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}


#' @export
p_value.zcpglm <- function(model, component = "all", ...) {
  insight::check_if_installed("cplm")

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated")
  )
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    p = as.vector(stats$tweedie[, "Pr(>|z|)"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    p = as.vector(stats$zero[, "Pr(>|z|)"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}


########## .bcpglm ---------------

#' @export
model_parameters.bcplm <- model_parameters.bayesQR


#' @export
p_value.bcplm <- p_value.brmsfit


########## .cpglm ---------------

#' @export
p_value.cpglm <- function(model, ...) {
  insight::check_if_installed("cplm")

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}


#' @export
standard_error.cpglm <- function(model, ...) {
  insight::check_if_installed("cplm")

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}


########## .cpglmm ---------------

#' @export
model_parameters.cpglmm <- function(model,
                                    ci = 0.95,
                                    ci_method = NULL,
                                    ci_random = NULL,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    effects = "all",
                                    group_level = FALSE,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    include_sigma = FALSE,
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  # p-values, CI and se might be based on different df-methods
  ci_method <- .check_df_method(ci_method)
  effects <- insight::validate_argument(effects, c("fixed", "random", "all"))

  # standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_alert("Standardizing coefficients only works for fixed effects of the mixed model.")
    }
    effects <- "fixed"
  }

  params <- .mixed_model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    effects = effects,
    p_adjust = p_adjust,
    group_level = group_level,
    ci_method = ci_method,
    include_sigma = include_sigma,
    ci_random = ci_random,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", "data.frame")

  params
}


#' @export
p_value.cpglmm <- function(model, method = "wald", ...) {
  p_value.default(model, method = method, ...)
}


#' @export
standard_error.cpglmm <- function(model, ...) {
  insight::check_if_installed("cplm")

  stats <- cplm::summary(model)$coefs
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}


# tools --------------------

.check_df_method <- function(df_method) {
  if (!is.null(df_method)) {
    df_method <- tolower(df_method)
    if (df_method %in% c("satterthwaite", "kenward", "kr")) {
      insight::format_alert("Satterthwaite or Kenward-Rogers approximation of degrees of freedom is only available for linear mixed models.")
      df_method <- "wald"
    }
    df_method <- insight::validate_argument(
      df_method,
      c(
        "wald", "normal", "residual", "ml1", "betwithin", "profile",
        "boot", "uniroot"
      )
    )
  }
  df_method
}

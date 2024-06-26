#' Parameters from Bayesian Models
#'
#' Parameters from Bayesian models.
#'
#' @param model Bayesian model (including SEM from **blavaan**. May also be
#'   a data frame with posterior samples, however, `as_draws` must be set to
#'   `TRUE` (else, for data frames `NULL` is returned).
#' @param ci Credible Interval (CI) level. Default to `0.95` (`95%`). See
#'   [bayestestR::ci()] for further details.
#' @param group_level Logical, for multilevel models (i.e. models with random
#'   effects) and when `effects = "all"` or `effects = "random"`,
#'   include the parameters for each group level from random effects. If
#'   `group_level = FALSE` (the default), only information on SD and COR
#'   are shown.
#' @param component Which type of parameters to return, such as parameters for the
#'   conditional model, the zero-inflation part of the model, the dispersion
#'   term, or other auxiliary parameters be returned? Applies to models with
#'   zero-inflation and/or dispersion formula, or if parameters such as `sigma`
#'   should be included. May be abbreviated. Note that the *conditional*
#'   component is also called *count* or *mean* component, depending on the
#'   model. There are three convenient shortcuts: `component = "all"` returns
#'   all possible parameters. If `component = "location"`, location parameters
#'   such as `conditional`, `zero_inflated`, or `smooth_terms`, are returned
#'   (everything that are fixed or random effects - depending on the `effects`
#'   argument - but no auxiliary parameters). For `component = "distributional"`
#'   (or `"auxiliary"`), components like `sigma`, `dispersion`, or `beta`
#'   (and other auxiliary parameters) are returned.
#' @param as_draws Logical, if `TRUE` and `model` is of class `data.frame`,
#'   the data frame is treated as posterior samples and handled similar to
#'   Bayesian models. All arguments in `...` are passed to
#'   `model_parameters.draws()`.
#' @inheritParams model_parameters.default
#' @inheritParams bayestestR::describe_posterior
#' @inheritParams insight::get_parameters
#'
#' @seealso [insight::standardize_names()] to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @note When `standardize = "refit"`, columns `diagnostic`,
#'   `bf_prior` and `priors` refer to the *original*
#'   `model`. If `model` is a data frame, arguments `diagnostic`,
#'   `bf_prior` and `priors` are ignored. \cr \cr There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the
#'   [**see**-package](https://easystats.github.io/see/).
#'
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("rstanarm")) {
#'   model <- suppressWarnings(stan_glm(
#'     Sepal.Length ~ Petal.Length * Species,
#'     data = iris, iter = 500, refresh = 0
#'   ))
#'   model_parameters(model)
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.stanreg <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = "pd",
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     bf_prior = NULL,
                                     diagnostic = c("ESS", "Rhat"),
                                     priors = TRUE,
                                     effects = "fixed",
                                     exponentiate = FALSE,
                                     standardize = NULL,
                                     group_level = FALSE,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  # Processing
  params <- .extract_parameters_bayesian(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    diagnostic = diagnostic,
    priors = priors,
    effects = effects,
    standardize = standardize,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  if (effects != "fixed") {
    random_effect_levels <- which(
      params$Effects == "random" & !startsWith(params$Parameter, "Sigma[")
    )
    if (length(random_effect_levels) && isFALSE(group_level)) {
      params <- params[-random_effect_levels, , drop = FALSE]
    }
  }

  ## TODO: can we use the regular pretty-name-formatting?
  params <- .add_pretty_names(params, model)

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    ci_method = ci_method,
    group_level = group_level,
    verbose = verbose,
    ...
  )

  attr(params, "parameter_info") <- insight::clean_parameters(model)
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
model_parameters.stanmvreg <- function(model,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = "pd",
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       bf_prior = NULL,
                                       diagnostic = c("ESS", "Rhat"),
                                       priors = TRUE,
                                       effects = "fixed",
                                       standardize = NULL,
                                       keep = NULL,
                                       drop = NULL,
                                       verbose = TRUE,
                                       ...) {
  # Processing
  params <- .extract_parameters_bayesian(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    diagnostic = diagnostic,
    priors = priors,
    effects = effects,
    standardize = standardize,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  params$Parameter <- gsub("^(.*)\\|(.*)", "\\2", params$Parameter)
  params <- .add_pretty_names(params, model)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.stanreg <- standard_error.brmsfit


#' @export
standard_error.mvstanreg <- standard_error.brmsfit


#' @export
p_value.stanreg <- p_value.BFBayesFactor

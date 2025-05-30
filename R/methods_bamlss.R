#' @export
model_parameters.bamlss <- function(model,
                                    centrality = "median",
                                    dispersion = FALSE,
                                    ci = 0.95,
                                    ci_method = "eti",
                                    test = "pd",
                                    rope_range = "default",
                                    rope_ci = 0.95,
                                    component = "all",
                                    exponentiate = FALSE,
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
    bf_prior = NULL,
    diagnostic = NULL,
    priors = FALSE,
    effects = "all",
    component = component,
    standardize = standardize,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  params <- .add_pretty_names(params, model)

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(params,
    model,
    ci,
    exponentiate,
    ci_method = ci_method,
    verbose = verbose,
    ...
  )

  attr(params, "parameter_info") <- insight::clean_parameters(model)
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- unique(c("parameters_model", "see_parameters_model", class(params)))

  params
}


#' @export
standard_error.bamlss <- function(model,
                                  component = c("all", "conditional", "location", "distributional", "auxilliary"),
                                  ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = component, ...)

  .data_frame(
    Parameter = colnames(params),
    SE = unname(sapply(params, stats::sd, na.rm = TRUE))
  )
}


#' @export
p_value.bamlss <- p_value.BFBayesFactor

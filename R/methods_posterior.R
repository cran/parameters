#' @rdname model_parameters.stanreg
#' @export
model_parameters.draws <- function(model,
                                   centrality = "median",
                                   dispersion = FALSE,
                                   ci = .95,
                                   ci_method = "eti",
                                   test = c("pd", "rope"),
                                   rope_range = "default",
                                   rope_ci = 0.95,
                                   keep = NULL,
                                   drop = NULL,
                                   parameters = keep,
                                   verbose = TRUE,
                                   ...) {
  out <- .posterior_draws_to_df(model)

  # Processing
  params <- .extract_parameters_bayesian(
    out,
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
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}



# Standard Errors ---------------------------------------------

#' @export
standard_error.draws <- function(model, verbose = TRUE, ...) {
  params <- .posterior_draws_to_df(model)
  .data_frame(
    Parameter = colnames(params),
    SE = unname(sapply(params, stats::sd, na.rm = TRUE))
  )
}



# p-Values ---------------------------------------------

#' @export
p_value.draws <- function(model, ...) {
  params <- .posterior_draws_to_df(model)
  p <- bayestestR::p_direction(params)
  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}



# helper ------------------------------

.posterior_draws_to_df <- function(x) {
  UseMethod(".posterior_draws_to_df")
}

.posterior_draws_to_df.default <- function(x) {
  stop(paste0("Objects of class '%s' are not yet supported.", class(x)[1]))
}

.posterior_draws_to_df.data.frame <- function(x) {
  x
}

.posterior_draws_to_df.draws_df <- function(x) {
  insight::check_if_installed("posterior")
  datawizard::data_remove(as.data.frame(posterior::as_draws_df(x)), c(".chain", ".iteration", ".draw"))
}

.posterior_draws_to_df.draws_matrix <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_array <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_list <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_rvars <- .posterior_draws_to_df.draws_df
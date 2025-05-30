# .varest

#' @export
model_parameters.varest <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- model_parameters(
      model = model$varresult[[i]],
      ci = ci,
      bootstrap = bootstrap,
      iterations = iterations,
      standardize = standardize,
      exponentiate = exponentiate,
      p_adjust = p_adjust,
      verbose = verbose,
      ...
    )
    out$Group <- paste0("Equation ", i)
    out
  })

  params <- do.call(rbind, params)
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  params
}


#' @export
ci.varest <- function(x, ci = 0.95, method = NULL, ...) {
  params <- lapply(names(x$varresult), function(i) {
    out <- ci(x = x$varresult[[i]], ci = ci, method = method, ...)
    out$Group <- paste0("Equation ", i)
    out
  })

  do.call(rbind, params)
}


#' @export
standard_error.varest <- function(model, method = NULL, ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- standard_error(model = model$varresult[[i]], method = method, ...)
    out$Group <- paste0("Equation ", i)
    out
  })

  do.call(rbind, params)
}


#' @export
p_value.varest <- function(model, ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- p_value(model = model$varresult[[i]], ...)
    out$Group <- paste0("Equation ", i)
    out
  })

  do.call(rbind, params)
}


#' @export
simulate_model.varest <- function(model, iterations = 1000, ...) {
  out <- lapply(names(model$varresult), function(i) {
    simulate_model(model = model$varresult[[i]], iterations = iterations, ...)
  })
  names(out) <- paste0("Equation ", names(model$varresult))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
simulate_parameters.varest <- function(model,
                                       iterations = 1000,
                                       centrality = "median",
                                       ci = 0.95,
                                       ci_method = "quantile",
                                       test = "p-value",
                                       ...) {
  data <- simulate_model(model, iterations = iterations, ...)
  out <- lapply(names(data), function(i) {
    x <- .summary_bootstrap(
      data = data[[i]],
      test = test,
      centrality = centrality,
      ci = ci,
      ci_method = ci_method,
      ...
    )
    x$Group <- i
    x
  })

  out <- do.call(rbind, out)
  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality

  out
}

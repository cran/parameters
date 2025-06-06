#' @export
model_parameters.mhurdle <- function(model,
                                     ci = 0.95,
                                     component = "all",
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary")
  )

  params <- .model_parameters_generic(
    model,
    ci = ci,
    merge_by = c("Parameter", "Component"),
    exponentiate = exponentiate,
    effects = "fixed",
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )
  params$Parameter <- gsub("^(h1|h2|h3)\\.(.*)", "\\2", params$Parameter)

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  params
}


#' @export
p_value.mhurdle <- function(model,
                            component = c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary"),
                            ...) {
  component <- match.arg(component)
  s <- summary(model)
  params <- insight::get_parameters(model, component = "all")

  pvals <- data.frame(
    Parameter = rownames(s$coefficients),
    p = as.vector(s$coefficients[, 4]),
    stringsAsFactors = FALSE
  )

  params <- merge(params, pvals, sort = FALSE)

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  params[c("Parameter", "p", "Component")]
}


#' @export
ci.mhurdle <- function(x, ci = 0.95, ...) {
  .ci_generic(model = x, ci = ci, ...)
}


#' @export
standard_error.mhurdle <- function(model, component = c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary"), ...) {
  component <- match.arg(component)
  s <- summary(model)

  params <- insight::get_parameters(model, component = "all")

  se <- data.frame(
    Parameter = rownames(s$coefficients),
    SE = as.vector(s$coefficients[, 2]),
    stringsAsFactors = FALSE
  )

  params <- merge(params, se, sort = FALSE)

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  params[c("Parameter", "SE", "Component")]
}


#' @export
simulate_model.mhurdle <- function(model, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary"), ...) {
  component <- match.arg(component)
  out <- .simulate_model(model, iterations, component = component, effects = "fixed", ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}

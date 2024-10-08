# model parameters ---------------------


#' @export
model_parameters.logitor <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     exponentiate = TRUE,
                                     p_adjust = NULL,
                                     verbose = TRUE,
                                     ...) {
  model_parameters.default(
    model$fit,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )
}


#' @export
model_parameters.poissonirr <- model_parameters.logitor


#' @export
model_parameters.negbinirr <- model_parameters.logitor


#' @export
model_parameters.poissonmfx <- function(model,
                                        ci = 0.95,
                                        bootstrap = FALSE,
                                        iterations = 1000,
                                        component = c("all", "conditional", "marginal"),
                                        standardize = NULL,
                                        exponentiate = FALSE,
                                        p_adjust = NULL,
                                        keep = NULL,
                                        drop = NULL,
                                        verbose = TRUE,
                                        ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
model_parameters.logitmfx <- model_parameters.poissonmfx


#' @export
model_parameters.probitmfx <- model_parameters.poissonmfx


#' @export
model_parameters.negbinmfx <- model_parameters.poissonmfx


#' @rdname model_parameters.averaging
#' @export
model_parameters.betaor <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = c("conditional", "precision", "all"),
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- match.arg(component)
  model_parameters.betareg(
    model$fit,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    component = component,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )
}


#' @rdname model_parameters.averaging
#' @export
model_parameters.betamfx <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     component = c("all", "conditional", "precision", "marginal"),
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}




# ci ------------------


#' @export
ci.logitor <- function(x, ci = 0.95, method = NULL, ...) {
  .ci_generic(model = x$fit, ci = ci, method = method, ...)
}


#' @export
ci.poissonirr <- ci.logitor


#' @export
ci.negbinirr <- ci.logitor


#' @export
ci.poissonmfx <- function(x,
                          ci = 0.95,
                          component = c("all", "conditional", "marginal"),
                          method = NULL,
                          ...) {
  component <- match.arg(component)
  .ci_generic(model = x, ci = ci, component = component, method = method, ...)
}


#' @export
ci.negbinmfx <- ci.poissonmfx


#' @export
ci.logitmfx <- ci.poissonmfx


#' @export
ci.probitmfx <- ci.poissonmfx


#' @export
ci.betaor <- function(x,
                      ci = 0.95,
                      component = c("all", "conditional", "precision"),
                      ...) {
  component <- match.arg(component)
  .ci_generic(model = x$fit, ci = ci, dof = Inf, component = component)
}


#' @export
ci.betamfx <- function(x,
                       ci = 0.95,
                       method = NULL,
                       component = c("all", "conditional", "precision", "marginal"),
                       ...) {
  component <- match.arg(component)
  .ci_generic(model = x, ci = ci, component = component, method = method, ...)
}




# standard error ------------------


#' @export
standard_error.negbin <- standard_error.default


#' @export
standard_error.logitor <- function(model, ...) {
  standard_error.default(model$fit, ...)
}


#' @export
standard_error.poissonirr <- standard_error.logitor


#' @export
standard_error.negbinirr <- standard_error.logitor


#' @export
standard_error.poissonmfx <- function(model,
                                      component = "all",
                                      ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- stats::coef(summary(model$fit))
  se <- c(as.vector(model$mfxest[, 2]), as.vector(cs[, 2]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    SE = se,
    Component = parms$Component
  )

  component <- match.arg(component, choices = c("all", "conditional", "marginal"))
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
standard_error.logitmfx <- standard_error.poissonmfx


#' @export
standard_error.probitmfx <- standard_error.poissonmfx


#' @export
standard_error.negbinmfx <- standard_error.poissonmfx


#' @export
standard_error.betaor <- function(model,
                                  component = c("all", "conditional", "precision"),
                                  ...) {
  component <- match.arg(component)
  standard_error.betareg(model$fit, component = component, ...)
}


#' @export
standard_error.betamfx <- function(model,
                                   component = "all",
                                   ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- do.call(rbind, stats::coef(summary(model$fit)))
  se <- c(as.vector(model$mfxest[, 2]), as.vector(cs[, 2]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    SE = se,
    Component = parms$Component
  )

  component <- match.arg(component, choices = c("all", "conditional", "precision", "marginal"))
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}



# p values ------------------


#' p-values for Marginal Effects Models
#'
#' This function attempts to return, or compute, p-values of marginal effects
#' models from package **mfx**.
#'
#' @param model A statistical model.
#' @param component Should all parameters, parameters for the conditional model,
#'   precision-component or marginal effects be returned? `component` may be one
#'   of `"conditional"`, `"precision"`, `"marginal"` or `"all"` (default).
#' @param ... Currently not used.
#'
#' @return A data frame with at least two columns: the parameter names and the
#'   p-values. Depending on the model, may also include columns for model
#'   components etc.
#'
#' @examples
#' if (require("mfx", quietly = TRUE)) {
#'   set.seed(12345)
#'   n <- 1000
#'   x <- rnorm(n)
#'   y <- rnegbin(n, mu = exp(1 + 0.5 * x), theta = 0.5)
#'   d <- data.frame(y, x)
#'   model <- poissonmfx(y ~ x, data = d)
#'
#'   p_value(model)
#'   p_value(model, component = "marginal")
#' }
#' @export
p_value.poissonmfx <- function(model,
                               component = c("all", "conditional", "marginal"),
                               ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- stats::coef(summary(model$fit))
  p <- c(as.vector(model$mfxest[, 4]), as.vector(cs[, 4]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    p = p,
    Component = parms$Component
  )

  component <- match.arg(component)
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
p_value.logitor <- function(model, method = NULL, ...) {
  p_value.default(model$fit, method = method, ...)
}


#' @export
p_value.poissonirr <- p_value.logitor


#' @export
p_value.negbinirr <- p_value.logitor


#' @export
p_value.logitmfx <- p_value.poissonmfx


#' @export
p_value.probitmfx <- p_value.poissonmfx


#' @export
p_value.negbinmfx <- p_value.poissonmfx


#' @rdname p_value.poissonmfx
#' @export
p_value.betaor <- function(model,
                           component = c("all", "conditional", "precision"),
                           ...) {
  component <- match.arg(component)
  p_value.betareg(model$fit, component = component, ...)
}


#' @rdname p_value.poissonmfx
#' @export
p_value.betamfx <- function(model,
                            component = c("all", "conditional", "precision", "marginal"),
                            ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- do.call(rbind, stats::coef(summary(model$fit)))
  p <- c(as.vector(model$mfxest[, 4]), as.vector(cs[, 4]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    p = p,
    Component = parms$Component
  )

  component <- match.arg(component)
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}




# simulate model ------------------


#' @export
simulate_model.betaor <- function(model,
                                  iterations = 1000,
                                  component = c("all", "conditional", "precision"),
                                  ...) {
  component <- match.arg(component)

  simulate_model.betareg(model$fit,
    iterations = iterations,
    component = component,
    ...
  )
}


#' @export
simulate_model.betamfx <- simulate_model.betaor

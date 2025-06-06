#' @title Simulate Model Parameters
#' @name simulate_parameters
#'
#' @description Compute simulated draws of parameters and their related indices
#' such as Confidence Intervals (CI) and p-values. Simulating parameter draws
#' can be seen as a (computationally faster) alternative to bootstrapping.
#'
#' @inheritParams simulate_model
#' @inheritParams bayestestR::describe_posterior
#'
#' @return A data frame with simulated parameters.
#'
#' @references Gelman A, Hill J. Data analysis using regression and
#' multilevel/hierarchical models. Cambridge; New York: Cambridge University
#' Press 2007: 140-143
#'
#' @seealso [`bootstrap_model()`], [`bootstrap_parameters()`], [`simulate_model()`]
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @details
#' ## Technical Details
#' `simulate_parameters()` is a computationally faster alternative
#' to `bootstrap_parameters()`. Simulated draws for coefficients are based
#' on a multivariate normal distribution (`MASS::mvrnorm()`) with mean
#' `mu = coef(model)` and variance `Sigma = vcov(model)`.
#'
#' ## Models with Zero-Inflation Component
#' For models from packages **glmmTMB**, **pscl**, **GLMMadaptive** and
#' **countreg**, the `component` argument can be used to specify
#' which parameters should be simulated. For all other models, parameters
#' from the conditional component (fixed effects) are simulated. This may
#' include smooth terms, but not random effects.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' simulate_parameters(model)
#'
#' \donttest{
#' if (require("glmmTMB", quietly = TRUE)) {
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'   simulate_parameters(model, centrality = "mean")
#'   simulate_parameters(model, ci = c(.8, .95), component = "zero_inflated")
#' }
#' }
#' @export
simulate_parameters <- function(model, ...) {
  UseMethod("simulate_parameters")
}


#' @rdname simulate_parameters
#' @export
simulate_parameters.default <- function(model,
                                        iterations = 1000,
                                        centrality = "median",
                                        ci = 0.95,
                                        ci_method = "quantile",
                                        test = "p-value",
                                        ...) {
  # check for valid input
  .is_model_valid(model)

  sim_data <- simulate_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(
    data = sim_data,
    test = test,
    centrality = centrality,
    ci = ci,
    ci_method = ci_method,
    ...
  )

  params <- insight::get_parameters(model, verbose = FALSE)
  if ("Effects" %in% colnames(params) && insight::n_unique(params$Effects) > 1) {
    out$Effects <- params$Effects
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality
  attr(out, "simulated") <- TRUE

  out
}

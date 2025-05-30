#' @export
model_parameters.nestedLogit <- function(model,
                                         ci = 0.95,
                                         ci_method = NULL,
                                         component = "all",
                                         bootstrap = FALSE,
                                         iterations = 1000,
                                         standardize = NULL,
                                         exponentiate = FALSE,
                                         p_adjust = NULL,
                                         vcov = NULL,
                                         vcov_args = NULL,
                                         include_info = getOption("parameters_info", FALSE),
                                         keep = NULL,
                                         drop = NULL,
                                         verbose = TRUE,
                                         ...) {
  dots <- list(...)

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else if (!is.null(vcov) || !is.null(vcov_args)) {
      ci_method <- "wald"
    } else {
      ci_method <- "profile"
    }
  }

  # "component" might be set to "conditional", when called from "compare_parameters()"
  # set to "all" here.
  if (identical(component, "conditional")) {
    component <- "all"
  }

  # profiled CIs may take a long time to compute, so we warn the user about it
  if (any(unlist(insight::n_obs(model)) > 1e4) && identical(ci_method, "profile")) {
    insight::format_alert(
      "Profiled confidence intervals may take longer time to compute.",
      "Use `ci_method=\"wald\"` for faster computation of CIs."
    )
  }

  # tell user that profiled CIs don't respect vcov-args
  if (identical(ci_method, "profile") && (!is.null(vcov) || !is.null(vcov_args)) && isTRUE(verbose)) {
    insight::format_alert(
      "When `ci_method=\"profile\"`, `vcov` only modifies standard errors, test-statistic and p-values, but not confidence intervals.", # nolint
      "Use `ci_method=\"wald\"` to return confidence intervals based on robust standard errors."
    )
  }

  fun_args <- list(
    model = model,
    ci = ci,
    ci_method = ci_method,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    include_info = include_info,
    keep_parameters = keep,
    drop_parameters = drop,
    vcov = vcov,
    vcov_args = vcov_args
  )
  fun_args <- c(fun_args, dots)
  out <- do.call(".model_parameters_generic", fun_args)

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.nestedLogit <- function(model,
                                       component = "all",
                                       vcov = NULL,
                                       vcov_args = NULL,
                                       verbose = TRUE,
                                       ...) {
  dots <- list(...)
  se <- NULL

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
    se <- unlist(lapply(.vcov, function(i) sqrt(diag(i))), use.names = FALSE)
  }

  # classical se from summary()
  if (is.null(se)) {
    se <- as.vector(as.data.frame(do.call(rbind, lapply(model$models, function(i) {
      stats::coef(summary(i))
    })))[, "Std. Error"])
  }

  # classical se from get_varcov()
  if (is.null(se)) {
    .vcov <- insight::get_varcov(
      model,
      component = component,
      verbose = verbose,
      ...
    )
    se <- unlist(lapply(.vcov, function(i) sqrt(diag(i))), use.names = FALSE)
  }

  params <- insight::get_parameters(model, component = component)
  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se),
    Response = params$Response,
    Component = params$Component
  )
}


#' @export
p_value.nestedLogit <- function(model,
                                dof = NULL,
                                method = NULL,
                                component = "all",
                                vcov = NULL,
                                vcov_args = NULL,
                                verbose = TRUE,
                                ...) {
  if (is.null(vcov)) {
    p <- as.vector(as.data.frame(do.call(rbind, lapply(model$models, function(i) {
      stats::coef(summary(i))
    })))[, "Pr(>|z|)"])
  } else {
    p <- p_value.default(
      model,
      dof = dof,
      method = method,
      component = component,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    )[["p"]]
  }

  params <- insight::get_parameters(model, component = component)
  .data_frame(
    Parameter = params$Parameter,
    p = p,
    Response = params$Response,
    Component = params$Component
  )
}


#' @export
ci.nestedLogit <- function(x,
                           ci = 0.95,
                           dof = NULL,
                           method = "profile",
                           component = "all",
                           vcov = NULL,
                           vcov_args = NULL,
                           verbose = TRUE,
                           ...) {
  out <- lapply(
    x$models,
    ci,
    dof = dof,
    method = method,
    vcov = vcov,
    vcov_args = vcov_args,
    verbose = verbose,
    ...
  )

  for (i in names(out)) {
    out[[i]]$Component <- i
  }

  out <- do.call(rbind, out)
  row.names(out) <- NULL

  if (!is.null(component) && !identical(component, "all")) {
    comp <- intersect(names(x$models), component)
    if (!length(comp) && verbose) {
      insight::format_alert(
        paste0(
          "No matching model found. Possible values for `component` are ",
          toString(paste0("\"", names(x$models), "\"")),
          "."
        )
      )
    } else {
      out <- out[out$Component %in% component, ]
    }
  }

  params <- insight::get_parameters(x, component = component)
  out$Response <- params$Response
  out[c("Parameter", "CI", "CI_low", "CI_high", "Response", "Component")]
}


#' @export
simulate_model.nestedLogit <- function(model, iterations = 1000, ...) {
  if (is.null(iterations)) iterations <- 1000

  params <- insight::get_parameters(model, component = "all", verbose = FALSE)
  varcov <- insight::get_varcov(model, component = "all", verbose = FALSE, ...)

  out <- lapply(unique(params$Component), function(i) {
    pars <- params[params$Component == i, ]
    betas <- stats::setNames(pars$Estimate, pars$Parameter)
    d <- as.data.frame(.mvrnorm(n = iterations, mu = betas, Sigma = varcov[[i]]))
    d$Component <- i
    d
  })

  out <- do.call(rbind, out)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
simulate_parameters.nestedLogit <- function(model,
                                            iterations = 1000,
                                            centrality = "median",
                                            ci = 0.95,
                                            ci_method = "quantile",
                                            test = "p-value",
                                            ...) {
  sim_data <- simulate_model(model, iterations = iterations, ...)

  out <- lapply(unique(sim_data$Component), function(i) {
    pars <- sim_data[sim_data$Component == i, ]
    d <- .summary_bootstrap(
      data = pars,
      test = test,
      centrality = centrality,
      ci = ci,
      ci_method = ci_method,
      ...
    )
    d$Component <- i
    d
  })
  out <- do.call(rbind, out)

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality
  attr(out, "simulated") <- TRUE

  out
}

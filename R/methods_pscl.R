# .zeroinfl, .hurdle, .zerocount

# model parameters -----------------

#' @export
model_parameters.zeroinfl <- model_parameters.zcpglm

#' @export
model_parameters.hurdle <- model_parameters.zcpglm

#' @export
model_parameters.zerocount <- model_parameters.zcpglm


# ci -----------------

#' @export
ci.zeroinfl <- function(x,
                        ci = 0.95,
                        dof = NULL,
                        method = "wald",
                        component = "all",
                        verbose = TRUE,
                        ...) {
  method <- tolower(method)
  method <- insight::validate_argument(
    method,
    c("wald", "normal", "residual", "robust")
  )
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated")
  )

  if (is.null(.check_component(x, component, verbose = verbose))) {
    return(NULL)
  }

  # all other
  .ci_generic(model = x, ci = ci, dof = dof, method = method, component = component, ...)
}

#' @export
ci.hurdle <- ci.zeroinfl

#' @export
ci.zerocount <- ci.zeroinfl


# standard error -----------------

#' @export
standard_error.zeroinfl <- function(model,
                                    component = "all",
                                    method = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated")
  )
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(standard_error.default(model, component = component, ...))
  }

  cs <- insight::compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    if (i == "count") {
      comp <- "conditional"
    } else {
      comp <- "zi"
    }

    stats <- cs[[i]]

    # remove log(theta)
    theta <- grepl("Log(theta)", rownames(stats), fixed = TRUE)
    if (any(theta)) {
      stats <- stats[!theta, ]
    }

    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      SE = as.vector(stats[, 2]),
      Component = comp
    )
  })

  se <- do.call(rbind, x)
  se$Component <- .rename_values(se$Component, "cond", "conditional")
  se$Component <- .rename_values(se$Component, "zi", "zero_inflated")

  .filter_component(se, component)
}

#' @export
standard_error.hurdle <- standard_error.zeroinfl

#' @export
standard_error.zerocount <- standard_error.zeroinfl


# p values -----------------------

#' @export
p_value.zeroinfl <- function(model, component = "all", method = NULL, verbose = TRUE, ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated")
  )
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(p_value.default(model, component = component, ...))
  }

  cs <- insight::compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    if (i == "count") {
      comp <- "conditional"
    } else {
      comp <- "zi"
    }
    stats <- cs[[i]]

    # remove log(theta)
    theta <- grepl("Log(theta)", rownames(stats), fixed = TRUE)
    if (any(theta)) {
      stats <- stats[!theta, ]
    }

    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      p = as.vector(stats[, 4]),
      Component = comp
    )
  })

  p <- do.call(rbind, x)
  p$Component <- .rename_values(p$Component, "cond", "conditional")
  p$Component <- .rename_values(p$Component, "zi", "zero_inflated")

  .filter_component(p, component)
}

#' @export
p_value.hurdle <- p_value.zeroinfl

#' @export
p_value.zerocount <- p_value.zeroinfl


# simulate model -----------------

#' @export
simulate_model.zeroinfl <- simulate_model.glmmTMB

#' @export
simulate_model.hurdle <- simulate_model.zeroinfl

#' @export
simulate_model.zerocount <- simulate_model.zeroinfl


# simulate paramaters -----------------

#' @export
simulate_parameters.zeroinfl <- function(model,
                                         iterations = 1000,
                                         centrality = "median",
                                         ci = 0.95,
                                         ci_method = "quantile",
                                         test = "p-value",
                                         ...) {
  data <- simulate_model(model, iterations = iterations, ...)
  out <-
    .summary_bootstrap(
      data = data,
      test = test,
      centrality = centrality,
      ci = ci,
      ci_method = ci_method,
      ...
    )

  params <- insight::get_parameters(model)
  if ("Effects" %in% colnames(params) && insight::n_unique(params$Effects) > 1) {
    out$Effects <- params$Effects
  }

  if ("Component" %in% colnames(params) && insight::n_unique(params$Component) > 1) {
    out$Component <- params$Component
  }

  if (inherits(model, c("zeroinfl", "hurdle", "zerocount"))) {
    out$Parameter <- gsub("^(count_|zero_)", "", out$Parameter)
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality

  out
}

#' @export
simulate_parameters.hurdle <- simulate_parameters.zeroinfl

#' @export
simulate_parameters.zerocount <- simulate_parameters.zeroinfl

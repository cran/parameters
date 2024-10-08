#' @export
model_parameters.mediate <- function(model, ci = 0.95, exponentiate = FALSE, verbose = TRUE, ...) {
  # Parameters, Estimate and CI
  params <- insight::get_parameters(model)

  # CI
  params <- merge(params, ci(model, ci = ci), by = "Parameter", sort = FALSE)
  params$CI <- NULL

  # p-value
  params <- merge(params, p_value(model), by = "Parameter", sort = FALSE)

  # ==== Renaming

  if (any(endsWith(params$Parameter, "(control)"))) {
    params$Component <- gsub("(.*)\\((.*)\\)$", "\\2", params$Parameter)
  }

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, verbose = verbose, ...)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
ci.mediate <- function(x, ci = 0.95, ...) {
  info <- insight::model_info(x$model.y, verbose = FALSE)
  alpha <- (1 + ci) / 2
  if (info$is_linear && !x$INT) {
    out <- data.frame(
      Parameter = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
      CI = ci,
      CI_low = c(
        stats::quantile(x$d0.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$z0.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$tau.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$n0.sims, probs = 1 - alpha, names = FALSE)
      ),
      CI_high = c(
        stats::quantile(x$d0.sims, probs = alpha, names = FALSE),
        stats::quantile(x$z0.sims, probs = alpha, names = FALSE),
        stats::quantile(x$tau.sims, probs = alpha, names = FALSE),
        stats::quantile(x$n0.sims, probs = alpha, names = FALSE)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      Parameter = c(
        "ACME (control)", "ACME (treated)", "ADE (control)",
        "ADE (treated)", "Total Effect", "Prop. Mediated (control)",
        "Prop. Mediated (treated)", "ACME (average)", "ADE (average)",
        "Prop. Mediated (average)"
      ),
      CI = ci,
      CI_low = c(
        stats::quantile(x$d0.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$d1.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$z0.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$z1.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$tau.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$n0.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$n1.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$d.avg.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$z.avg.sims, probs = 1 - alpha, names = FALSE),
        stats::quantile(x$n.avg.sims, probs = 1 - alpha, names = FALSE)
      ),
      CI_high = c(
        stats::quantile(x$d0.sims, probs = alpha, names = FALSE),
        stats::quantile(x$d1.sims, probs = alpha, names = FALSE),
        stats::quantile(x$z0.sims, probs = alpha, names = FALSE),
        stats::quantile(x$z1.sims, probs = alpha, names = FALSE),
        stats::quantile(x$tau.sims, probs = alpha, names = FALSE),
        stats::quantile(x$n0.sims, probs = alpha, names = FALSE),
        stats::quantile(x$n1.sims, probs = alpha, names = FALSE),
        stats::quantile(x$d.avg.sims, probs = alpha, names = FALSE),
        stats::quantile(x$z.avg.sims, probs = alpha, names = FALSE),
        stats::quantile(x$n.avg.sims, probs = alpha, names = FALSE)
      ),
      stringsAsFactors = FALSE
    )
  }
  out
}


#' @export
standard_error.mediate <- function(model, ...) {
  NULL
}


#' @export
p_value.mediate <- function(model, ...) {
  info <- insight::model_info(model$model.y, verbose = FALSE)
  if (info$is_linear && !model$INT) {
    out <- data.frame(
      Parameter = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
      p = c(model$d0.p, model$z0.p, model$tau.p, model$n0.p),
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      Parameter = c(
        "ACME (control)", "ACME (treated)", "ADE (control)", "ADE (treated)",
        "Total Effect", "Prop. Mediated (control)", "Prop. Mediated (treated)",
        "ACME (average)", "ADE (average)", "Prop. Mediated (average)"
      ),
      p = c(
        model$d0.p, model$d1.p, model$z0.p, model$z1.p, model$tau.p, model$n0.p,
        model$n1.p, model$d.avg.p, model$z.avg.p, model$n.avg.p
      ),
      stringsAsFactors = FALSE
    )
  }
  out
}


#' @export
format_parameters.mediate <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  params <- insight::trim_ws(gsub("(.*)\\((.*)\\)$", "\\1", params))
  names(params) <- params
  params[params == "ACME"] <- "Indirect Effect (ACME)"
  params[params == "ADE"] <- "Direct Effect (ADE)"
  params
}

#' @export
model_parameters.lqmm <- function(model,
                                  ci = 0.95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  p_adjust = NULL,
                                  verbose = TRUE,
                                  ...) {
  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(
      model,
      iterations = iterations,
      ci = ci,
      ...
    )
  } else {
    parameters <- .extract_parameters_lqmm(
      model,
      ci = ci,
      p_adjust = p_adjust,
      verbose = verbose,
      ...
    )
  }

  parameters <- .add_model_parameters_attributes(
    parameters,
    model,
    ci,
    exponentiate = FALSE,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  attr(parameters, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @export
model_parameters.lqm <- model_parameters.lqmm


#' @export
ci.lqmm <- function(x, ...) {
  out <- model_parameters(x, ...)
  as.data.frame(out[c("Parameter", "CI_low", "CI_high")])
}


#' @export
ci.lqm <- ci.lqmm


#' @export
standard_error.lqmm <- function(model, ...) {
  out <- model_parameters(model, ...)
  as.data.frame(out[c("Parameter", "SE")])
}


#' @export
standard_error.lqm <- standard_error.lqmm


#' @export
p_value.lqmm <- function(model, ...) {
  out <- model_parameters(model, ...)
  as.data.frame(out[c("Parameter", "p")])
}


#' @export
p_value.lqm <- p_value.lqmm


# helper ------------------


.extract_parameters_lqmm <- function(model, ci, p_adjust, verbose = TRUE, ...) {
  cs <- summary(model)
  parameters <- insight::get_parameters(model)

  if (is.list(cs$tTable)) {
    summary_table <- do.call(rbind, cs$tTable)
  } else {
    summary_table <- cs$tTable
  }


  # ==== Coefficient, SE and test statistic

  parameters$Coefficient <- parameters$Estimate
  parameters$SE <- summary_table[, 2]
  parameters$t <- parameters$Estimate / parameters$SE

  # ==== DF

  parameters$df_error <- tryCatch(
    {
      if (!is.null(cs$rdf)) {
        cs$rdf
      } else {
        attr(cs$B, "R") - 1
      }
    },
    error = function(e) {
      Inf
    }
  )

  # ==== Conf Int
  parameters$CI_low <- parameters$Coefficient - stats::qt((1 + ci) / 2, df = parameters$df_error) * parameters$SE
  parameters$CI_high <- parameters$Coefficient + stats::qt((1 + ci) / 2, df = parameters$df_error) * parameters$SE

  # ==== p-value

  parameters$p <- summary_table[, 5]

  if (!is.null(p_adjust)) {
    parameters <- .p_adjust(parameters, p_adjust, model, verbose)
  }

  # ==== Reorder

  col_order <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t", "df_error", "p", "Component")
  parameters[col_order[col_order %in% names(parameters)]]
}

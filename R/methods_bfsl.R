#' @export
model_parameters.bfsl <- function(model,
                                  ci = 0.95,
                                  ci_method = "residual",
                                  p_adjust = NULL,
                                  summary = getOption("parameters_summary", FALSE),
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    merge_by = "Parameter",
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    summary = summary,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.bfsl <- function(model, ...) {
  cf <- stats::coef(model)

  params <- data.frame(
    Parameter = rownames(cf),
    SE = unname(cf[, "Std. Error"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  insight::text_remove_backticks(params, verbose = FALSE)
}



#' @export
degrees_of_freedom.bfsl <- function(model, method = "residual", ...) {
  if (is.null(method)) {
    method <- "wald"
  }

  method <- match.arg(tolower(method), choices = c("analytical", "any", "fit", "wald", "residual", "normal"))

  if (method %in% c("wald", "residual", "fit")) {
    model$df.residual
  } else {
    degrees_of_freedom.default(model, method = method, ...)
  }
}

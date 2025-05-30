#' @export
model_parameters.deltaMethod <- function(model, p_adjust = NULL, verbose = TRUE, ...) {
  dots <- list(...)
  if ("ci" %in% names(dots)) {
    insight::format_warning(
      "The `ci` argument is not supported by `model_parameters` for objects of this class. Use the `level` argument of the `deltaMethod` function instead." # nolint
    )
    dots[["ci"]] <- NULL
  }

  # tweak column names
  params <- insight::standardize_names(datawizard::rownames_as_column(model, "Parameter"))

  # find CIs
  ci_cols <- endsWith(colnames(params), "%")
  cis <- as.numeric(gsub("%", "", colnames(params)[ci_cols], fixed = TRUE)) / 100
  ci <- diff(cis)

  # rename CI columns
  colnames(params)[ci_cols] <- c("CI_low", "CI_high")

  # check if statistic is available
  if (is.null(params$Statistic)) {
    params <- merge(params, insight::get_statistic(model), by = "Parameter", sort = FALSE)
  }

  # check if statistic is available
  if (is.null(params$p)) {
    params$p <- as.vector(2 * stats::pnorm(abs(params$Statistic), lower.tail = FALSE))
  }

  # rename statistic column
  names(params) <- gsub("Statistic", "z", names(params), fixed = TRUE)

  # adjust p?
  if (!is.null(p_adjust)) {
    params <- .p_adjust(params, p_adjust, model, verbose)
  }

  fun_args <- list(
    params,
    model,
    ci = ci,
    exponentiate = FALSE,
    bootstrap = FALSE,
    iterations = NULL,
    ci_method = "residual",
    p_adjust = p_adjust,
    include_info = FALSE,
    verbose = verbose
  )
  fun_args <- c(fun_args, dots)

  params <- do.call(".add_model_parameters_attributes", fun_args)

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(params, "no_caption") <- TRUE
  params
}


#' @export
ci.deltaMethod <- function(x, ...) {
  params <- model_parameters(x, ...)
  ci <- attributes(params)$ci
  params$CI <- ci
  as.data.frame(params[c("Parameter", "CI", "CI_low", "CI_high")])
}


#' @export
standard_error.deltaMethod <- function(model, ...) {
  params <- model_parameters(model, ...)
  as.data.frame(params[c("Parameter", "SE")])
}


#' @export
p_value.deltaMethod <- function(model, ...) {
  params <- model_parameters(model, ...)
  if (is.null(params[["p"]])) {
    return(NULL)
  }
  as.data.frame(params[c("Parameter", "p")])
}

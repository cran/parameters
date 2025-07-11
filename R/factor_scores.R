#' Extract factor scores from Factor Analysis (EFA) or Omega
#'
#' `factor_scores()` extracts the factor scores from objects returned by
#' [`psych::fa()`], [`factor_analysis()`], or [`psych::omega()`]
#'
#' @param x An object returned by [`psych::fa()`], [`factor_analysis()`], or
#' [`psych::omega()`].
#' @param ... Currently unused.
#'
#' @return A data frame with the factor scores. It simply extracts the `$scores`
#' element from the object and converts it into a data frame.
#'
#' @seealso [`factor_analysis()`]
#'
#' @examplesIf insight::check_if_installed("psych", quietly = TRUE)
#' data(mtcars)
#' out <- factor_analysis(mtcars[, 1:7], n = 2)
#' head(factor_scores(out))
#'
#' @export
factor_scores <- function(x, ...) {
  UseMethod("factor_scores")
}

#' @export
factor_scores.fa <- function(x, ...) {
  as.data.frame(x$scores)
}

#' @export
factor_scores.omega <- function(x, ...) {
  as.data.frame(x$scores)
}

#' @export
factor_scores.parameters_efa <- function(x, ...) {
  model <- attributes(x)$model
  if (is.null(model)) {
    insight::format_error("The `model` attribute is missing from the input object.")
  }
  as.data.frame(model$scores)
}

#' @export
factor_scores.parameters_omega <- factor_scores.parameters_efa

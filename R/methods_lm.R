# lm: .lm, .summary.lm

# .lm ---------------------


#' @export
p_value.lm <- p_value.default


#' @export
ci.lm <- function(x, ci = 0.95, method = "residual", ...) {
  .ci_generic(model = x, ci = ci, method = method, ...)
}


# .summary.lm ---------------------

#' @export
standard_error.summary.lm <- function(model, ...) {
  cs <- stats::coef(model)

  data.frame(
    Parameter = rownames(cs),
    SE = as.vector(cs[, 2]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
p_value.summary.lm <- function(model, ...) {
  cs <- stats::coef(model)

  data.frame(
    Parameter = rownames(cs),
    p = as.vector(cs[, 4]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
ci.summary.lm <- function(x, ci = 0.95, method = "residual", ...) {
  .ci_generic(model = x, ci = ci, method = method, dof = insight::get_df(x), ...)
}

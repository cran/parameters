#' @rdname principal_components
#' @export
factor_analysis <- function(x,
                            n = "auto",
                            rotation = "none",
                            sort = FALSE,
                            threshold = NULL,
                            standardize = TRUE,
                            cor = NULL,
                            ...) {
  UseMethod("factor_analysis")
}


#' @export
factor_analysis.data.frame <- function(x,
                                       n = "auto",
                                       rotation = "none",
                                       sort = FALSE,
                                       threshold = NULL,
                                       standardize = TRUE,
                                       cor = NULL,
                                       ...) {
  # Standardize
  if (standardize && is.null(cor)) {
    x <- datawizard::standardize(x, ...)
  }

  # N factors
  n <- .get_n_factors(x, n = n, type = "FA", rotation = rotation, cor = cor)

  .factor_analysis_rotate(
    x,
    n,
    rotation = rotation,
    sort = sort,
    threshold = threshold,
    cor = cor,
    ...
  )
}


#' @keywords internal
.factor_analysis_rotate <- function(x,
                                    n,
                                    rotation,
                                    sort = FALSE,
                                    threshold = NULL,
                                    cor = NULL,
                                    ...) {
  if (!inherits(x, "data.frame")) {
    insight::format_error("`x` must be a data frame.")
  }

  # rotate loadings
  if (!requireNamespace("psych", quietly = TRUE)) {
    insight::format_error(sprintf("Package `psych` required for `%s`-rotation.", rotation))
  }

  # Pass cor if available
  if (is.null(cor)) {
    out <- model_parameters(
      psych::fa(x, nfactors = n, rotate = rotation, ...),
      sort = sort,
      threshold = threshold
    )
  } else {
    out <- model_parameters(
      psych::fa(
        cor,
        nfactors = n,
        rotate = rotation,
        n.obs = nrow(x),
        ...
      ),
      sort = sort,
      threshold = threshold
    )
  }

  attr(out, "dataset") <- x
  out
}

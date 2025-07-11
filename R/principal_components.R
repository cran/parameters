#' Principal Component Analysis (PCA) and Factor Analysis (FA)
#'
#' The functions `principal_components()` and `factor_analysis()` can be used to
#' perform a principal component analysis (PCA) or a factor analysis (FA). They
#' return the loadings as a data frame, and various methods and functions are
#' available to access / display other information (see the 'Details' section).
#'
#' @param x A data frame or a statistical model. For `closest_component()`, the
#'   output of the `principal_components()` function.
#' @param n Number of components to extract. If `n="all"`, then `n` is set as
#'   the number of variables minus 1 (`ncol(x)-1`). If `n="auto"` (default) or
#'   `n=NULL`, the number of components is selected through [`n_factors()`]
#'   resp. [`n_components()`]. Else, if `n` is a number, `n` components are
#'   extracted. If `n` exceeds number of variables in the data, it is
#'   automatically set to the maximum number (i.e. `ncol(x)`). In
#'   [`reduce_parameters()`], can also be `"max"`, in which case it will select
#'   all the components that are maximally pseudo-loaded (i.e., correlated) by
#'   at least one variable.
#' @param rotation If not `"none"`, the PCA / FA will be computed using the
#'   **psych** package. Possible options include `"varimax"`, `"quartimax"`,
#'   `"promax"`, `"oblimin"`, `"simplimax"`, or `"cluster"` (and more). See
#'   [`psych::fa()`] for details. The default is `"none"` for PCA, and
#'   `"oblimin"` for FA.
#' @param factor_method The factoring method to be used. Passed to the `fm`
#'   argument in `psych::fa()`. Defaults to `"minres"` (minimum residual). Other
#'   options include `"uls"`, `"ols"`, `"wls"`, `"gls"`, `"ml"`, `"minchi"`,
#'   `"minrank"`, `"old.min"`, and `"alpha"`. See `?psych::fa` for details.
#' @param sparse Whether to compute sparse PCA (SPCA, using [`sparsepca::spca()`]).
#'   SPCA attempts to find sparse loadings (with few nonzero values), which improves
#'   interpretability and avoids overfitting. Can be `TRUE` or `"robust"` (see
#'   [`sparsepca::robspca()`]).
#' @param sort Sort the loadings.
#' @param n_obs An integer or a matrix.
#'   - **Integer:** Number of observations in the original data set if `x` is a
#'     correlation matrix. Required to compute correct fit indices.
#'   - **Matrix:** A matrix where each cell `[i, j]` specifies the number of
#'     pairwise complete observations used to compute the correlation between
#'     variable `i` and variable `j` in the input `x`. It is crucial when `x` is
#'     a correlation matrix (rather than raw data), especially if that matrix
#'     was derived from a dataset containing missing values using pairwise
#'     deletion. Providing a matrix allows `psych::fa()` to accurately calculate
#'     statistical measures, such as chi-square fit statistics, by accounting
#'     for the varying sample sizes that contribute to each individual
#'     correlation coefficient.
#' @param threshold A value between 0 and 1 indicates which (absolute) values
#'   from the loadings should be removed. An integer higher than 1 indicates the
#'   n strongest loadings to retain. Can also be `"max"`, in which case it will
#'   only display the maximum loading per variable (the most simple structure).
#' @param standardize A logical value indicating whether the variables should be
#'   standardized (centered and scaled) to have unit variance before the
#'   analysis (in general, such scaling is advisable). **Note:** This defaults
#'   to `TRUE` for PCA, but to `FALSE` for FA (because `factor_analysis()`
#'   computes a correlation matrix and uses that r-matrix for the factor analysis
#'   by default - therefore, standardization of the raw variables is unnecessary,
#'   and even undesirable when using `cor = "poly"`).
#' @param object An object of class `parameters_pca`, `parameters_efa` or
#'   `psych_efa`.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param names Optional character vector to name columns of the returned data
#'   frame.
#' @param keep_na Logical, if `TRUE`, predictions also return observations
#'   with missing values from the original data, hence the number of rows of
#'   predicted data and original data is equal.
#' @param ... Arguments passed to or from other methods.
#' @param digits Argument for `print()`, indicates the number of digits
#'   (rounding) to be used.
#' @param labels Argument for `print()`, character vector of same length as
#'   columns in `x`. If provided, adds an additional column with the labels.
#' @param verbose Toggle warnings.
#'
#' @details
#'
#' ## Methods and Utilities
#' - [`n_components()`] and [`n_factors()`] automatically estimates the optimal
#'   number of dimensions to retain.
#'
#' - [`performance::check_factorstructure()`] checks the suitability of the
#'   data for factor analysis using the sphericity (see
#'   [`performance::check_sphericity_bartlett()`]) and the KMO (see
#'   [`performance::check_kmo()`]) measure.
#'
#' - [`performance::check_itemscale()`] computes various measures of internal
#'   consistencies applied to the (sub)scales (i.e., components) extracted from
#'   the PCA.
#'
#' - Running `summary()` returns information related to each component/factor,
#'   such as the explained variance and the Eivenvalues.
#'
#' - Running [`get_scores()`] computes scores for each subscale.
#'
#' - [`factor_scores()`] extracts the factor scores from objects returned by
#'   [`psych::fa()`], [`factor_analysis()`], or [`psych::omega()`].
#'
#' - Running [`closest_component()`] will return a numeric vector with the
#'   assigned component index for each column from the original data frame.
#'
#' - Running [`rotated_data()`] will return the rotated data, including missing
#'   values, so it matches the original data frame.
#'
#' - `performance::item_omega()` is a convenient wrapper around `psych::omega()`,
#'   which provides some additional methods to work seamlessly within the
#'   *easystats* framework.
#'
#' - [`performance::check_normality()`] checks residuals from objects returned
#'   by [`psych::fa()`], [`factor_analysis()`], `performance::item_omega()`,
#'   or [`psych::omega()`] for normality.
#'
#' - [`performance::model_performance()`] returns fit-indices for objects returned
#'   by [`psych::fa()`], [`factor_analysis()`], or [`psych::omega()`].
#'
#' - Running
#'   [`plot()`](https://easystats.github.io/see/articles/parameters.html#principal-component-analysis)
#'   visually displays the loadings (that requires the
#'   [**see**-package](https://easystats.github.io/see/) to work).
#'
#' ## Complexity
#' Complexity represents the number of latent components needed to account
#' for the observed variables. Whereas a perfect simple structure solution
#' has a complexity of 1 in that each item would only load on one factor,
#' a solution with evenly distributed items has a complexity greater than 1
#' (_Hofman, 1978; Pettersson and Turkheimer, 2010_).
#'
#' ## Uniqueness
#' Uniqueness represents the variance that is 'unique' to the variable and
#'  not shared with other variables. It is equal to `1 - communality`
#'  (variance that is shared with other variables). A uniqueness of `0.20`
#'  suggests that `20%` or that variable's variance is not shared with other
#'  variables in the overall factor model. The greater 'uniqueness' the lower
#'  the relevance of the variable in the factor model.
#'
#' ## MSA
#' MSA represents the Kaiser-Meyer-Olkin Measure of Sampling Adequacy
#' (_Kaiser and Rice, 1974_) for each item. It indicates whether there is
#' enough data for each factor give reliable results for the PCA. The value
#' should be > 0.6, and desirable values are > 0.8 (_Tabachnick and Fidell, 2013_).
#'
#' ## PCA or FA?
#' There is a simplified rule of thumb that may help do decide whether to run
#' a factor analysis or a principal component analysis:
#'
#' - Run *factor analysis* if you assume or wish to test a theoretical model of
#'   *latent factors* causing observed variables.
#'
#' - Run *principal component analysis* If you want to simply *reduce* your
#'   correlated observed variables to a smaller set of important independent
#'   composite variables.
#'
#'  (Source: [CrossValidated](https://stats.stackexchange.com/q/1576/54740))
#'
#' ## Computing Item Scores
#' Use [`get_scores()`] to compute scores for the "subscales" represented by the
#' extracted principal components or factors. `get_scores()` takes the results
#' from `principal_components()` or `factor_analysis()` and extracts the
#' variables for each component found by the PCA. Then, for each of these
#' "subscales", raw means are calculated (which equals adding up the single
#' items and dividing by the number of items). This results in a sum score for
#' each component from the PCA, which is on the same scale as the original,
#' single items that were used to compute the PCA. One can also use `predict()`
#' to back-predict scores for each component, to which one can provide `newdata`
#' or a vector of `names` for the components.
#'
#' ## Explained Variance and Eingenvalues
#' Use `summary()` to get the Eigenvalues and the explained variance for each
#' extracted component. The eigenvectors and eigenvalues represent the "core"
#' of a PCA: The eigenvectors (the principal components) determine the
#' directions of the new feature space, and the eigenvalues determine their
#' magnitude. In other words, the eigenvalues explain the variance of the
#' data along the new feature axes.
#'
#' @return A data frame of loadings. For `factor_analysis()`, this data frame is
#' also of class `parameters_efa()`. Objects from `principal_components()` are
#' of class `parameters_pca()`.
#'
#' @references
#' - Kaiser, H.F. and Rice. J. (1974). Little jiffy, mark iv. Educational
#'   and Psychological Measurement, 34(1):111–117
#'
#' - Hofmann, R. (1978). Complexity and simplicity as objective indices
#'   descriptive of factor solutions. Multivariate Behavioral Research, 13:2,
#'   247-250, \doi{10.1207/s15327906mbr1302_9}
#'
#' - Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation,
#'   and simple structure in personality data. Journal of research in
#'   personality, 44(4), 407-420, \doi{10.1016/j.jrp.2010.03.002}
#'
#' - Tabachnick, B. G., and Fidell, L. S. (2013). Using multivariate
#'   statistics (6th ed.). Boston: Pearson Education.
#'
#' @examplesIf require("nFactors", quietly = TRUE) && require("sparsepca", quietly = TRUE) && require("psych", quietly = TRUE)
#' library(parameters)
#'
#' \donttest{
#' # Principal Component Analysis (PCA) -------------------
#' principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#'
#' # Automated number of components
#' principal_components(mtcars[, 1:4], n = "auto")
#'
#' # labels can be useful if variable names are not self-explanatory
#' print(
#'   principal_components(mtcars[, 1:4], n = "auto"),
#'   labels = c(
#'     "Miles/(US) gallon",
#'     "Number of cylinders",
#'     "Displacement (cu.in.)",
#'     "Gross horsepower"
#'   )
#' )
#'
#' # Sparse PCA
#' principal_components(mtcars[, 1:7], n = 4, sparse = TRUE)
#' principal_components(mtcars[, 1:7], n = 4, sparse = "robust")
#'
#' # Rotated PCA
#' principal_components(mtcars[, 1:7],
#'   n = 2, rotation = "oblimin",
#'   threshold = "max", sort = TRUE
#' )
#' principal_components(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)
#'
#' pca <- principal_components(mtcars[, 1:5], n = 2, rotation = "varimax")
#' pca # Print loadings
#' summary(pca) # Print information about the factors
#' predict(pca, names = c("Component1", "Component2")) # Back-predict scores
#'
#' # which variables from the original data belong to which extracted component?
#' closest_component(pca)
#' }
#'
#' # Factor Analysis (FA) ------------------------
#'
#' factor_analysis(mtcars[, 1:7], n = "all", threshold = 0.2, rotation = "Promax")
#' factor_analysis(mtcars[, 1:7], n = 2, threshold = "max", sort = TRUE)
#' factor_analysis(mtcars[, 1:7], n = 2, rotation = "none", threshold = 2, sort = TRUE)
#'
#' efa <- factor_analysis(mtcars[, 1:5], n = 2)
#' summary(efa)
#' predict(efa, verbose = FALSE)
#'
#' \donttest{
#' # Automated number of components
#' factor_analysis(mtcars[, 1:4], n = "auto")
#' }
#'
#' @export
principal_components <- function(x, ...) {
  UseMethod("principal_components")
}


#' @rdname principal_components
#' @export
rotated_data <- function(x, verbose = TRUE) {
  original_data <- attributes(x)$dataset
  rotated_matrix <- insight::get_predicted(attributes(x)$model)
  out <- NULL

  if (is.null(original_data) || is.null(rotated_matrix)) {
    if (verbose) {
      insight::format_warning("Either the original or the rotated data could not be retrieved.")
    }
    return(NULL)
  }

  compl_cases <- attributes(x)$complete_cases
  if (is.null(compl_cases) && nrow(original_data) != nrow(rotated_matrix)) {
    if (verbose) {
      insight::format_warning("Could not retrieve information about missing data.")
    }
    return(NULL)
  }

  original_data$.parameters_merge_id <- seq_len(nrow(original_data))
  rotated_matrix$.parameters_merge_id <- (seq_len(nrow(original_data)))[compl_cases]
  out <- merge(original_data, rotated_matrix, by = ".parameters_merge_id", all = TRUE, sort = FALSE)
  out$.parameters_merge_id <- NULL

  out
}


#' @rdname principal_components
#' @export
principal_components.data.frame <- function(x,
                                            n = "auto",
                                            rotation = "none",
                                            sparse = FALSE,
                                            sort = FALSE,
                                            threshold = NULL,
                                            standardize = TRUE,
                                            ...) {
  # save name of data set
  data_name <- insight::safe_deparse_symbol(substitute(x))

  # original data
  original_data <- x

  # remove missing
  x <- stats::na.omit(x)

  # Select numeric only
  x <- x[vapply(x, is.numeric, TRUE)]

  # N factors
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = rotation)

  # Catch and compute Rotated PCA
  if (rotation != "none") {
    if (sparse) {
      insight::format_error("Sparse PCA is currently incompatible with rotation. Use either `sparse=TRUE` or `rotation`.")
    }

    pca_loadings <- .pca_rotate(
      x,
      n,
      rotation = rotation,
      sort = sort,
      threshold = threshold,
      original_data = original_data,
      ...
    )

    attr(pca_loadings, "data") <- data_name
    attr(pca_loadings, "dataset") <- original_data

    return(pca_loadings)
  }

  # Compute PCA
  if (is.character(sparse) && sparse == "robust") {
    # Robust sparse PCA
    insight::check_if_installed("sparsepca")

    model <- sparsepca::robspca(
      x,
      center = standardize,
      scale = standardize,
      verbose = FALSE,
      ...
    )
    model$rotation <- model$loadings
    row.names(model$rotation) <- names(x)
    model$x <- model$scores
  } else if (isTRUE(sparse)) {
    # Sparse PCA
    insight::check_if_installed("sparsepca")

    model <- sparsepca::spca(
      x,
      center = standardize,
      scale = standardize,
      verbose = FALSE,
      ...
    )
    model$rotation <- stats::setNames(model$loadings, names(x))
    row.names(model$rotation) <- names(x)
    model$x <- model$scores
  } else {
    # Normal PCA
    model <- stats::prcomp(x,
      retx = TRUE,
      center = standardize,
      scale. = standardize,
      ...
    )
  }


  # Re-add centers and scales
  # if (standardize) {
  #   model$center <- attributes(x)$center
  #   model$scale <- attributes(x)$scale
  # }

  # Summary (cumulative variance etc.)
  eigenvalues <- model$sdev^2
  data_summary <- .data_frame(
    Component = sprintf("PC%i", seq_len(length(model$sdev))),
    Eigenvalues = eigenvalues,
    Variance = eigenvalues / sum(eigenvalues),
    Variance_Cumulative = cumsum(eigenvalues / sum(eigenvalues))
  )
  data_summary$Variance_Proportion <- data_summary$Variance / sum(data_summary$Variance)

  # Sometimes if too large n is requested the returned number is lower, so we
  # have to adjust n to the new number
  n <- pmin(sum(!is.na(model$sdev)), n)

  model$sdev <- model$sdev[1:n]
  model$rotation <- model$rotation[, 1:n, drop = FALSE]
  model$x <- model$x[, 1:n, drop = FALSE]
  data_summary <- data_summary[1:n, , drop = FALSE]


  # Compute loadings
  if (length(model$sdev) > 1) {
    pca_loadings <- as.data.frame(model$rotation %*% diag(model$sdev))
  } else {
    pca_loadings <- as.data.frame(model$rotation %*% model$sdev)
  }
  names(pca_loadings) <- data_summary$Component


  # Format
  pca_loadings <- cbind(data.frame(Variable = row.names(pca_loadings)), pca_loadings)
  row.names(pca_loadings) <- NULL

  # Add information
  loading_cols <- 2:(n + 1)
  pca_loadings$Complexity <- (apply(pca_loadings[, loading_cols, drop = FALSE], 1, function(x) sum(x^2)))^2 /
    apply(pca_loadings[, loading_cols, drop = FALSE], 1, function(x) sum(x^4))

  # Add attributes
  attr(pca_loadings, "summary") <- data_summary
  attr(pca_loadings, "model") <- model
  attr(pca_loadings, "rotation") <- "none"
  attr(pca_loadings, "scores") <- model$x
  attr(pca_loadings, "standardize") <- standardize
  attr(pca_loadings, "additional_arguments") <- list(...)
  attr(pca_loadings, "n") <- n
  attr(pca_loadings, "type") <- "prcomp"
  attr(pca_loadings, "loadings_columns") <- loading_cols
  attr(pca_loadings, "complete_cases") <- stats::complete.cases(original_data)

  # Sorting
  if (isTRUE(sort)) {
    pca_loadings <- .sort_loadings(pca_loadings)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    pca_loadings <- .filter_loadings(pca_loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(pca_loadings, "loadings_long") <- .long_loadings(pca_loadings, threshold = threshold)
  # here we match the original columns in the data set with the assigned components
  # for each variable, so we know which column in the original data set belongs
  # to which extracted component...

  attr(pca_loadings, "closest_component") <- .closest_component(
    pca_loadings,
    loadings_columns = loading_cols,
    variable_names = colnames(x)
  )
  attr(pca_loadings, "data") <- data_name
  attr(pca_loadings, "dataset") <- original_data

  # add class-attribute for printing
  class(pca_loadings) <- unique(c("parameters_pca", "see_parameters_pca", class(pca_loadings)))

  pca_loadings
}


#' @keywords internal
.get_n_factors <- function(x,
                           n = NULL,
                           type = "PCA",
                           rotation = "varimax",
                           ...) {
  # N factors
  if (is.null(n) || n == "auto") {
    n <- as.numeric(n_factors(x, type = type, rotation = rotation, ...))
  } else if (n == "all") {
    n <- ncol(x) - 1
  } else if (n >= ncol(x)) {
    n <- ncol(x)
  } else if (n < 1) {
    n <- 1
  }
  n
}


#' @keywords internal
.pca_rotate <- function(x,
                        n,
                        rotation,
                        sort = FALSE,
                        threshold = NULL,
                        original_data = NULL,
                        ...) {
  rotation <- insight::validate_argument(
    rotation,
    c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none")
  )

  if (!inherits(x, c("prcomp", "data.frame"))) {
    insight::format_error("`x` must be of class `prcomp` or a data frame.")
  }

  if (!inherits(x, "data.frame") && rotation != "varimax") {
    insight::format_error(sprintf("`x` must be a data frame for `%s`-rotation.", rotation))
  }

  # rotate loadings
  insight::check_if_installed("psych", reason = sprintf("`%s`-rotation.", rotation))

  pca <- psych::principal(x, nfactors = n, rotate = rotation, ...)
  msa <- psych::KMO(x)

  attr(pca, "MSA") <- msa$MSAi
  out <- model_parameters(pca, sort = sort, threshold = threshold)

  attr(out, "dataset") <- original_data
  attr(out, "complete_cases") <- stats::complete.cases(original_data)
  out
}

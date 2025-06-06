#' @rdname p_value_ml1
#' @export
dof_ml1 <- function(model) {
  if (!insight::is_mixed_model(model)) {
    insight::format_error("Model must be a mixed model.")
  }

  re_groups <- insight::get_random(model)

  parameters <- insight::find_parameters(model, effects = "fixed")[["conditional"]]
  predictors <- insight::find_predictors(model, effects = "fixed", component = "conditional", flatten = TRUE)
  predictors <- setdiff(predictors, names(re_groups))

  model_data <- insight::get_data(model, verbose = FALSE)[predictors]
  has_intcp <- insight::has_intercept(model)

  term_assignment <- .find_term_assignment(model_data, predictors, parameters)

  ddf <- sapply(model_data, function(.x) {
    min(vapply(re_groups, .get_df_ml1_approx, numeric(1), x = .x))
  })

  ltab <- table(ddf)
  ltab <- list(m = as.integer(names(ltab)), l = as.vector(ltab))

  ltab$ddf <- ltab$m - ltab$l
  if (has_intcp) ltab$ddf <- ltab$ddf - 1

  ii <- match(ddf, ltab$m)
  ddf[] <- ltab$ddf[ii]

  out <- numeric(length = length(parameters))
  ## FIXME: number of items to replace is not a multiple of replacement length
  suppressWarnings(out[which("(Intercept)" != parameters)] <- ddf[term_assignment]) # nolint
  if (has_intcp) out[which("(Intercept)" == parameters)] <- min(ddf)

  stats::setNames(out, parameters)
}


.get_df_ml1_approx <- function(x, g) {
  if (!is.factor(g)) {
    g <- as.factor(g)
  }
  m <- nlevels(g)
  n <- length(x)
  if (is.character(x)) {
    x <- as.numeric(as.factor(x))
  } else {
    x <- as.numeric(x)
  }
  x.bar <- stats::ave(x, g)
  var.within <- stats::var(x - x.bar)
  var.between <- stats::var(x.bar)
  if (var.within >= var.between) {
    n
  } else {
    m
  }
}


.find_term_assignment <- function(model_data, predictors, parameters) {
  parms <- unlist(lapply(seq_along(predictors), function(i) {
    p <- predictors[i]
    if (is.factor(model_data[[p]])) {
      ps <- paste0(p, levels(model_data[[p]]))
      names(ps)[seq_along(ps)] <- i
      ps
    } else {
      names(p) <- i
      p
    }
  }))
  out <- as.numeric(names(parms)[match(insight::clean_names(parameters), parms)])
  out[!is.na(out)]
}

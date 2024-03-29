#' @export
model_parameters.SemiParBIV <- function(model,
                                        ci = 0.95,
                                        bootstrap = FALSE,
                                        iterations = 1000,
                                        standardize = NULL,
                                        exponentiate = FALSE,
                                        p_adjust = NULL,
                                        keep = NULL,
                                        drop = NULL,
                                        verbose = TRUE,
                                        ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    component = "all",
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    keep_parameters = keep,
    drop_parameters = drop,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
p_value.SemiParBIV <- function(model, ...) {
  s <- summary(model)
  s <- insight::compact_list(s[startsWith(names(s), "tableP")])
  params <- do.call(rbind, lapply(seq_along(s), function(i) {
    out <- as.data.frame(s[[i]])
    out$Parameter <- rownames(out)
    out$Component <- paste0("Equation", i)
    out
  }))
  colnames(params)[4] <- "p"
  rownames(params) <- NULL
  insight::text_remove_backticks(params[c("Parameter", "p", "Component")], verbose = FALSE)
}


#' @export
standard_error.SemiParBIV <- function(model, ...) {
  s <- summary(model)
  s <- insight::compact_list(s[startsWith(names(s), "tableP")])
  params <- do.call(rbind, lapply(seq_along(s), function(i) {
    out <- as.data.frame(s[[i]])
    out$Parameter <- rownames(out)
    out$Component <- paste0("Equation", i)
    out
  }))
  colnames(params)[2] <- "SE"
  rownames(params) <- NULL
  insight::text_remove_backticks(params[c("Parameter", "SE", "Component")], verbose = FALSE)
}

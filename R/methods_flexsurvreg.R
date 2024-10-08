#' @export
standard_error.flexsurvreg <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  se <- model$res[rownames(model$res) %in% params, "se"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


## TODO add ci_method later?

#' @export
p_value.flexsurvreg <- function(model, ...) {
  params <- insight::get_parameters(model)
  est <- params$Estimate
  se <- standard_error(model)$SE
  p <- 2 * stats::pt(abs(est / se), df = insight::get_df(x = model, type = "wald"), lower.tail = FALSE)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p)
  )
}

# Package nlme; .lme, .gls

############### .lme --------------

#' @export
model_parameters.lme <- model_parameters.merMod


#' @export
ci.lme <- function(x,
                   ci = 0.95,
                   vcov = NULL,
                   vcov_args = NULL,
                   method = "wald",
                   ...) {
  method <- tolower(method)
  method <- match.arg(method, choices = c("wald", "normal", "residual", "betwithin", "ml1"))

  if (method %in% c("wald", "residual", "normal")) {
    # `vcov` argument must be computed using the `.ci_generic` function.
    # note that this uses `dof()`, which produces slightly different results than the stock degrees of freedom
    if (!is.null(vcov) || !requireNamespace("nlme", quietly = TRUE)) {
      .ci_generic(model = x, ci = ci, method = method, vcov = vcov, vcov_args = vcov_args, ...)
    } else {
      insight::check_if_installed("nlme")
      out <- lapply(ci, function(i) {
        ci_list <- tryCatch(
          {
            nlme::intervals(x, level = i, ...)
          },
          error = function(e) {
            nlme::intervals(x, level = i, which = "fixed", ...)
          }
        )
        .data_frame(
          Parameter = rownames(ci_list$fixed),
          CI = i,
          CI_low = as.vector(ci_list$fixed[, "lower"]),
          CI_high = as.vector(ci_list$fixed[, "upper"])
        )
      })
      insight::text_remove_backticks(do.call(rbind, out), verbose = FALSE)
    }
    # ml1 approx
  } else if (method == "ml1") {
    ci_ml1(x, ci)

    # betwithin approx
  } else if (method == "betwithin") {
    ci_betwithin(x, ci)
  }
}


#' @export
p_value.lme <- function(model,
                        vcov = NULL,
                        vcov_args = NULL,
                        ...) {
  # default values
  if (is.null(vcov)) {
    cs <- stats::coef(summary(model))
    p <- cs[, 5]
    param <- rownames(cs)

    # robust standard errors or custom varcov
  } else {
    b <- insight::get_parameters(model)
    se <- standard_error(model, vcov = vcov, vcov_args = vcov_args, ...)
    tstat <- b$Estimate / se$SE
    # residuals are defined like this in `nlme:::summary.lme`
    dof <- model$fixDF[["X"]]
    p <- 2 * stats::pt(-abs(tstat), df = dof)
    param <- se$Parameter
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(param),
    p = as.vector(p)
  )
}


#' @export
standard_error.lme <- standard_error.default


############### .gls --------------


#' @export
standard_error.gls <- standard_error.default


#' @export
p_value.gls <- p_value.default

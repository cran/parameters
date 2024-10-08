#' @export
ci.parameters_standardized <- function(x, ci = 0.95, verbose = TRUE, ...) {
  se <- attr(x, "standard_error")

  if (is.null(se)) {
    if (isTRUE(verbose)) {
      insight::print_color("\nCould not extract standard errors of standardized coefficients.\n", "red")
    }
    return(NULL)
  }

  # for "refit" method
  if (is.data.frame(se) && "SE" %in% colnames(se)) {
    se <- se$SE
  }

  # check if we have model. if so, use df from model
  model <- .get_object(x)
  if (!is.null(model)) {
    dof <- insight::get_df(model, type = "wald")
    if (!is.null(dof)) {
      if (length(dof) > 1 && length(dof) != nrow(x)) {
        dof <- Inf
      }
    } else {
      dof <- Inf
    }
  } else {
    dof <- Inf
  }

  out <- lapply(ci, function(i) {
    alpha <- (1 + i) / 2
    fac <- stats::qt(alpha, df = dof)
    data.frame(
      Parameter = x$Parameter,
      CI = i,
      CI_low = x$Std_Coefficient - se * fac,
      CI_high = x$Std_Coefficient + se * fac,
      stringsAsFactors = FALSE
    )
  })

  insight::text_remove_backticks(do.call(rbind, out), verbose = FALSE)
}


#' @export
ci.effectsize_table <- ci.parameters_standardized


#' @export
standard_error.effectsize_table <- standard_error.parameters_standardized

# ci -----------------

#' @export
ci.negbin <- ci.glm


#' @export
ci.polr <- function(x, ci = 0.95, dof = NULL, method = "profile", ...) {
  method <- match.arg(method, choices = c("profile", "wald", "robust"))

  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(ci.default(x, ...))
  }

  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled2(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- .ci_generic(model = x, ci = ci, dof = dof, method = method, ...)
  }

  # for polr, profiled CI do not return CI for response levels
  # thus, we also calculate Wald CI and add missing rows to result

  out_missing <- .ci_generic(model = x, ci = ci)
  missing_rows <- out_missing$Parameter %in% setdiff(out_missing$Parameter, out$Parameter)
  out <- rbind(out, out_missing[missing_rows, ])

  # fix names, to match standard error and p_value

  out$Parameter <- gsub("Intercept: ", "", out$Parameter, fixed = TRUE)
  row.names(out) <- NULL

  out
}


# SE -----------------

#' @export
standard_error.polr <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(standard_error.default(model, ...))
  }

  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  se <- smry[[2]]
  names(se) <- rownames(smry)

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


# p -----------------

#' @export
p_value.negbin <- p_value.default


#' @export
p_value.rlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- 2 * stats::pt(abs(cs[, 3]), df = insight::get_df(model, type = "wald"), lower.tail = FALSE)
  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


#' @export
p_value.polr <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(p_value.default(model, ...))
  }

  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  tstat <- smry[[3]]
  p <- 2 * stats::pt(abs(tstat), df = insight::get_df(x = model, type = "wald"), lower.tail = FALSE)
  names(p) <- rownames(smry)

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


# parameters -----------------

#' @export
model_parameters.ridgelm <- function(model, verbose = TRUE, ...) {
  parameters <- insight::get_parameters(model)
  parameters$Scale <- as.vector(model$scales)

  # remove all complete-missing cases
  parameters <- parameters[apply(parameters, 1, function(i) !all(is.na(i))), ]

  rownames(parameters) <- NULL

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  attr(parameters, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  parameters
}

#' @export
model_parameters.polr <- model_parameters.glm

#' @export
model_parameters.negbin <- model_parameters.glm

.ci_profiled <- function(model, ci) {
  glm_ci <- tryCatch(
    {
      out <- as.data.frame(
        suppressWarnings(stats::confint(model, level = ci)),
        stringsAsFactors = FALSE
      )
      names(out) <- c("CI_low", "CI_high")

      out$CI <- ci
      out$Parameter <- insight::get_parameters(model,
        effects = "fixed",
        component = "conditional",
        verbose = FALSE
      )$Parameter

      out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
      rownames(out) <- NULL

      out
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(glm_ci)) {
    glm_ci <- .ci_generic(model, ci = ci)
  }

  glm_ci
}


# we need this function for models where confint and get_parameters return
# different length (e.g. as for "polr" models)
.ci_profiled2 <- function(model, ci) {
  glm_ci <- tryCatch(
    {
      out <- as.data.frame(stats::confint(model, level = ci), stringsAsFactors = FALSE)
      names(out) <- c("CI_low", "CI_high")

      out$CI <- ci
      out$Parameter <- .remove_backticks_from_string(rownames(out))

      out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
      rownames(out) <- NULL

      out
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(glm_ci)) {
    glm_ci <- .ci_generic(model, ci = ci)
  }

  glm_ci
}


#' @keywords internal
.ci_profile_merMod <- function(x, ci, profiled, ...) {
  out <- as.data.frame(suppressWarnings(stats::confint(profiled, level = ci, ...)))
  rownames(out) <- gsub("`", "", rownames(out), fixed = TRUE)
  out <- out[rownames(out) %in% insight::find_parameters(x, effects = "fixed")$conditional, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- row.names(out)
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  row.names(out) <- NULL
  out
}


#' @keywords internal
.ci_profile_glmmTMB <- function(x, ci, profiled, component, ...) {
  # make sure "..." doesn't pass invalid arguments to package TMB
  dot_args <- .check_profile_uniroot_args(...)

  if (is.null(profiled)) {
    fun_args <- list(x, method = "profile", level = ci, dot_args)
    out <- as.data.frame(do.call(stats::confint, fun_args))
  } else {
    fun_args <- list(profiled, level = ci, dot_args)
    out <- .safe(as.data.frame(do.call(stats::confint, fun_args)))
    if (is.null(out)) {
      fun_args <- list(x, method = "profile", level = ci, dot_args)
      out <- as.data.frame(do.call(stats::confint, fun_args))
    }
  }
  .process_glmmTMB_CI(x, out, ci, component)
}


#' @keywords internal
.ci_uniroot_glmmTMB <- function(x, ci, component, ...) {
  # make sure "..." doesn't pass invalid arguments to package TMB
  dot_args <- .check_profile_uniroot_args(...)
  fun_args <- list(x, level = ci, method = "uniroot", dot_args)
  out <- as.data.frame(do.call(stats::confint, fun_args))
  .process_glmmTMB_CI(x, out, ci, component)
}


.check_profile_uniroot_args <- function(...) {
  .profile_formals <- c(
    "cl", "fitted", "h", "level_max", "lincomb", "maxit", "name",
    "ncpus", "npts", "obj", "parallel", "parm", "parm.range", "slice",
    "stderr", "stepfac", "trace", "ystep", "ytol"
  )
  dots <- list(...)
  dot_args <- intersect(names(dots), .profile_formals)
  out <- dots[dot_args]
  if (!length(out)) {
    return(NULL)
  }
  out
}


.process_glmmTMB_CI <- function(x, out, ci, component) {
  rownames(out) <- gsub("`", "", rownames(out), fixed = TRUE)

  pars <- insight::get_parameters(x,
    effects = "fixed",
    component = component,
    verbose = FALSE
  )

  param_names <- switch(component,
    conditional = pars$Parameter,
    zi = ,
    zero_inflated = paste0("zi~", pars$Parameter),
    c(
      pars$Parameter[pars$Component == "conditional"],
      paste0("zi~", pars$Parameter[pars$Component == "zero_inflated"])
    )
  )

  out <- out[rownames(out) %in% param_names, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- pars$Parameter
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  out$Component <- pars$Component
  row.names(out) <- NULL
  out
}


#' @keywords internal
.ci_boot_merMod <- function(x, ci, iterations = 500, effects = "fixed", ...) {
  insight::check_if_installed("lme4")

  # Compute
  out <- suppressWarnings(suppressMessages(as.data.frame(
    lme4::confint.merMod(x, level = ci, method = "boot", nsim = iterations, ...)
  )))
  rownames(out) <- gsub("`", "", rownames(out), fixed = TRUE)
  out <- out[rownames(out) %in% insight::find_parameters(x, effects = "fixed")$conditional, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- row.names(out)
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  row.names(out) <- NULL
  out
}

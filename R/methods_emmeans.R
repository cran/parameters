# emmeans

# model_parameters ----------------


#' @export
model_parameters.emmGrid <- function(model,
                                     ci = 0.95,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci_method = "eti",
                                     test = "pd",
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  # set default for p-adjust
  emm_padjust <- .safe(model@misc$adjust)
  if (!is.null(emm_padjust) && is.null(p_adjust)) {
    p_adjust <- emm_padjust
  }

  s <- summary(model, level = ci, adjust = "none")
  params <- as.data.frame(s)

  if (.is_bayesian_emmeans(model)) {
    # Bayesian models go here...
    params <- bayestestR::describe_posterior(
      model,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      bf_prior = NULL,
      diagnostic = NULL,
      priors = NULL,
      verbose = verbose,
      ...
    )
    statistic <- NULL
  } else {
    # we assume frequentist here...
    statistic <- insight::get_statistic(model, ci = ci, adjust = "none")
    SE <- standard_error(model)
    p <- p_value(model, ci = ci, adjust = "none")

    params$Statistic <- statistic$Statistic
    params$SE <- SE$SE
    params$p <- p$p

    # ==== adjust p-values?

    if (!is.null(p_adjust)) {
      params <- .p_adjust(params, p_adjust, model, verbose)
    }
  }


  # Renaming
  estName <- attr(s, "estName")
  if (!is.null(statistic)) {
    names(params) <- gsub(
      "Statistic",
      gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE),
      names(params),
      fixed = TRUE
    )
  }
  names(params) <- gsub("Std. Error", "SE", names(params), fixed = TRUE)
  names(params) <- gsub(estName, "Estimate", names(params), fixed = TRUE)
  names(params) <- gsub("lower.CL", "CI_low", names(params), fixed = TRUE)
  names(params) <- gsub("upper.CL", "CI_high", names(params), fixed = TRUE)
  names(params) <- gsub("asymp.LCL", "CI_low", names(params), fixed = TRUE)
  names(params) <- gsub("asymp.UCL", "CI_high", names(params), fixed = TRUE)
  names(params) <- gsub("lower.HPD", "CI_low", names(params), fixed = TRUE)
  names(params) <- gsub("upper.HPD", "CI_high", names(params), fixed = TRUE)

  # check if we have CIs
  if (!any(startsWith(colnames(params), "CI_"))) {
    df_column <- grep("(df|df_error)", colnames(params))
    if (length(df_column) > 0) {
      dof <- params[[df_column[1]]]
    } else {
      dof <- Inf
    }
    fac <- stats::qt((1 + ci) / 2, df = dof)
    params$CI_low <- params$Estimate - fac * params$SE
    params$CI_high <- params$Estimate + fac * params$SE
  }

  # rename if necessary
  if ("df" %in% colnames(params)) {
    colnames(params)[colnames(params) == "df"] <- "df_error"
  }

  # Reorder
  estimate_pos <- which(colnames(s) == estName)
  parameter_names <- colnames(params)[seq_len(estimate_pos - 1)]
  col_order <- c(
    parameter_names, "Estimate", "Median", "Mean", "SE", "SD", "MAD",
    "CI_low", "CI_high", "F", "t", "z", "df", "df_error", "p", "pd",
    "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage"
  )
  params <- params[col_order[col_order %in% names(params)]]

  # rename
  names(params) <- gsub("Estimate", "Coefficient", names(params), fixed = TRUE)

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  # filter parameters
  if (!is.null(keep) || !is.null(drop)) {
    params <- .filter_parameters(params,
      keep = keep,
      drop = drop,
      verbose = verbose
    )
  }

  params <- suppressWarnings(.add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate = FALSE,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  ))
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(params, "parameter_names") <- parameter_names

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}


#' @export
model_parameters.emm_list <- function(model,
                                      ci = 0.95,
                                      exponentiate = FALSE,
                                      p_adjust = NULL,
                                      verbose = TRUE,
                                      ...) {
  s <- summary(model)
  params <- lapply(seq_along(s), function(i) {
    pars <- model_parameters(
      model[[i]],
      ci = ci,
      exponentiate = exponentiate,
      p_adjust = p_adjust,
      verbose = verbose
    )
    estimate_pos <- which(colnames(pars) %in% c("Coefficient", "Median", "Mean"))[1]
    pars[seq_len(estimate_pos - 1)] <- NULL
    cbind(
      Parameter = .pretty_emmeans_Parameter_names(model[[i]]),
      pars
    )
  })
  params <- do.call(rbind, params)
  params$Component <- .pretty_emmeans_Component_names(s)

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
model_parameters.summary_emm <- function(model,
                                         keep = NULL,
                                         drop = NULL,
                                         verbose = TRUE,
                                         ...) {
  params <- model
  # Renaming
  estName <- attr(model, "estName")
  names(params) <- gsub("Std. Error", "SE", names(params), fixed = TRUE)
  names(params) <- gsub(estName, "Estimate", names(params), fixed = TRUE)
  names(params) <- gsub("response", "Response", names(params), fixed = TRUE)
  names(params) <- gsub("lower.CL", "CI_low", names(params), fixed = TRUE)
  names(params) <- gsub("upper.CL", "CI_high", names(params), fixed = TRUE)
  names(params) <- gsub("asymp.LCL", "CI_low", names(params), fixed = TRUE)
  names(params) <- gsub("asymp.UCL", "CI_high", names(params), fixed = TRUE)
  names(params) <- gsub("lower.HPD", "CI_low", names(params), fixed = TRUE)
  names(params) <- gsub("upper.HPD", "CI_high", names(params), fixed = TRUE)

  # rename if necessary
  if ("df" %in% colnames(params)) {
    colnames(params)[colnames(params) == "df"] <- "df_error"
  }

  # Reorder
  estimate_pos <- which(colnames(model) == estName)
  parameter_names <- colnames(params)[seq_len(estimate_pos - 1)]
  col_order <- c(
    parameter_names, "Estimate", "Median", "Mean", "SE", "SD", "MAD",
    "CI_low", "CI_high", "F", "t", "z", "df", "df_error", "p", "pd",
    "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage"
  )
  params <- params[col_order[col_order %in% names(params)]]

  # rename
  names(params) <- gsub("Estimate", "Coefficient", names(params), fixed = TRUE)

  # filter parameters
  if (!is.null(keep) || !is.null(drop)) {
    params <- .filter_parameters(params,
      keep = keep,
      drop = drop,
      verbose = verbose
    )
  }

  params <- suppressWarnings(.add_model_parameters_attributes(
    params,
    model,
    ci = 0.95,
    exponentiate = FALSE,
    p_adjust = NULL,
    verbose = verbose,
    ...
  ))
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(params, "parameter_names") <- parameter_names

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}


# standard errors -----------------


#' @export
standard_error.emmGrid <- function(model, ...) {
  if (!is.null(model@misc$is_boot) && model@misc$is_boot) {
    return(boot_em_standard_error(model))
  }

  s <- summary(model)
  estimate_pos <- which(colnames(s) == attr(s, "estName"))

  if (length(estimate_pos) && !is.null(s$SE)) {
    out <- .data_frame(
      Parameter = .pretty_emmeans_Parameter_names(model),
      SE = unname(s$SE)
    )
  } else {
    out <- NULL
  }
  out
}


#' @export
standard_error.emm_list <- function(model, ...) {
  if (!is.null(model[[1]]@misc$is_boot) && model[[1]]@misc$is_boot) {
    return(boot_em_standard_error(model))
  }

  params <- insight::get_parameters(model)
  s <- summary(model)
  se <- unlist(lapply(s, function(i) {
    if (is.null(i$SE)) {
      rep(NA, nrow(i))
    } else {
      i$SE
    }
  }))

  .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    SE = unname(se),
    Component = .pretty_emmeans_Component_names(s)
  )
}

boot_em_standard_error <- function(model) {
  est <- insight::get_parameters(model, summary = FALSE)

  Component <- NULL
  s <- summary(model)
  if (inherits(s, "list")) {
    Component <- .pretty_emmeans_Component_names(s)
  }

  out <- .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    SE = vapply(est, stats::sd, numeric(1))
  )

  if (!is.null(Component)) out$Component <- Component

  out
}


# p values ----------------------


#' @rdname p_value
#' @export
p_value.emmGrid <- function(model, ci = 0.95, adjust = "none", ...) {
  if (!is.null(model@misc$is_boot) && model@misc$is_boot) {
    return(boot_em_pval(model, adjust))
  }

  s <- summary(model, level = ci, adjust = adjust)
  estimate_pos <- which(colnames(s) == attr(s, "estName"))

  if (!length(estimate_pos)) {
    return(NULL)
  }

  stat <- insight::get_statistic(model, ci = ci, adjust = adjust)
  p <- 2 * stats::pt(abs(stat$Statistic), df = s$df, lower.tail = FALSE)

  .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    p = as.vector(p)
  )
}


#' @export
p_value.emm_list <- function(model, adjust = "none", ...) {
  if (!is.null(model[[1]]@misc$is_boot) && model[[1]]@misc$is_boot) {
    return(boot_em_pval(model, adjust))
  }


  params <- insight::get_parameters(model)
  s <- summary(model, adjust = adjust)

  # p-values
  p <- unlist(lapply(s, function(i) {
    if (is.null(i$p)) {
      rep(NA, nrow(i))
    } else {
      i$p
    }
  }))

  # result
  out <- .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    p = as.vector(p),
    Component = .pretty_emmeans_Component_names(s)
  )

  # any missing values?
  if (anyNA(out$p)) {
    # standard errors
    se <- unlist(lapply(s, function(i) {
      if (is.null(i$SE)) {
        rep(NA, nrow(i))
      } else {
        i$SE
      }
    }))

    # test statistic and p-values
    stat <- params$Estimate / se
    dof <- insight::get_df(model)
    p_val <- 2 * stats::pt(abs(stat), df = dof, lower.tail = FALSE)
    out$p[is.na(out$p)] <- p_val[is.na(out$p)]
  }

  out
}


boot_em_pval <- function(model, adjust) {
  est <- insight::get_parameters(model, summary = FALSE)
  p <- sapply(est, p_value)
  p <- stats::p.adjust(p, method = adjust)

  Component <- NULL
  s <- summary(model)
  if (inherits(s, "list")) {
    Component <- .pretty_emmeans_Component_names(s)
  }

  out <- .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    p = unname(p)
  )

  if (!is.null(Component)) out$Component <- Component

  out
}


# format parameters -----------------


#' @export
format_parameters.emm_list <- function(model, ...) {
  NULL
}


# Utils -------------------------------------------------------------------

.pretty_emmeans_Parameter_names <- function(model) {
  s <- summary(model)

  if (inherits(s, "list")) {
    parnames <- lapply(seq_along(s), function(i) .pretty_emmeans_Parameter_names(model[[i]]))
    parnames <- unlist(parnames)
  } else {
    estimate_pos <- which(colnames(s) == attr(s, "estName"))
    params <- s[, 1:(estimate_pos - 1), drop = FALSE]
    if (ncol(params) >= 2) {
      r <- apply(params, 1, function(i) paste0(colnames(params), " [", i, "]"))
      parnames <- unname(sapply(as.data.frame(r), toString))
    } else {
      parnames <- as.vector(params[[1]])
    }
  }
  parnames
}

.pretty_emmeans_Component_names <- function(s) {
  Component <- lapply(seq_along(s), function(i) {
    rep(names(s)[[i]], nrow(s[[i]]))
  })
  Component <- unlist(Component)
}

.is_bayesian_emmeans <- function(model) {
  is_frq <- isTRUE(all.equal(dim(model@post.beta), c(1, 1))) &&
    isTRUE(is.na(model@post.beta)) && is.null(model@misc$is_boot)
  isFALSE(is_frq)
}

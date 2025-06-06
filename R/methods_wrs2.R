#' Parameters from robust statistical objects in `WRS2`
#'
#' @param model Object from `WRS2` package.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.default
#'
#' @examples
#' if (require("WRS2") && packageVersion("WRS2") >= "1.1.3") {
#'   model <- t1way(libido ~ dose, data = viagra)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export

# anova ----------------------

model_parameters.t1way <- function(model, keep = NULL, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_t1way(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_t1way <- function(model) {
  fcall <- insight::safe_deparse(model$call)
  # effect sizes are by default contained for `t1way` but not `rmanova`
  if (grepl("^(t1way|WRS2::t1way)", fcall)) {
    data.frame(
      `F` = model$test,
      df = model$df1,
      df_error = model$df2,
      p = model$p.value,
      Method = "A heteroscedastic one-way ANOVA for trimmed means",
      Estimate = model$effsize,
      CI = 1 - model$alpha,
      CI_low = model$effsize_ci[1],
      CI_high = model$effsize_ci[2],
      Effectsize = "Explanatory measure of effect size",
      stringsAsFactors = FALSE
    )
  } else if (grepl("^(rmanova|WRS2::rmanova)", fcall)) {
    data.frame(
      `F` = model$test,
      df = model$df1,
      df_error = model$df2,
      p = model$p.value,
      Method = "A heteroscedastic one-way repeated measures ANOVA for trimmed means",
      stringsAsFactors = FALSE
    )
  }
}


#' @export
model_parameters.med1way <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_med1way(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_med1way <- function(model) {
  data.frame(
    `F` = model$test,
    `Critical value` = model$crit.val,
    p = model$p.value,
    Method = "Heteroscedastic one-way ANOVA for medians",
    stringsAsFactors = FALSE
  )
}

#' @export
model_parameters.dep.effect <- function(model,
                                        keep = NULL,
                                        verbose = TRUE,
                                        ...) {
  parameters <- .extract_wrs2_dep.effect(model, keep = keep)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_dep.effect <- function(model, keep = NULL, ...) {
  out <- as.data.frame(model)

  out$Parameter <- c(attributes(out)$row.names)

  # effectsize descriptions
  out$Effectsize <- c(
    "Algina-Keselman-Penfield robust standardized difference", # AKP
    "Quantile shift based on the median of the distribution of difference scores", # QS (median)
    "Quantile shift based on the trimmed mean of the distribution of X-Y", # QStr
    "P(X<Y), Probability of first being less than second for a random pair" # SIGN
  )

  # column names
  names(out) <- c(
    "Mu", "Estimate", "Small", "Medium", "Large",
    "CI_low", "CI_high", "Parameter", "Effectsize"
  )

  # add CI column
  out$CI <- 0.95

  # reorder columns
  col_order <- c(
    "Parameter", "Estimate", "CI", "CI_low", "CI_high", "Effectsize",
    "Mu", "Small", "Medium", "Large"
  )
  out <- out[col_order[col_order %in% names(out)]]

  # remove rownames
  rownames(out) <- NULL

  # select a specific effect size only
  if (!is.null(keep)) {
    out <- out[out$Parameter == keep, ]
  }

  out
}

# t-test ----------------------

#' @export
model_parameters.yuen <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_yuen(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_yuen <- function(model) {
  fcall <- insight::safe_deparse(model$call)

  # the latter regexe covers `rlang::exec` or `do.call` instances
  if (grepl("^(yuen\\(|WRS2::yuen\\()", fcall) ||
    grepl("^(yuenbt\\(|WRS2::yuenbt\\()", fcall) ||
    grepl("function (formula, data, tr = 0.2, ...)", fcall, fixed = TRUE)) {
    # between-subjects
    method <- "Yuen's test on trimmed means for independent samples"
  } else {
    # within-subjects
    method <- "Yuen's test on trimmed means for dependent samples"
  }

  out <- data.frame(
    t = model$test,
    df_error = model$df,
    p = model$p.value,
    Method = method,
    Difference = model$diff,
    CI = 0.95,
    Difference_CI_low = model$conf.int[1],
    Difference_CI_high = model$conf.int[2],
    stringsAsFactors = FALSE
  )

  if (!is.null(model$effsize)) {
    out$Estimate <- model$effsize
    out$Effectsize <- "Explanatory measure of effect size"
  }

  out
}

#' @export
model_parameters.pb2 <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_pb2(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_pb2 <- function(model) {
  data.frame(
    t = model$test,
    p = model$p.value,
    Method = "Robust t-test for independent samples",
    CI = 0.95,
    CI_low = model$conf.int[1],
    CI_high = model$conf.int[2],
    stringsAsFactors = FALSE
  )
}

# pairwise comparisons ----------------------

#' @export
model_parameters.mcp1 <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_mcp12(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  attr(parameters, "no_caption") <- TRUE
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

#' @export
model_parameters.mcp2 <- model_parameters.mcp1

.extract_wrs2_mcp12 <- function(model) {
  # component of the object containing results from multiple comparisons
  out <- as.data.frame(model$comp)

  # rename to `eaystats` conventions
  names(out)[1:6] <- c("Group1", "Group2", "Psihat", "CI_low", "CI_high", "p")

  # convert names to character
  out$Group1 <- model$fnames[model$comp[, 1]]
  out$Group2 <- model$fnames[model$comp[, 2]]

  # CI column
  out$CI <- 1 - model$alpha

  # reorder
  col_order <- c("Group1", "Group2", "Psihat", "CI", "CI_low", "CI_high", "p", "p.crit")
  out <- out[col_order[col_order %in% names(out)]]
  attr(parameters, "no_caption") <- TRUE

  out
}

# comparison of discrete distributions ----------------------

#' @export
model_parameters.robtab <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_robtab(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


.extract_wrs2_robtab <- function(model) {
  fcall <- insight::safe_deparse(model$call)

  # dataframe
  out <- as.data.frame(model$partable)

  # rename to `eaystats` conventions
  if (grepl("^(discmcp\\(|WRS2::discmcp\\()", fcall)) {
    names(out)[1:3] <- c("Group1", "Group2", "p")
  }

  if (grepl("^(discstep\\(|WRS2::discstep\\()", fcall)) {
    names(out)[1:2] <- c("Groups", "p")
  }

  if (grepl("^(binband\\(|WRS2::binband\\()", fcall)) {
    names(out)[1:4] <- c("Value", "Probability1", "Probability2", "Difference")
    if ("p.value" %in% names(out)) {
      out$p <- out$p.value
      out$p.value <- NULL
    }
  }

  out
}

# one-sample percentile bootstrap ----------------------

#' @export
model_parameters.onesampb <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_onesampb(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_onesampb <- function(model) {
  data.frame(
    Estimate = model$estimate,
    CI = 1 - model$alpha,
    CI_low = model$ci[1],
    CI_high = model$ci[2],
    p = model$p.value,
    n_Obs = model$n,
    Effectsize = "Robust location measure",
    Method = "One-sample percentile bootstrap",
    stringsAsFactors = FALSE
  )
}

#' @export
model_parameters.trimcibt <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_trimcibt(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_trimcibt <- function(model) {
  data.frame(
    t = model$test.stat,
    p = model$p.value,
    n_Obs = model$n,
    Method = "Bootstrap-t method for one-sample test",
    Estimate = model$estimate[[1]],
    CI = 1 - model$alpha,
    CI_low = model$ci[1],
    CI_high = model$ci[2],
    Effectsize = "Trimmed mean",
    stringsAsFactors = FALSE
  )
}


# AKP effect sizes ----------------------

#' @export
model_parameters.AKP <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_AKP(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_AKP <- function(model) {
  data.frame(
    Estimate = model$AKPeffect,
    CI = 1 - model$alpha,
    CI_low = model$AKPci[1],
    CI_high = model$AKPci[2],
    Effectsize = "Algina-Keselman-Penfield robust standardized difference",
    stringsAsFactors = FALSE
  )
}

#' @export
model_parameters.wmcpAKP <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_wmcpAKP(model)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_wmcpAKP <- function(model) {
  data.frame(
    Estimate = model[[1]],
    CI = 0.95,
    CI_low = model[[2]],
    CI_high = model[[3]],
    Effectsize = "Algina-Keselman-Penfield robust standardized difference average",
    stringsAsFactors = FALSE
  )
}

#' @importFrom bayestestR p_significance
#' @export
bayestestR::p_significance


#' @title Practical Significance (ps)
#'
#' @description Compute the probability of **Practical Significance** (*ps*),
#' which can be conceptualized as a unidirectional equivalence test. It returns
#' the probability that an effect is above a given threshold corresponding to a
#' negligible effect in the median's direction, considering a parameter's _full_
#' confidence interval. In other words, it returns the probability of a clear
#' direction of an effect, which is larger than the smallest effect size of
#' interest (e.g., a minimal important difference). Its theoretical range is
#' from zero to one, but the *ps* is typically larger than 0.5 (to indicate
#' practical significance).
#'
#' In comparison the the [`equivalence_test()`] function, where the *SGPV*
#' (second generation p-value) describes the proportion of the _full_ confidence
#' interval that is _inside_ the ROPE, the value returned by `p_significance()`
#' describes the _larger_ proportion of the _full_ confidence interval that is
#' _outside_ the ROPE. This makes `p_significance()` comparable to
#' [`bayestestR::p_direction()`], however, while `p_direction()` compares to a
#' point-null by default, `p_significance()` compares to a range-null.
#'
#' @param x A statistical model.
#' @inheritParams bayestestR::p_significance
#' @inheritParams model_parameters.default
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to other methods.
#'
#' @seealso For more details, see [`bayestestR::p_significance()`]. See also
#' [`equivalence_test()`], [`p_function()`] and [`bayestestR::p_direction()`]
#' for functions related to checking effect existence and significance.
#'
#' @details `p_significance()` returns the proportion of the _full_ confidence
#' interval range (assuming a normally or t-distributed, equal-tailed interval,
#' based on the model) that is outside a certain range (the negligible effect,
#' or ROPE, see argument `threshold`). If there are values of the distribution
#' both below and above the ROPE, `p_significance()` returns the higher
#' probability of a value being outside the ROPE. Typically, this value should
#' be larger than 0.5 to indicate practical significance. However, if the range
#' of the negligible effect is rather large compared to the range of the
#' confidence interval, `p_significance()` will be less than 0.5, which
#' indicates no clear practical significance.
#'
#' Note that the assumed interval, which is used to calculate the practical
#' significance, is an estimation of the _full interval_ based on the chosen
#' confidence level. For example, if the 95% confidence interval of a
#' coefficient ranges from -1 to 1, the underlying _full (normally or
#' t-distributed) interval_ approximately ranges from -1.9 to 1.9, see also
#' following code:
#'
#' ```
#' # simulate full normal distribution
#' out <- bayestestR::distribution_normal(10000, 0, 0.5)
#' # range of "full" distribution
#' range(out)
#' # range of 95% CI
#' round(quantile(out, probs = c(0.025, 0.975)), 2)
#' ```
#'
#' This ensures that the practical significance always refers to the general
#' compatible parameter space of coefficients. Therefore, the _full interval_ is
#' similar to a Bayesian posterior distribution of an equivalent Bayesian model,
#' see following code:
#'
#' ```
#' library(bayestestR)
#' library(brms)
#' m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' m2 <- brm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' # probability of significance (ps) for frequentist model
#' p_significance(m)
#' # similar to ps of Bayesian models
#' p_significance(m2)
#' # similar to ps of simulated draws / bootstrap samples
#' p_significance(simulate_model(m))
#' ```
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html)
#' implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @inheritSection model_parameters Statistical inference - how to quantify evidence
#'
#' @references
#'
#'   - Amrhein, V., Korner-Nievergelt, F., and Roth, T. (2017). The earth is
#'     flat (p > 0.05): Significance thresholds and the crisis of unreplicable
#'     research. PeerJ, 5, e3544. \doi{10.7717/peerj.3544}
#'
#'   - Greenland S, Rafi Z, Matthews R, Higgs M. To Aid Scientific Inference,
#'     Emphasize Unconditional Compatibility Descriptions of Statistics. (2022)
#'     https://arxiv.org/abs/1909.08583v7 (Accessed November 10, 2022)
#'
#'   - Lakens, D. (2024). Improving Your Statistical Inferences (Version v1.5.1).
#'     Retrieved from https://lakens.github.io/statistical_inferences/.
#'     \doi{10.5281/ZENODO.6409077}
#'
#'   - Lakens, D., Scheel, A. M., and Isager, P. M. (2018). Equivalence Testing
#'     for Psychological Research: A Tutorial. Advances in Methods and Practices
#'     in Psychological Science, 1(2), 259–269.
#'
#'   - Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D. (2019).
#'     Indices of Effect Existence and Significance in the Bayesian Framework.
#'     Frontiers in Psychology, 10, 2767. \doi{10.3389/fpsyg.2019.02767}
#'
#'   - Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
#'     science: replace confidence and significance by compatibility and surprise.
#'     BMC Medical Research Methodology (2020) 20:244.
#'
#'   - Schweder T. Confidence is epistemic probability for empirical science.
#'     Journal of Statistical Planning and Inference (2018) 195:116–125.
#'     \doi{10.1016/j.jspi.2017.09.016}
#'
#'   - Schweder T, Hjort NL. Frequentist analogues of priors and posteriors.
#'     In Stigum, B. (ed.), Econometrics and the Philosophy of Economics: Theory
#'     Data Confrontation in Economics, pp. 285-217. Princeton University Press,
#'     Princeton, NJ, 2003
#'
#'   - Vos P, Holbert D. Frequentist statistical inference without repeated sampling.
#'     Synthese 200, 89 (2022). \doi{10.1007/s11229-022-03560-x}
#'
#' @return A data frame with columns for the parameter names, the confidence
#' intervals and the values for practical significance. Higher values indicate
#' more practical significance (upper bound is one).
#'
#' @examplesIf requireNamespace("bayestestR") && packageVersion("bayestestR") > "0.14.0" && requireNamespace("sandwich")
#' data(qol_cancer)
#' model <- lm(QoL ~ time + age + education, data = qol_cancer)
#'
#' p_significance(model)
#' p_significance(model, threshold = c(-0.5, 1.5))
#'
#' # based on heteroscedasticity-robust standard errors
#' p_significance(model, vcov = "HC3")
#'
#' if (require("see", quietly = TRUE)) {
#'   result <- p_significance(model)
#'   plot(result)
#' }
#' @export
p_significance.lm <- function(x,
                              threshold = "default",
                              ci = 0.95,
                              vcov = NULL,
                              vcov_args = NULL,
                              verbose = TRUE,
                              ...) {
  # generate normal distribution based on CI range
  result <- .posterior_ci(x, ci, vcov = vcov, vcov_args = vcov_args, ...)

  # copy
  out <- result$out
  posterior <- result$posterior

  # calculate the ROPE range - for multiple thresholds, we have to check
  # each list element for "default", to replace it with the appropriate range
  if (is.list(threshold)) {
    threshold <- lapply(threshold, function(i) {
      if (all(i == "default")) {
        i <- bayestestR::rope_range(x, verbose = verbose)
      }
      i
    })
  } else if (all(threshold == "default")) {
    threshold <- bayestestR::rope_range(x, verbose = verbose)
  }

  # add ps
  result_ps <- bayestestR::p_significance(
    posterior,
    threshold = threshold,
    verbose = verbose
  )
  out$ps <- as.numeric(result_ps)

  # for list-thresholds, we have the list as attribute and need to save it as
  # data.frame
  if (is.list(threshold)) {
    # save for later
    threshold_data <- stats::setNames(
      as.data.frame(do.call(rbind, attributes(result_ps)$threshold)),
      c("ROPE_low", "ROPE_high")
    )
    out <- cbind(out, threshold_data)
    keep <- c("Parameter", "CI", "CI_low", "CI_high", "ROPE_low", "ROPE_high", "ps", "Effects", "Component")
  } else {
    keep <- c("Parameter", "CI", "CI_low", "CI_high", "ps", "Effects", "Component")
  }

  # for plot, we need to have it numeric
  if (!is.numeric(threshold) && !is.list(threshold)) {
    threshold <- 0.1
  }

  # Reorder columns of 'out' to keep only the relevant ones
  out <- out[intersect(keep, colnames(out))]

  attr(out, "data") <- posterior
  attr(out, "threshold") <- threshold
  class(out) <- c("p_significance_lm", "p_significance", "see_p_significance", "data.frame")
  out
}


# helper ----------------------------------------------------------------------

.posterior_ci <- function(x, ci, vcov = NULL, vcov_args = NULL, ...) {
  # first, we need CIs
  if (inherits(x, "parameters_model")) {
    # for model_parameters objects, directly extract CIs
    out <- as.data.frame(x)[intersect(
      c("Parameter", "CI_low", "CI_high", "Component", "Effects"),
      colnames(x)
    )]
    ci <- attributes(x)$ci
    # and extract degrees of freedom
    df_column <- grep("(df|df_error)", colnames(x))
    if (length(df_column) > 0) {
      dof <- unique(x[[df_column]])
      if (length(dof) > 1) {
        dof <- Inf
      }
    } else {
      dof <- Inf
    }
  } else {
    out <- ci(x, ci = ci, vcov = vcov, vcov_args = vcov_args, ...)
    dof <- .safe(insight::get_df(x, type = "wald"), Inf)
  }
  # we now iterate all confidence intervals and create an approximate normal
  # distribution that covers the CI-range.
  posterior <- as.data.frame(lapply(seq_len(nrow(out)), function(i) {
    ci_range <- as.numeric(out[i, c("CI_low", "CI_high")])
    .generate_posterior_from_ci(ci, ci_range, dof = dof)
  }))
  colnames(posterior) <- out$Parameter

  # deal with Effects and Component columns
  if ("Effects" %in% colnames(out) && insight::has_single_value(out$Effects, remove_na = TRUE)) {
    out$Effects <- NULL
  }
  if ("Component" %in% colnames(out) && insight::has_single_value(out$Component, remove_na = TRUE)) {
    out$Component <- NULL
  }

  # check we don't have duplicated columns in "posterior" we need this for
  # plotting
  if (anyDuplicated(colnames(posterior)) > 0 && !is.null(out$Component)) {
    comps <- .rename_values(out$Component, "zero_inflated", "zi")
    comps <- .rename_values(comps, "conditional", "cond")
    colnames(posterior) <- paste0(out$Parameter, "_", comps)
    out$Parameter <- paste0(out$Parameter, "_", comps)
  }
  list(out = out, posterior = posterior)
}


# methods ---------------------------------------------------------------------

#' @export
print.p_significance_lm <- function(x, digits = 2, ...) {
  threshold <- attributes(x)$threshold
  # Check if threshold is a list, which indicates multiple thresholds
  if (is.list(threshold)) {
    caption <- "Practical Significance"
  } else {
    # make sure it's numeric
    if (!is.numeric(threshold)) {
      threshold <- 0.1
    }
    # make sure we have both bounds for the range
    if (length(threshold) == 1) {
      threshold <- c(threshold * -1, threshold)
    }
    caption <- sprintf(
      "Practical Significance (threshold: %s)",
      toString(insight::format_value(threshold, digits = 2))
    )
  }
  x$ps <- insight::format_pd(x$ps, name = NULL)
  x <- insight::format_table(x, digits = digits)
  cat(insight::export_table(x, title = caption, ...))
}


# other classes --------------------------------------------------------------

#' @export
p_significance.glm <- p_significance.lm

#' @export
p_significance.coxph <- p_significance.lm

#' @export
p_significance.svyglm <- p_significance.lm

#' @export
p_significance.glmmTMB <- p_significance.lm

#' @export
p_significance.merMod <- p_significance.lm

#' @export
p_significance.wbm <- p_significance.lm

#' @export
p_significance.lme <- p_significance.lm

#' @export
p_significance.gee <- p_significance.lm

#' @export
p_significance.gls <- p_significance.lm

#' @export
p_significance.feis <- p_significance.lm

#' @export
p_significance.felm <- p_significance.lm

#' @export
p_significance.mixed <- p_significance.lm

#' @export
p_significance.hurdle <- p_significance.lm

#' @export
p_significance.zeroinfl <- p_significance.lm

#' @export
p_significance.rma <- p_significance.lm

#' @export
p_significance.parameters_model <- p_significance.lm

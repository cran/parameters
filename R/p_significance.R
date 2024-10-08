#' @importFrom bayestestR p_significance
#' @export
bayestestR::p_significance



#' @title Practical Significance (ps)
#'
#' @description Compute the probability of **Practical Significance** (*ps*),
#' which can be conceptualized as a unidirectional equivalence test. It returns
#' the probability that an effect is above a given threshold corresponding to a
#' negligible effect in the median's direction, considering a parameter's _full_
#' confidence interval. In comparison the the [`equivalence_test()`] function,
#' where the *SGPV* (second generation p-value) describes the proportion of the
#' _full_ confidence interval that is _inside_ the ROPE, the value returned by
#' `p_significance()` describes the _larger_ proportion of the _full_ confidence
#' interval that is _outside_ the ROPE. This makes `p_significance()` comparable
#' to [`bayestestR::p_direction()`], however, while `p_direction()` compares to
#' a point-null by default, `p_significance()` compares to a range-null.
#'
#' @param x A statistical model.
#' @inheritParams bayestestR::p_significance
#' @inheritParams model_parameters.default
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to other methods, e.g. `ci()`.
#'
#' @seealso For more details, see [`bayestestR::p_significance()`]. See also
#' [`equivalence_test()`].
#'
#' @details `p_significance()` returns the proportion of the _full_ confidence
#' interval range (assuming a normally distributed, equal-tailed interval) that
#' is outside a certain range (the negligible effect, or ROPE, see argument
#' `threshold`). If there are values of the distribution both below and above
#' the ROPE, `p_significance()` returns the higher probability of a value being
#' outside the ROPE. Typically, this value should be larger than 0.5 to indicate
#' practical significance. However, if the range of the negligible effect is
#' rather large compared to the range of the confidence interval,
#' `p_significance()` will be less than 0.5, which indicates no clear practical
#' significance.
#'
#' Note that the assumed interval, which is used to calculate the practical
#' significance, is an _approximation_ of the _full interval_ based on the
#' chosen confidence level. For example, if the 95% confidence interval of a
#' coefficient ranges from -1 to 1, the underlying _full (normally distributed)
#' interval_ approximately ranges from -1.9 to 1.9, see also following code:
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
#' @return A data frame.
#'
#' @examplesIf requireNamespace("bayestestR") && packageVersion("bayestestR") > "0.14.0"
#' data(qol_cancer)
#' model <- lm(QoL ~ time + age + education, data = qol_cancer)
#'
#' p_significance(model)
#' p_significance(model, threshold = c(-0.5, 1.5))
#'
#' # plot method
#' if (require("see", quietly = TRUE)) {
#'   result <- p_significance(model)
#'   plot(result)
#' }
#' @export
p_significance.lm <- function(x, threshold = "default", ci = 0.95, verbose = TRUE, ...) {
  # first, we need CIs
  out <- ci(x, ci = ci, ...)
  # we now iterate all confidence intervals and create an approximate normal
  # distribution that covers the CI-range.
  posterior <- as.data.frame(lapply(seq_len(nrow(out)), function(i) {
    ci_range <- as.numeric(out[i, c("CI_low", "CI_high")])
    .generate_posterior_from_ci(ci, ci_range)
  }))
  colnames(posterior) <- out$Parameter

  # calculate the ROPE range
  if (all(threshold == "default")) {
    threshold <- bayestestR::rope_range(x, verbose = verbose)
  }

  out$ps <- as.numeric(bayestestR::p_significance(
    posterior,
    threshold = threshold,
    verbose = verbose
  ))

  # for plot, we need to have it numeric
  if (!is.numeric(threshold)) {
    threshold <- 0.1
  }

  attr(out, "data") <- posterior
  attr(out, "threshold") <- threshold
  class(out) <- c("p_significance_lm", "p_significance", "see_p_significance", "data.frame")
  out
}


# methods ---------------------------------------------------------------------

#' @export
print.p_significance_lm <- function(x, digits = 2, p_digits = 3, ...) {
  threshold <- attributes(x)$threshold
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
  x$ps <- insight::format_p(x$ps, name = "ps", digits = p_digits)
  x <- insight::format_table(x, digits = digits, p_digits = p_digits)
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

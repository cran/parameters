% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_parameters.R
\name{bootstrap_parameters}
\alias{bootstrap_parameters}
\alias{parameters_bootstrap}
\title{Parameters bootstrapping}
\usage{
bootstrap_parameters(
  model,
  iterations = 1000,
  centrality = "median",
  ci = 0.95,
  ci_method = "quantile",
  test = "p-value",
  ...
)

parameters_bootstrap(
  model,
  iterations = 1000,
  centrality = "median",
  ci = 0.95,
  ci_method = "quantile",
  test = "p-value",
  ...
)
}
\arguments{
\item{model}{Statistical model.}

\item{iterations}{The number of draws to simulate/bootstrap.}

\item{centrality}{The point-estimates (centrality indices) to compute.  Character (vector) or list with one or more of these options: \code{"median"}, \code{"mean"}, \code{"MAP"} or \code{"all"}.}

\item{ci}{Value or vector of probability of the CI (between 0 and 1)
to be estimated. Default to \code{.89} (89\%) for Bayesian models and \code{.95} (95\%) for frequentist models.}

\item{ci_method}{The type of index used for Credible Interval. Can be
\code{"HDI"} (default, see \code{\link[bayestestR:hdi]{hdi}}) or \code{"ETI"}
(see \code{\link[bayestestR:eti]{eti}}).}

\item{test}{The indices of effect existence to compute. Character (vector) or
list with one or more of these options: \code{"p_direction"} (or \code{"pd"}),
\code{"rope"}, \code{"p_map"}, \code{"equivalence_test"} (or \code{"equitest"}),
\code{"bayesfactor"} (or \code{"bf"}) or \code{"all"} to compute all tests.
For each "test", the corresponding \pkg{bayestestR} function is called
(e.g. \code{\link[bayestestR:rope]{rope}} or \code{\link[bayestestR:p_direction]{p_direction}}) and its results
included in the summary output.}

\item{...}{Arguments passed to or from other methods.}
}
\value{
Bootstrapped parameters.
}
\description{
Compute bootstrapped parameters and their related indices such as Confidence Intervals (CI) and p-values.
}
\examples{
library(parameters)

model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
bootstrap_parameters(model)
}
\references{
Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (Vol. 1). Cambridge university press.
}
\seealso{
\code{\link{bootstrap_model}}, \code{\link{simulate_parameters}}, \code{\link{simulate_model}}
}
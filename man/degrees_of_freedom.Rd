% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dof.R
\name{degrees_of_freedom}
\alias{degrees_of_freedom}
\alias{dof}
\title{Degrees of Freedom (DoF)}
\usage{
degrees_of_freedom(model, method = "analytical")

dof(model, method = "analytical")
}
\arguments{
\item{model}{A statistical model.}

\item{method}{Can be \code{"analytical"} (default, DoFs are estimated based on the model type), \code{"fit"}, in which case they are directly taken from the model if available (for Bayesian models, the goal (looking for help to make it happen) would be to refit the model as a frequentist one before extracting the DoFs), or \code{"any"}, which tries to extract DoF by any of those methods, whichever succeeds.}
}
\description{
Estimate or extract degrees of freedom of models.
}
\examples{
model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
dof(model)

model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
dof(model)

library(lme4)
model <- lmer(Sepal.Length ~ Petal.Length + (1|Species), data = iris)
dof(model)
\donttest{
library(rstanarm)
model <- stan_glm(
  Sepal.Length ~ Petal.Length * Species,
  data = iris,
  chains = 2,
  refresh = 0
)
dof(model)}
}
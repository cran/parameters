% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters_type.R
\name{parameters_type}
\alias{parameters_type}
\title{Type of model parameters}
\usage{
parameters_type(model, ...)
}
\arguments{
\item{model}{A statistical model.}

\item{...}{Arguments passed to or from other methods.}
}
\value{
A data frame.
}
\description{
In a regression model, the parameters do not all have the meaning. For
instance, the intercept has to be interpreted as theoretical outcome value
under some conditions (when predictors are set to 0), whereas other
coefficients are to be interpreted as amounts of change. Others, such as
interactions, represent changes in another of the parameter. The
\code{parameters_type} function attempts to retrieve information and meaning
of parameters. It outputs a dataframe of information for each parameters,
such as the \code{Type} (whether the parameter corresponds to a factor or a
numeric predictor, or whether it is a (regular) interaction or a nested
one), the \code{Link} (whether the parameter can be interpreted as a mean
value, the slope of an association or a difference between two levels) and,
in the case of interactions, which other parameters is impacted by which
parameter.
}
\examples{
library(parameters)

model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
parameters_type(model)

# Interactions
model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Sepal.Width * Species * Petal.Length, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species / Sepal.Width, data = iris)
parameters_type(model)


# Complex interactions
data <- iris
data$fac2 <- ifelse(data$Sepal.Width > mean(data$Sepal.Width), "A", "B")
model <- lm(Sepal.Length ~ Species / fac2 / Petal.Length, data = data)
parameters_type(model)

model <- lm(Sepal.Length ~ Species / fac2 * Petal.Length, data = data)
parameters_type(model)
}

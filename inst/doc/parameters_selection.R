## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = ">")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("performance", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

## ----message=FALSE, warning=FALSE---------------------------------------------
model <- lm(Sepal.Length ~ .*., data=iris)
summary(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(performance)

check_normality(model)
check_heteroscedasticity(model)
check_autocorrelation(model)
check_collinearity(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
lm(Sepal.Length ~ .*., data = iris) %>% 
  select_parameters() %>% 
  summary()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(lme4)

lmer(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + (1|Species), data = iris) %>%
  select_parameters() %>%
  summary()


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

if (!requireNamespace("mice", quietly = TRUE) ||
    !requireNamespace("GLMMadaptive", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(333)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(mice)
library(parameters)

data("nhanes2")
imp <- mice(nhanes2, printFlag = FALSE)
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))

model_parameters(fit)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(lme4)
library(GLMMadaptive)

data(cbpp)
cbpp$period[sample(1:nrow(cbpp), size = 10)] <- NA

imputed_data <- mice(cbpp, printFlag = FALSE)

## ----message=FALSE, eval=FALSE------------------------------------------------
#  fit <- with(data = imputed_data, expr = GLMMadaptive::mixed_model(
#    cbind(incidence, size - incidence) ~ period,
#    random = ~ 1 | herd,
#    family = binomial
#  ))
#  #> Error in as.data.frame(data) :
#  #>   argument "data" is missing, with no default

## ----message=FALSE------------------------------------------------------------
models <- lapply(1:imputed_data$m, function(i) {
  mixed_model(
    cbind(incidence, size - incidence) ~ period,
    random = ~ 1 | herd,
    data = complete(imputed_data, action = i),
    family = binomial
  )
})
pool_parameters(models)

## ----message=FALSE------------------------------------------------------------
library(mice)
library(parameters)

data("nhanes2")
imp <- mice(nhanes2, printFlag = FALSE)

# approach when model is supported by "mice"
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
summary(pool(fit))

# approach when model is *not* supported by "mice"
models <- lapply(1:5, function(i) {
  lm(bmi ~ age + hyp + chl, data = complete(imp, action = i))
})
pool_parameters(models)

## ----message=FALSE, warning=FALSE---------------------------------------------
data("nhanes2")
imp <- mice(nhanes2, printFlag = FALSE)
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
pooled <- pool(fit)

model_parameters(pooled)


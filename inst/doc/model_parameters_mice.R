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

## ----message=FALSE------------------------------------------------------------
library(mice)
library(parameters)

data("nhanes2")
imp <- mice(nhanes2)
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))

model_parameters(fit)

## ----message=FALSE------------------------------------------------------------
library(lme4)
library(GLMMadaptive)

data(cbpp)
cbpp$period[sample(1:nrow(cbpp), size = 10)] <- NA

imputed_data <- mice(cbpp)

## ----message=FALSE, eval=FALSE------------------------------------------------
#  fit <- with(data = imputed_data, expr = GLMMadaptive::mixed_model(
#    cbind(incidence, size - incidence) ~ period,
#    random = ~ 1 | herd,
#    family = binomial
#  ))
#  #> Error in as.data.frame(data) :
#  #>   argument "data" is missing, with no default

## ----message=FALSE------------------------------------------------------------
analyses <- as.list(seq_len(imputed_data$m))
for (i in seq_along(analyses)) {
  data.i <- complete(imputed_data, i)
  analyses[[i]] <- mixed_model(
    cbind(incidence, size - incidence) ~ period,
    random = ~ 1 | herd,
    data = data.i,
    family = binomial
  )
}
object <- list(analyses = analyses)
class(object) <- c("mira", "matrix", "list")

model_parameters(object)

## ----message=FALSE------------------------------------------------------------
data("nhanes2")
imp <- mice(nhanes2)
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
pooled <- pool(fit)

model_parameters(pooled)


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE, 
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  eval = TRUE
)

if (!requireNamespace("lme4", quietly = TRUE) ||
    !requireNamespace("lfe", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(lme4)
  library(lfe)
}

set.seed(333)

## -----------------------------------------------------------------------------
library(parameters)
data("qol_cancer")

## -----------------------------------------------------------------------------
qol_cancer <- cbind(
  qol_cancer,
  demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
)

## -----------------------------------------------------------------------------
fe_model1 <- lm(
  QoL ~ 0 + time + phq4_within + ID,
   data = qol_cancer
)
# we use only the first two rows, because the remaining rows are
# the estimates for "ID", which is not of interest here...
model_parameters(fe_model1)[1:2, ]


# instead of removing the intercept, we could also use the 
# de-meaned response...
fe_model2 <- lm(
  QoL_within ~ time + phq4_within + ID,
   data = qol_cancer
)
model_parameters(fe_model2)[2:3, ]


# we compare the results with those from the "lfe"-package for panel data
library(lfe)
fe_model3 <- felm(
  QoL ~ time + phq4_within | ID,
   data = qol_cancer
)
model_parameters(fe_model3)

## -----------------------------------------------------------------------------
library(lme4)
mixed_1 <- lmer(
  QoL ~ time + phq4_within + phq4_between + (1 | ID),
   data = qol_cancer
)
model_parameters(mixed_1)

# compare to FE-model
model_parameters(fe_model1)[1:2, ]

## -----------------------------------------------------------------------------
mixed_2 <- lmer(
  QoL ~ time + phq4_within + phq4_between + education + (1 + time | ID),
   data = qol_cancer
)
model_parameters(mixed_2)

## ----echo=FALSE---------------------------------------------------------------
f <- "y<sub>it</sub> = &beta;<sub>0</sub> + &beta;<sub>1W</sub> (x<sub>it</sub> - &#x035E;x<sub>i</sub>) + &beta;<sub>2B</sub> &#x035E;x<sub>i</sub> + &beta;<sub>3</sub> z<sub>i</sub> + &upsilon;<sub>i0</sub> + &upsilon;<sub>i1</sub> (x<sub>it</sub> - &#x035E;x<sub>i</sub>) + &epsilon;<sub>it</sub>"
knitr::asis_output(f)

## ----echo=FALSE---------------------------------------------------------------
f <- "<ul><li>x<sub>it</sub> - &#x035E;x<sub>i</sub> is the de-meaned predictor, <em>phq4_within</em></li><li>&#x035E;x<sub>i</sub> is the group-meaned predictor, <em>phq4_between</em></li><li>&beta;<sub>1W</sub> is the coefficient for phq4_within (within-subject)</li><li>&beta;<sub>2B</sub> is the coefficient for phq4_between (bewteen-subject)</li><li>&beta;<sub>3</sub> is the coefficient for time-constant predictors, such as `hospital` or `education` (bewteen-subject)</li></ul>"
knitr::asis_output(f)

## -----------------------------------------------------------------------------
rewb <- lmer(
  QoL ~ time + phq4_within + phq4_between + education + 
    (1 + time | ID) + (1 + phq4_within | ID),
   data = qol_cancer
)

## -----------------------------------------------------------------------------
model_parameters(rewb)

## -----------------------------------------------------------------------------
random_parameters(rewb)


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = ">")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("BayesFactor", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE) ||
    !requireNamespace("metafor", quietly = TRUE) ||
    !requireNamespace("lavaan", quietly = TRUE) ||
    !requireNamespace("brms", quietly = TRUE) ||
    !requireNamespace("psych", quietly = TRUE) ||
    !requireNamespace("rstanarm", quietly = TRUE) ||
    !requireNamespace("FactoMineR", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

## ---- warning=FALSE, message=FALSE--------------------------------------------
cor.test(iris$Sepal.Length, iris$Sepal.Width) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
t.test(mpg ~ vs, data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(BayesFactor)

BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
BayesFactor::ttestBF(formula = mpg ~ vs, data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
aov(Sepal.Length ~ Species, data = iris) %>% 
  parameters(omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE)

## ---- warning=FALSE, message=FALSE--------------------------------------------
aov(mpg ~ am + Error(gear), data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(lme4)

lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE, eval = FALSE------------------------------
#  library(rstanarm)
#  
#  stan_glm(mpg ~ wt * cyl, data = mtcars) %>%
#    parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
library(rstanarm)

stan_glm(mpg ~ wt * cyl, data = mtcars, iter = 500, chains = 2, refresh = 0) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(brms)
data(fish)
set.seed(123)
model <- brm(bf(
   count ~ persons + child + camper + (1 | persons),
   zi ~ child + camper + (1 | persons)
 ),
 data = fish,
 family = zero_inflated_poisson(),
 iter = 500,
 chains = 1,
 refresh = 0
)
parameters(model, component = "conditional")

parameters(model, effects = "all", component = "all")

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(psych)

psych::pca(mtcars, nfactors = 3) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE, eval = FALSE------------------------------
#  library(FactoMineR)
#  
#  FactoMineR::FAMD(iris, ncp = 3) %>%
#    parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
library(FactoMineR)

FactoMineR::FAMD(iris, ncp = 3, graph = FALSE) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(lavaan)

model <- lavaan::cfa(' visual  =~ x1 + x2 + x3
                       textual =~ x4 + x5 + x6
                       speed   =~ x7 + x8 + x9 ', 
                       data = HolzingerSwineford1939)

model_parameters(model)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(metafor)

mydat <- data.frame(
  effectsize = c(-0.393, 0.675, 0.282, -1.398),
  standarderror = c(0.317, 0.317, 0.13, 0.36)
)

rma(yi = effectsize, sei = standarderror, method = "REML", data = mydat) %>% 
  model_parameters()


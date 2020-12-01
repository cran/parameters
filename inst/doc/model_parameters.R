## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

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
  parameters(omega_squared = "partial", eta_squared = "partial", epsilon_squared = "partial")

## ---- warning=FALSE, message=FALSE--------------------------------------------
aov(Sepal.Length ~ Species * Sepal.Width, data = iris) %>%
  parameters(omega_squared = "partial", eta_squared = "partial", ci = .8)

## ---- warning=FALSE, message=FALSE--------------------------------------------
aov(mpg ~ am + Error(gear), data = mtcars) %>%
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial()) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial()) %>% 
  parameters(exponentiate = TRUE, df_method = "wald")

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(lme4)

lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(GLMMadaptive)
library(glmmTMB)
data("Salamanders")
model <- mixed_model(
  count ~ spp + mined,
  random = ~1 | site,
  zi_fixed = ~spp + mined,
  family = zi.negative.binomial(), 
  data = Salamanders
)
parameters(model)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(glmmTMB)
sim1 <- function(nfac = 40, nt = 100, facsd = 0.1, tsd = 0.15, mu = 0, residsd = 1) {
  dat <- expand.grid(fac = factor(letters[1:nfac]), t = 1:nt)
  n <- nrow(dat)
  dat$REfac <- rnorm(nfac, sd = facsd)[dat$fac]
  dat$REt <- rnorm(nt, sd = tsd)[dat$t]
  dat$x <- rnorm(n, mean = mu, sd = residsd) + dat$REfac + dat$REt
  dat
}
set.seed(101)
d1 <- sim1(mu = 100, residsd = 10)
d2 <- sim1(mu = 200, residsd = 5)
d1$sd <- "ten"
d2$sd <- "five"
dat <- rbind(d1, d2)
model <- glmmTMB(x ~ sd + (1 | t), dispformula =  ~ sd, data = dat)

parameters(model)

## ---- warning=FALSE, message=FALSE, eval = FALSE------------------------------
#  library(rstanarm)
#  
#  stan_glm(mpg ~ wt * cyl, data = mtcars) %>%
#    parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
library(rstanarm)

stan_glm(mpg ~ wt * cyl, data = mtcars, iter = 500, chains = 2, refresh = 0) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE, eval=FALSE--------------------------------
#  library(brms)
#  data(fish)
#  set.seed(123)
#  model <- brm(bf(
#     count ~ persons + child + camper + (1 | persons),
#     zi ~ child + camper + (1 | persons)
#   ),
#   data = fish,
#   family = zero_inflated_poisson()
#  )
#  parameters(model, component = "conditional")
#  #> # Fixed effects
#  #>
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE |  Rhat |  ESS
#  #> -------------------------------------------------------------------------
#  #> (Intercept) |  -0.86 | [-1.40, -0.12] | 97.88% |     0.68% | 1.014 |  178
#  #> persons     |   0.84 | [ 0.64,  1.05] |   100% |        0% | 1.014 |  171
#  #> child       |  -1.16 | [-1.30, -1.00] |   100% |        0% | 1.001 | 1421
#  #> camper1     |   0.73 | [ 0.59,  0.88] |   100% |        0% | 1.000 | 3552
#  #>
#  #> Using highest density intervals as credible intervals.
#  
#  parameters(model, effects = "all", component = "all")
#  #> # Fixed effects (conditional)
#  #>
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE |  Rhat |  ESS
#  #> -------------------------------------------------------------------------
#  #> (Intercept) |  -0.86 | [-1.40, -0.12] | 97.88% |     0.68% | 1.014 |  178
#  #> persons     |   0.84 | [ 0.64,  1.05] |   100% |        0% | 1.014 |  171
#  #> child       |  -1.16 | [-1.30, -1.00] |   100% |        0% | 1.001 | 1421
#  #> camper1     |   0.73 | [ 0.59,  0.88] |   100% |        0% | 1.000 | 3552
#  #>
#  #> # Fixed effects (zero-inflated)
#  #>
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE |  Rhat |  ESS
#  #> -------------------------------------------------------------------------
#  #> (Intercept) |  -0.65 | [-1.81,  0.37] | 84.72% |     7.25% | 1.007 |  988
#  #> child       |   1.86 | [ 1.33,  2.39] |   100% |        0% | 1.003 |  999
#  #> camper1     |  -0.81 | [-1.39, -0.23] | 98.75% |     1.65% | 1.001 | 2103
#  #>
#  #> # Random effects (conditional) SD/Cor: persons
#  #>
#  #> Parameter   | Median |       89% CI |   pd | % in ROPE |  Rhat | ESS
#  #> --------------------------------------------------------------------
#  #> (Intercept) |   0.12 | [0.00, 0.44] | 100% |    43.25% | 1.018 | 293
#  #>
#  #> # Random effects (zero-inflated) SD/Cor: persons
#  #>
#  #> Parameter    | Median |       89% CI |   pd | % in ROPE |  Rhat | ESS
#  #> ---------------------------------------------------------------------
#  #> zi_Intercept |   1.29 | [0.52, 2.56] | 100% |        0% | 1.009 | 484
#  #>
#  #> Using highest density intervals as credible intervals.

## ----warning=FALSE, message=FALSE, eval=FALSE---------------------------------
#  parameters(model, effects = "all", component = "conditional", group_level = TRUE)
#  #> # Fixed effects
#  #>
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE |  Rhat |  ESS
#  #> -------------------------------------------------------------------------
#  #> (Intercept) |  -0.86 | [-1.40, -0.12] | 97.88% |     0.68% | 1.014 |  178
#  #> persons     |   0.84 | [ 0.64,  1.05] |   100% |        0% | 1.014 |  171
#  #> child       |  -1.16 | [-1.30, -1.00] |   100% |        0% | 1.001 | 1421
#  #> camper1     |   0.73 | [ 0.59,  0.88] |   100% |        0% | 1.000 | 3552
#  #>
#  #> # Random effects Intercept: persons
#  #>
#  #> Parameter |    Median |        89% CI |     pd | % in ROPE |  Rhat | ESS
#  #> ------------------------------------------------------------------------
#  #> persons.1 | -4.77e-03 | [-0.35, 0.36] | 54.00% |    61.05% | 1.019 | 182
#  #> persons.2 |      0.02 | [-0.15, 0.34] | 65.18% |    62.30% | 1.011 | 230
#  #> persons.3 |     -0.01 | [-0.23, 0.15] | 58.48% |    68.80% | 1.006 | 341
#  #> persons.4 |  2.72e-03 | [-0.22, 0.34] | 52.78% |    62.28% | 1.009 | 250
#  #>
#  #> # Random effects SD/Cor: persons
#  #>
#  #> Parameter   | Median |       89% CI |   pd | % in ROPE |  Rhat | ESS
#  #> --------------------------------------------------------------------
#  #> (Intercept) |   0.12 | [0.00, 0.44] | 100% |    43.25% | 1.018 | 293
#  #>
#  #> Using highest density intervals as credible intervals.

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


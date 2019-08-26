
# parameters <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/parameters)](https://cran.r-project.org/package=parameters)
[![downloads](http://cranlogs.r-pkg.org/badges/parameters)](https://cran.r-project.org/package=parameters)
[![Build
Status](https://travis-ci.org/easystats/parameters.svg?branch=master)](https://travis-ci.org/easystats/parameters)
[![codecov](https://codecov.io/gh/easystats/parameters/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/parameters)

***Describe and understand your model’s parameters\!***

`parameters`’s primary goal is to provide utilities for processing the
parameters of various statistical models. Beyond computing
***p*-values**, **CIs**, **Bayesian indices** and other measures for a
wide variety of models, this package implements features like
**standardization** or **bootstrapping** of parameters and models,
**feature reduction** (feature extraction and variable selection) as
well as conversion between indices of **effect size**.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/parameters")
```

``` r
library("parameters")
```

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-parameters-orange.svg?colorB=E91E63)](https://easystats.github.io/parameters/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-parameters-orange.svg?colorB=2196F3)](https://easystats.github.io/parameters/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/parameters/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

#### Parameters Engineering

  - [Bootstrapped
    parameters](https://easystats.github.io/parameters/articles/bootstrapping.html)
  - [Standardized
    parameters](https://easystats.github.io/parameters/articles/standardization.html)

#### Variable/Feature Reduction

  - [How many factors to retain in Factor Analysis
    (FA)](https://easystats.github.io/parameters/articles/n_factors.html)
  - [Variable extraction (PCA, FA,
    …)](https://easystats.github.io/parameters/articles/variable_extraction.html)
  - [Variable selection (stepwise, projpred,
    …)](https://easystats.github.io/parameters/articles/variable_selection.html)

# Features

## Model’s parameters description

<img src='man/figures/figure1.png' align="center" />

The `model_parameters` function allows you to extract the parameters and
their characteristics from various models in a consistent way. It can be
considered as a lightweight alternative to
[`broom::tidy()`](https://github.com/tidymodels/broom), with some
notable differences:

  - The names of the returned dataframe are **specific** to their
    content. For instance, the column containing the statistic is named
    following the statistic name, *i.e.*, *t*, *z*, etc., instead of a
    generic name such as *statistic*.
  - It is able to compute or extract indices not available by default,
    such as ***p* values**, **CIs**, etc.
  - It includes **feature engineering** capabilities, including
    [**bootstrapping**](https://easystats.github.io/parameters/articles/bootstrapping.html)
    and
    [**standardization**](https://easystats.github.io/parameters/articles/standardization.html)
    of parameters.

### Correlations

#### Frequentist

``` r
model <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
model_parameters(model)
```

    # Parameter1        |       Parameter2 |     r |     t |  df |    p |        95% CI |  Method
    # -------------------------------------------------------------------------------------------
    # iris$Sepal.Length | iris$Sepal.Width | -0.12 | -1.44 | 148 | > .1 | [-0.27, 0.04] | Pearson

#### Bayesian

``` r
library(BayesFactor)

model <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width)
model_parameters(model)
```

    # Parameter | Median |        89% CI |     pd | % in ROPE |              Prior |   BF
    # -----------------------------------------------------------------------------------
    # rho       |  -0.11 | [-0.23, 0.02] | 92.90% |    43.13% | Cauchy (0 +- 0.33) | 0.51

### *t*-tests

#### Frequentist

``` r
df <- iris
df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

model <- t.test(Sepal.Length ~ Sepal.Big, data = df)
model_parameters(model)
```

    # Parameter    |     Group | Mean_Group1 | Mean_Group2 | Difference |    t |     df |    p |        95% CI |                  Method
    # ----------------------------------------------------------------------------------------------------------------------------------
    # Sepal.Length | Sepal.Big |        5.95 |        5.78 |      -0.18 | 1.36 | 142.15 | > .1 | [-0.08, 0.43] | Welch Two Sample t-test

#### Bayesian

``` r
model <- BayesFactor::ttestBF(formula = Sepal.Length ~ Sepal.Big, 
    data = df)
model_parameters(model)
```

    # Parameter  | Median |        89% CI |     pd | % in ROPE |              Prior |   BF
    # ------------------------------------------------------------------------------------
    # Difference |   0.16 | [-0.05, 0.37] | 88.85% |    26.79% | Cauchy (0 +- 0.71) | 0.38

### ANOVAs

#### Simple

``` r
model <- aov(Sepal.Length ~ Sepal.Big, data = df)
model_parameters(model, omega_squared = "partial", eta_squared = "partial", 
    epsilon_squared = TRUE)
```

    # Parameter | Sum_Squares |  df | Mean_Square |    F |    p | Omega_Sq (partial) | Eta_Sq (partial) | Epsilon_sq
    # --------------------------------------------------------------------------------------------------------------
    # Sepal.Big |        1.10 |   1 |        1.10 | 1.61 | > .1 |               0.00 |             0.01 |       0.00
    # Residuals |      101.07 | 148 |        0.68 |      |   NA |                    |                  |

#### Repeated measures

``` r
model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
model_parameters(model)
```

    # Group   | Parameter | Sum_Squares |  df | Mean_Square |     F |      p
    # ----------------------------------------------------------------------
    # Species | Sepal.Big |       28.27 |   1 |       28.27 |  0.81 |   > .1
    # Species | Residuals |       34.94 |   1 |       34.94 |       |     NA
    # Within  | Sepal.Big |        4.74 |   1 |        4.74 | 20.24 | < .001
    # Within  | Residuals |       34.21 | 146 |        0.23 |       |     NA

``` r
library(lme4)
model <- anova(lmer(Sepal.Length ~ Sepal.Big + (1 | Species), 
    data = df))
model_parameters(model)
```

    # Parameter | Sum_Squares | df | Mean_Square |     F
    # --------------------------------------------------
    # Sepal.Big |        4.64 |  1 |        4.64 | 19.82

### General Linear Models (GLM)

``` r
model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
model_parameters(model, standardize = "refit")
```

    # Parameter   | Coefficient |   SE |         95% CI |     z | df |     p | Coefficient (std.)
    # -------------------------------------------------------------------------------------------
    # (Intercept) |       10.62 | 4.17 |  [4.79, 22.66] |  2.55 | 29 | < .05 |              -0.76
    # wt          |        2.10 | 1.55 |  [-0.53, 6.24] |  1.36 | 29 |  > .1 |               2.05
    # cyl         |       -2.93 | 1.38 | [-6.92, -1.07] | -2.12 | 29 | < .05 |              -5.23

### Bootstrapped models

``` r
model <- lm(mpg ~ drat * cyl, data = mtcars)
model_parameters(model, bootstrap = TRUE, iterations = 500)
```

    # Parameter   | Coefficient |          95% CI |     p | Coefficient (std.)
    # ------------------------------------------------------------------------
    # (Intercept) |       -0.94 | [-45.58, 32.09] |  > .1 |              -0.13
    # drat        |        9.55 |   [0.55, 21.32] | < .05 |               0.16
    # cyl         |        2.18 |   [-2.50, 8.41] |  > .1 |              -0.71
    # drat * cyl  |       -1.25 |   [-2.92, 0.12] |  0.06 |              -0.19

### Mixed models

``` r
library(lme4)

model <- lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
model_parameters(model, standardize = "refit")
```

    # Parameter    | Coefficient |   SE |        95% CI |    t |      p | Coefficient (std.)
    # --------------------------------------------------------------------------------------
    # (Intercept)  |        2.00 | 0.56 | [-1.92, 5.92] | 3.56 | < .001 |               0.00
    # Petal.Length |        0.28 | 0.06 | [-0.27, 0.83] | 4.75 | < .001 |               1.14

### Bayesian models

``` r
library(rstanarm)

model <- stan_glm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model)
```

    # Parameter   | Median |         89% CI |     pd | % in ROPE |  ESS |  Rhat |               Prior
    # -----------------------------------------------------------------------------------------------
    # (Intercept) |  39.65 | [36.79, 42.34] |   100% |        0% | 4833 | 1.000 | Normal (0 +- 60.27)
    # wt          |  -3.17 | [-4.39, -1.93] |   100% |     0.02% | 2206 | 1.001 | Normal (0 +- 15.40)
    # cyl         |  -1.52 | [-2.24, -0.89] | 99.95% |     1.90% | 2209 | 1.001 |  Normal (0 +- 8.44)

### Exploratory Factor Analysis (EFA) and Principal Component Analysis (PCA)

``` r
library(psych)

model <- psych::fa(attitude, nfactors = 3)
model_parameters(model)
```

    # # Rotated loadings from Principal Component Analysis (oblimin-rotation)
    # 
    # Variable   |   MR1 |   MR2 |   MR3 | Complexity | Uniqueness
    # ------------------------------------------------------------
    # rating     |  0.90 | -0.07 | -0.05 |       1.02 |       0.23
    # complaints |  0.97 | -0.06 |  0.04 |       1.01 |       0.10
    # privileges |  0.44 |  0.25 | -0.05 |       1.64 |       0.65
    # learning   |  0.47 |  0.54 | -0.28 |       2.51 |       0.24
    # raises     |  0.55 |  0.43 |  0.25 |       2.35 |       0.23
    # critical   |  0.16 |  0.17 |  0.48 |       1.46 |       0.67
    # advance    | -0.11 |  0.91 |  0.07 |       1.04 |       0.22
    # 
    # The 3 latent factors (oblimin rotation) accounted for 66.60% of the total variance of the original data (MR1 = 38.19%, MR2 = 22.69%, MR3 = 5.72%).

### Confirmatory Factor Analysis (CFA) and Structural Equation Models (SEM)

``` r
library(lavaan)

model <- lavaan::cfa(" visual  =~ x1 + x2 + x3
                       textual =~ x4 + x5 + x6
                       speed   =~ x7 + x8 + x9 ", 
    data = HolzingerSwineford1939)
model_parameters(model)
```

    # Link              | Coefficient |   SE |       95% CI |      p |        Type
    # ----------------------------------------------------------------------------
    # visual =~ x1      |        1.00 | 0.00 | [1.00, 1.00] | < .001 |     Loading
    # visual =~ x2      |        0.55 | 0.10 | [0.36, 0.75] | < .001 |     Loading
    # visual =~ x3      |        0.73 | 0.11 | [0.52, 0.94] | < .001 |     Loading
    # textual =~ x4     |        1.00 | 0.00 | [1.00, 1.00] | < .001 |     Loading
    # textual =~ x5     |        1.11 | 0.07 | [0.98, 1.24] | < .001 |     Loading
    # textual =~ x6     |        0.93 | 0.06 | [0.82, 1.03] | < .001 |     Loading
    # speed =~ x7       |        1.00 | 0.00 | [1.00, 1.00] | < .001 |     Loading
    # speed =~ x8       |        1.18 | 0.16 | [0.86, 1.50] | < .001 |     Loading
    # speed =~ x9       |        1.08 | 0.15 | [0.79, 1.38] | < .001 |     Loading
    # visual ~~ textual |        0.41 | 0.07 | [0.26, 0.55] | < .001 | Correlation
    # visual ~~ speed   |        0.26 | 0.06 | [0.15, 0.37] | < .001 | Correlation
    # textual ~~ speed  |        0.17 | 0.05 | [0.08, 0.27] | < .001 | Correlation

## Variable and parameters selection

<img src='man/figures/figure2.png' align="center" />

### General Linear Models (GLM)

``` r
library(dplyr)

lm(disp ~ ., data = mtcars) %>% parameters_selection() %>% model_parameters()
```

    # Parameter   | Coefficient |     SE |            95% CI |     t | df |      p | Coefficient (std.)
    # -------------------------------------------------------------------------------------------------
    # (Intercept) |      141.70 | 125.67 | [-116.62, 400.02] |  1.13 | 26 |   > .1 |              -0.00
    # cyl         |       13.14 |   7.90 |    [-3.10, 29.38] |  1.66 | 26 |   > .1 |               0.19
    # hp          |        0.63 |   0.20 |      [0.22, 1.03] |  3.18 | 26 |  < .01 |               0.35
    # wt          |       80.45 |  12.22 |   [55.33, 105.57] |  6.58 | 26 | < .001 |               0.64
    # qsec        |      -14.68 |   6.14 |   [-27.31, -2.05] | -2.39 | 26 |  < .05 |              -0.21
    # carb        |      -28.75 |   5.60 |  [-40.28, -17.23] | -5.13 | 26 | < .001 |              -0.37

### Mixed models

``` r
library(lme4)

lmer(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + 
    (1 | Species), data = iris) %>% parameters_selection() %>% 
    model_parameters()
```

    # Parameter                                  | Coefficient |   SE |        95% CI |     t |     p | Coefficient (std.)
    # --------------------------------------------------------------------------------------------------------------------
    # (Intercept)                                |        1.78 | 0.91 | [-1.71, 5.26] |  1.95 |  0.05 |              -0.24
    # Sepal.Width                                |        0.88 | 0.50 | [-0.84, 2.60] |  1.77 |  0.08 |               0.30
    # Petal.Length                               |        0.28 | 0.24 | [-0.27, 0.83] |  1.16 |  > .1 |               1.57
    # Petal.Width                                |       -2.04 | 1.52 | [1.96, -6.03] | -1.34 |  > .1 |              -0.43
    # Sepal.Width * Petal.Length                 |        0.75 | 0.27 | [-0.72, 2.22] |  2.80 | < .01 |              -0.15
    # Sepal.Width * Petal.Width                  |       -0.10 | 0.16 | [0.10, -0.30] | -0.63 |  > .1 |               0.07
    # Petal.Length * Petal.Width                 |       -0.05 | 0.08 | [0.04, -0.14] | -0.61 |  > .1 |               0.22
    # (Sepal.Width * Petal.Length) * Petal.Width |        0.34 | 0.49 | [-0.33, 1.02] |  0.70 |  > .1 |              -0.03

### Bayesian models

``` r
library(rstanarm)

model <- stan_glm(mpg ~ ., data = mtcars) %>% parameters_selection() %>% 
    model_parameters()
```

    # Parameter   | Median |         89% CI |     pd | % in ROPE |  ESS |  Rhat |               Prior
    # -----------------------------------------------------------------------------------------------
    # (Intercept) |  19.82 | [-1.20, 41.01] | 93.90% |     1.30% | 1254 | 0.999 | Normal (0 +- 60.27)
    # wt          |  -3.96 | [-6.01, -1.79] | 99.65% |     0.60% | 1249 | 0.999 | Normal (0 +- 15.40)
    # cyl         |  -0.50 |  [-1.85, 0.74] | 72.90% |    45.90% | 1472 | 1.000 |  Normal (0 +- 8.44)
    # hp          |  -0.02 |  [-0.04, 0.01] | 88.20% |      100% | 1813 | 0.999 |  Normal (0 +- 0.22)
    # am          |   2.93 |   [0.24, 5.80] | 94.85% |     6.85% | 1500 | 1.000 | Normal (0 +- 15.07)
    # qsec        |   0.80 |  [-0.04, 1.79] | 92.15% |    35.40% | 1237 | 0.999 |  Normal (0 +- 8.43)
    # disp        |   0.01 |  [-0.01, 0.03] | 86.10% |      100% | 1409 | 1.000 |  Normal (0 +- 0.12)

## Variable and features extraction

### How many factors to retain in Factor Analysis (FA)

``` r
n_factors(mtcars)
```

    # # Method Agreement Procedure:
    # 
    # The choice of 2 dimensions is supported by 2 (40.00%) methods out of 5 (EGA (glasso), EGA (TMFG)).

## Miscellaneous

### Describe a Distribution

``` r
x <- rnorm(300)
describe_distribution(x)
```

``` r
knitr::kable(describe_distribution(rnorm(300)), digits = 1)
```

| Mean | SD | Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ---: | -: | --: | --: | -------: | -------: | --: | ---------: |
|    0 |  1 | \-2 |   3 |        0 |    \-0.4 | 300 |          0 |

### Standardization and normalization

``` r
df <- standardize(iris)
summary(df$Sepal.Length)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    -1.9    -0.9    -0.1     0.0     0.7     2.5

df <- normalize(iris)
summary(df$Sepal.Length)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     0.0     0.2     0.4     0.4     0.6     1.0
```

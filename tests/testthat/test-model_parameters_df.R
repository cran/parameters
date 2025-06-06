skip_on_cran()

# glm ---------------------------

set.seed(123)
data(mtcars)
model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

test_that("model_parameters.glm", {
  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(4.7888, -0.52956, -6.91917), tolerance = 1e-3)
  expect_equal(params$p, c(0.01084, 0.17431, 0.03362), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(2.4503, -0.9299, -5.63472), tolerance = 1e-3)
  expect_equal(params$p, c(0.01084, 0.17431, 0.03362), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "residual"))
  expect_equal(params$df_error, c(29, 29, 29), tolerance = 1e-3)
  expect_equal(params$CI_low, c(2.09492, -1.06171, -5.75235), tolerance = 1e-3)
  expect_equal(params$p, c(0.0164, 0.18479, 0.04227), tolerance = 1e-3)
})


# PROreg ---------------------------


test_that("model_parameters.BBmm", {
  skip_if_not_installed("PROreg", minimum_version = "1.3.0")
  set.seed(1234)

  # defining the parameters
  k <- 100
  m <- 10
  phi <- 0.5
  beta <- c(1.5, -1.1)
  sigma <- 0.5

  # simulating the covariate and random effects
  x <- runif(k, 0, 10)
  X <- model.matrix(~x)
  z <- as.factor(PROreg::rBI(k, 4, 0.5, 2))
  Z <- model.matrix(~ z - 1)
  u <- rnorm(5, 0, sigma)

  # the linear predictor and simulated response variable
  eta <- beta[1] + beta[2] * x + crossprod(t(Z), u)
  p <- 1 / (1 + exp(-eta))
  y <- PROreg::rBB(k, m, p, phi)
  dat <- data.frame(cbind(y, x, z))
  dat$z <- as.factor(dat$z)

  # apply the model
  invisible(capture.output({
    model <- PROreg::BBmm(
      fixed.formula = y ~ x,
      random.formula = ~z,
      m = m,
      data = dat
    )
  }))
  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(96, 96), tolerance = 1e-3)
  expect_equal(params$CI_low, c(0.26366, -1.46628), tolerance = 1e-3)
  expect_equal(params$p, c(0.00814, 0), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(0.27313, -1.46119), tolerance = 1e-3)
  expect_equal(params$p, c(0.00814, 0), tolerance = 1e-3)
})

test_that("model_parameters.BBreg", {
  skip_if_not_installed("PROreg", minimum_version = "1.3.0")
  set.seed(18)
  # we simulate a covariate, fix the paramters of the beta-binomial
  # distribution and simulate a response variable.
  # then we apply the model, and try to get the same values.
  k <- 1000
  m <- 10
  x <- rnorm(k, 5, 3)
  beta <- c(-10, 2)
  p <- 1 / (1 + exp(-1 * (beta[1] + beta[2] * x)))
  phi <- 1.2
  y <- PROreg::rBB(k, m, p, phi)

  # model
  model <- PROreg::BBreg(y ~ x, m)

  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(997, 997), tolerance = 1e-3)
  expect_equal(params$CI_low, c(-11.08184, 1.84727), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(-11.08069, 1.84749), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0), tolerance = 1e-3)
})


# MASS / nnet ---------------------------

test_that("model_parameters.multinom", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("nnet")
  set.seed(123)
  utils::example(topic = birthwt, echo = FALSE, package = "MASS")

  # model
  model <- nnet::multinom(
    formula = low ~ .,
    data = bwt,
    trace = FALSE
  )
  params <- suppressWarnings(model_parameters(model, ci_method = "wald"))
  expect_equal(params$df_error, c(178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178), tolerance = 1e-3)
  expect_equal(params$CI_low, c(
    -1.6332, -0.11362, -0.02963, 0.13471, -0.17058,
    -0.08325, 0.39528, 0.49086, -0.23614, -1.38245, -0.72163
  ), tolerance = 1e-3)
  expect_equal(params$p, c(
    0.50926, 0.33729, 0.02833, 0.02736, 0.11049, 0.07719, 0.00575,
    0.00866, 0.14473, 0.36392, 0.69537
  ), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(
    -1.6165, -0.1131, -0.02953, 0.1419, -0.16439, -0.07755, 0.40173,
    0.50053, -0.22991, -1.37601, -0.71551
  ), tolerance = 1e-3)
  expect_equal(params$p, c(
    0.5084, 0.33599, 0.02706, 0.0261, 0.10872, 0.07548, 0.00518,
    0.00794, 0.14296, 0.36269, 0.6949
  ), tolerance = 1e-3)
})


## TODO: archieved on CRAN - add test back once ivprobit is back on CRAN.

# ivprobit ---------------------------
# test_that("model_parameters.ivprobit", {
#   skip_if_not_installed("ivprobit")
#   set.seed(123)
#   data(eco, package = "ivprobit")

#   # model
#   model <- ivprobit::ivprobit(
#     formula = d2 ~ ltass + roe + div | eqrat + bonus | ltass + roe + div + gap + cfa,
#     data = eco
#   )

#   params <- suppressWarnings(model_parameters(model))
#   expect_equal(params$df_error, c(789L, 789L, 789L, 789L, 789L, 789L), tolerance = 1e-3)
#   expect_equal(params$CI_low, c(-35.96484, -0.27082, -0.15557, -1e-05, -15.95755, -1e-05), tolerance = 1e-3)
#   expect_equal(params$p, c(0.08355, 0.12724, 0.55684, 0.63368, 0.46593, 0.61493), tolerance = 1e-3)

#   params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
#   expect_equal(params$df_error, c(Inf, Inf, Inf, Inf, Inf, Inf), tolerance = 1e-3)
#   expect_equal(params$CI_low, c(-35.93553, -0.26895, -0.15522, -1e-05, -15.91859, -1e-05), tolerance = 1e-3)
#   expect_equal(params$p, c(0.08316, 0.12684, 0.55668, 0.63355, 0.46571, 0.61479), tolerance = 1e-3)
# })


# biglm ---------------------------

test_that("model_parameters.bigglm", {
  skip_if_not_installed("biglm")
  set.seed(123)
  data(trees)

  # model
  model <- biglm::bigglm(
    formula = log(Volume) ~ log(Girth) + log(Height),
    data = trees,
    chunksize = 10,
    sandwich = TRUE
  )
  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(28, 28, 28), tolerance = 1e-3)
  expect_equal(params$CI_low, c(-8.12252, 1.86862, 0.72411), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0, 0), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(-8.05815, 1.87355, 0.74108), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0, 0), tolerance = 1e-3)
})


# ivreg ---------------------------

test_that("model_parameters.ivreg", {
  skip_if_not_installed("ivreg")
  data(CigaretteDemand, package = "ivreg")
  model <- ivreg::ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
    data = CigaretteDemand
  )
  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(45L, 45L, 45L), tolerance = 1e-3)
  expect_equal(params$CI_low, c(6.69477, -1.86742, -0.32644), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.00266, 0.42867), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(6.76831, -1.84795, -0.3119), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.00147, 0.42447), tolerance = 1e-3)
})


# plm ---------------------------


test_that("model_parameters.plm", {
  skip_if_not_installed("plm")

  data("Produc", package = "plm")
  set.seed(123)

  model <- suppressWarnings(plm::plm(
    formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
    data = Produc,
    index = c("state", "year")
  ))

  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(764L, 764L, 764L, 764L), tolerance = 1e-3)
  expect_equal(params$CI_low, c(-0.08308, 0.2427, 0.70909, -0.00724), tolerance = 1e-3)
  expect_equal(params$p, c(0.36752, 0, 0, 0), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(-0.08299, 0.24277, 0.70918, -0.00724), tolerance = 1e-3)
  expect_equal(params$p, c(0.36724, 0, 0, 0), tolerance = 1e-3)
})


# nlme ---------------------------

test_that("model_parameters.gls", {
  skip_if_not_installed("nlme")
  data(Ovary, package = "nlme")
  model <- nlme::gls(
    follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
    data = Ovary,
    correlation = nlme::corAR1(form = ~ 1 | Mare)
  )
  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(305L, 305L, 305L), tolerance = 1e-3)
  expect_equal(params$CI_low, c(10.90853, -4.04402, -2.2722), tolerance = 1e-3)
  expect_equal(params$p, c(0, 2e-05, 0.19814), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(10.91372, -4.03898, -2.26675), tolerance = 1e-3)
  expect_equal(params$p, c(0, 2e-05, 0.19716), tolerance = 1e-3)
})


# # complmrob ---------------------------
#
# test_that("model_parameters.complmrob", {
#   skip_if_not_installed("complmrob")
#   crimes <- data.frame(
#     lifeExp = state.x77[, "Life Exp"],
#     USArrests[, c("Murder", "Assault", "Rape")]
#   )
#
#   # model
#   model <- complmrob::complmrob(formula = lifeExp ~ ., data = crimes)
#   params <- suppressWarnings(model_parameters(model))
#   expect_equal(params$df_error, c(46L, 46L, 46L, 46L), tolerance = 1e-3)
#   expect_equal(params$CI_low, c(69.79492, -3.09088, -2.91019, 2.05479), tolerance = 1e-3)
#   expect_equal(params$p, c(0, 0, 0.26437, 0), tolerance = 1e-3)
#
#   params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
#   expect_equal(params$df_error, c(Inf, Inf, Inf, Inf), tolerance = 1e-3)
#   expect_equal(params$CI_low, c(69.81747, -3.06832, -2.86118, 2.087), tolerance = 1e-3)
#   expect_equal(params$p, c(0, 0, 0.25851, 0), tolerance = 1e-3)
# })

# drc ---------------------------

test_that("model_parameters.drc", {
  skip_if_not_installed("drc")
  set.seed(123)
  data("selenium", package = "drc")
  model <- drc::drm(
    formula = dead / total ~ conc,
    curveid = type,
    weights = total,
    data = selenium,
    fct = drc::LL.2(),
    type = "binomial"
  )
  params <- suppressWarnings(model_parameters(model))
  expect_equal(params$df_error, c(17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L), tolerance = 1e-3)
  expect_equal(params$CI_low, c(
    -1.83156, -1.13673, -2.4552, -1.80875, 223.0835, 295.39556,
    107.25398, 70.62683
  ), tolerance = 1e-3)
  expect_equal(params$p, c(0, 1e-05, 0, 0, 0, 0, 0, 0), tolerance = 1e-3)

  params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
  expect_equal(params$df_error, c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), tolerance = 1e-3)
  expect_equal(params$CI_low, c(
    -1.80826, -1.11588, -2.43449, -1.78349, 225.15547, 301.29532,
    108.13891, 71.91797
  ), tolerance = 1e-3)
  expect_equal(params$p, c(0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-3)
})

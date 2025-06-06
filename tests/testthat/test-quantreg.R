skip_on_cran()
skip_if(getRversion() < "4.2.0")

# rqss ---------

# data("CobarOre")
# set.seed(123)
# CobarOre$w <- rnorm(nrow(CobarOre))
# m1 <- rqss(z ~ w + qss(cbind(x, y), lambda = .08), data = CobarOre)

# mp <- suppressWarnings(model_parameters(m1))
# test_that("mp_rqss", {
#   expect_identical(mp$Parameter, c("(Intercept)", "w", "cbind(x, y)"))
#   expect_equal(mp$Coefficient, c(17.63057, 1.12506, NA), tolerance = 1e-3)
#   expect_equal(mp$df_error, c(15, 15, NA), tolerance = 1e-3)
#   expect_equal(mp[["df"]], c(NA, NA, 70), tolerance = 1e-3)
# })


# rq ---------
test_that("mp_rq", {
  skip_if_not_installed("quantreg")
  data(stackloss)
  m1 <- quantreg::rq(stack.loss ~ Air.Flow + Water.Temp, data = stackloss, tau = 0.25)

  mp <- suppressWarnings(model_parameters(m1))
  expect_identical(mp$Parameter, c("(Intercept)", "Air.Flow", "Water.Temp"))
  expect_equal(mp$Coefficient, c(-36, 0.5, 1), tolerance = 1e-3)
})


# rqs ---------
test_that("mp_rqs", {
  skip_if_not_installed("quantreg")
  set.seed(123)
  data("engel", package = "quantreg")
  m1 <- quantreg::rq(foodexp ~ income, data = engel, tau = 1:9 / 10)

  mp <- suppressWarnings(model_parameters(m1))
  expect_identical(mp$Parameter, c(
    "(Intercept)", "income", "(Intercept)", "income", "(Intercept)",
    "income", "(Intercept)", "income", "(Intercept)", "income", "(Intercept)",
    "income", "(Intercept)", "income", "(Intercept)", "income", "(Intercept)",
    "income"
  ))
  expect_equal(mp$Coefficient, c(
    110.14157, 0.40177, 102.31388, 0.4469, 99.11058, 0.48124, 101.95988,
    0.5099, 81.48225, 0.56018, 79.70227, 0.58585, 79.28362, 0.60885,
    58.00666, 0.65951, 67.35087, 0.6863
  ), tolerance = 1e-3)
  expect_equal(mp$SE, c(
    29.39768, 0.04024, 21.42836, 0.02997, 22.18115, 0.02987, 22.06032,
    0.02936, 19.25066, 0.02828, 17.61762, 0.02506, 14.25039, 0.02176,
    19.21719, 0.02635, 22.39538, 0.02849
  ), tolerance = 1e-3)
})

# crq ---------
test_that("mp_rq", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("survival")
  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- 5 + x + rnorm(n)
  c <- 4 + x + rnorm(n)
  d <- (y > c)

  dat <- data.frame(y, x, c, d)
  m1 <- quantreg::crq(survival::Surv(pmax(y, c), d, type = "left") ~ x, method = "Portnoy", data = dat)

  mp <- model_parameters(m1)
  expect_identical(
    mp$Parameter,
    c("(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x")
  )
  expect_equal(
    mp$Coefficient,
    c(4.26724, 0.97534, 4.84961, 0.92638, 5.21843, 0.98038, 5.91301, 0.97382),
    tolerance = 1e-3
  )
})

skip_if_not_installed("nlme")
skip_if_not_installed("lme4")

data("sleepstudy", package = "lme4")
m1_lme <- nlme::lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)

data("Orthodont", package = "nlme")
m2_lme <- nlme::lme(distance ~ age + Sex, random = ~ 1 | Subject, data = Orthodont, method = "ML")

data(iris)
set.seed(1234)
iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))
m3_lme <- nlme::lme(
  fixed = Sepal.Length ~ Species * Sepal.Width + Petal.Length,
  random = ~ 1 | grp,
  data = iris
)

test_that("ci", {
  expect_equal(
    ci(m1_lme)$CI_low,
    c(237.927995380985, 7.4146616764556),
    tolerance = 1e-4
  )
})

test_that("ci(vcov)", {
  # vcov changes results
  ci1 <- ci(m3_lme)
  ci2 <- suppressMessages(ci(m3_lme, vcov = "CR3"))
  expect_true(all(ci1$CI_low != ci2$CI_low))
  # manual computation
  b <- lme4::fixef(m3_lme)
  se <- standard_error(m3_lme, vcov = "CR3")$SE
  tstat <- b / se
  critical_t <- abs(qt(0.025, df = dof(m3_lme)))
  ci_lo <- b - critical_t * se
  ci_hi <- b + critical_t * se
  expect_equal(ci2$CI_low, ci_lo, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(ci2$CI_high, ci_hi, tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("se", {
  expect_equal(
    standard_error(m1_lme)$SE,
    c(6.82451602451407, 1.54578275017725),
    tolerance = 1e-4
  )
})

test_that("se: vcov", {
  skip_if_not_installed("clubSandwich")
  se1 <- standard_error(m1_lme, vcov = "CR3")$SE
  se2 <- sqrt(diag(as.matrix(clubSandwich::vcovCR(m1_lme, type = "CR3"))))
  expect_equal(se1, se2, ignore_attr = TRUE)
})

test_that("p_value", {
  expect_equal(
    p_value(m1_lme)$p,
    c(2.38350215912719e-80, 2.26328050057813e-10),
    tolerance = 1e-4
  )
})

test_that("p: vcov", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("lmtest")
  # default
  p1 <- stats::coef(summary(m3_lme))[, 5]
  p2 <- p_value(m3_lme)$p
  expect_equal(p1, p2, ignore_attr = TRUE)
  # manual computation
  p1 <- p_value(m3_lme, vcov = "CR3")$p
  b2 <- lme4::fixef(m3_lme)
  se2 <- sqrt(diag(as.matrix(clubSandwich::vcovCR(m3_lme, type = "CR3"))))
  t2 <- b2 / se2
  # same DF used in `nlme:::summary.lme`
  p2 <- 2 * pt(-abs(t2), df = m3_lme$fixDF[["X"]])
  expect_equal(p1, p2, ignore_attr = TRUE)
})

test_that("model_parameters", {
  expect_equal(
    model_parameters(m1_lme, effects = "fixed")$Coefficient,
    c(251.405104848485, 10.467285959596),
    tolerance = 1e-4
  )
})

test_that("model_parameters", {
  params <- model_parameters(m2_lme, effects = "fixed")
  expect_equal(params$Coefficient, c(17.70671, 0.66019, -2.32102), tolerance = 1e-4)
  expect_equal(params$SE, c(0.83155, 0.06209, 0.74307), tolerance = 1e-4)
  # expect_equal(params$df, c(80, 80, 25), tolerance = 1e-4)
  expect_equal(params$CI_low, c(16.07503, 0.53834, -3.82999), tolerance = 1e-4)
})

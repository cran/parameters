skip_on_cran()

skip_if_not_installed("lmerTest")
skip_if_not_installed("pbkrtest")

data("carrots", package = "lmerTest")
m1 <- lmerTest::lmer(Preference ~ sens2 + Homesize + (1 + sens2 | Consumer), data = carrots)

test_that("model_parameters, satterthwaite", {
  params <- model_parameters(m1, effects = "fixed", ci_method = "satterthwaite")
  s <- summary(m1)
  expect_equal(params$df, as.vector(s$coefficients[, "df"]), tolerance = 1e-4)
  expect_equal(params$t, as.vector(s$coefficients[, "t value"]), tolerance = 1e-4)
  expect_equal(params$p, as.vector(s$coefficients[, "Pr(>|t|)"]), tolerance = 1e-4)
})

test_that("model_parameters, kenward", {
  params <- model_parameters(m1, effects = "fixed", ci_method = "kenward")
  s <- summary(m1, ddf = "Kenward-Roger")
  expect_equal(params$df, as.vector(s$coefficients[, "df"]), tolerance = 1e-4)
  expect_equal(params$t, as.vector(s$coefficients[, "t value"]), tolerance = 1e-4)
  expect_equal(params$p, as.vector(s$coefficients[, "Pr(>|t|)"]), tolerance = 1e-4)
})

skip_if_not_installed("blme")
skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
set.seed(123)
model <- blme::blmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, cov.prior = NULL)

test_that("model_parameters.blmerMod", {
  params <- model_parameters(model, effects = "fixed")
  expect_equal(params$SE, c(6.8246, 1.54579), tolerance = 1e-3)
  expect_named(
    params,
    c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects")
  )
})

test_that("model_parameters.blmerMod-all", {
  skip_if_not_installed("merDeriv")
  params <- model_parameters(model, effects = "all")
  expect_equal(params$SE, c(6.8246, 1.54579, 5.83626, 1.24804, 0.31859, 1.50801), tolerance = 1e-3)
  expect_equal(params$Coefficient, c(251.4051, 10.46729, 24.74066, 5.92214, 0.06555, 25.5918), tolerance = 1e-3)
  expect_named(
    params,
    c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects", "Group")
  )
  expect_identical(
    params$Parameter,
    c("(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "Cor (Intercept~Days)", "SD (Observations)")
  )
})

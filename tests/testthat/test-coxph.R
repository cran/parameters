skip_if_not_installed("survival")

lung <- subset(survival::lung, subset = ph.ecog %in% 0:2)
lung$sex <- factor(lung$sex, labels = c("male", "female"))
lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

m1 <- survival::coxph(survival::Surv(time, status) ~ sex + age + ph.ecog, data = lung)

test_that("ci", {
  expect_equal(
    ci(m1)$CI_low,
    c(-0.87535, -0.00747, 0.01862, 0.45527),
    tolerance = 1e-4
  )
})

test_that("se", {
  expect_equal(
    standard_error(m1)$SE,
    c(0.16823, 0.00931, 0.19961, 0.22809),
    tolerance = 1e-4
  )
})

test_that("p_value", {
  expect_equal(
    p_value(m1)$p,
    c(0.00118, 0.24713, 0.04005, 8e-05),
    tolerance = 1e-4
  )
})

test_that("model_parameters", {
  expect_equal(
    model_parameters(m1)$Coefficient,
    c(-0.54563, 0.01078, 0.40984, 0.90232),
    tolerance = 1e-4
  )
})

test_that("model_parameters", {
  suppressPackageStartupMessages(library(survival, quietly = TRUE))

  # Create the simplest test data set
  test1 <- list(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = c(0, 0, 0, 0, 1, 1, 1)
  )
  # Fit a stratified model
  m2 <- coxph(Surv(time, status) ~ x + strata(sex), test1)
  expect_equal(model_parameters(m2)$Coefficient, 0.8023179, tolerance = 1e-4)
  expect_equal(model_parameters(m2)$z, 0.9756088, tolerance = 1e-4)
  expect_equal(model_parameters(m2)$p, 0.3292583, tolerance = 1e-4)

  unloadNamespace("rms")
  unloadNamespace("quantreg")
  unloadNamespace("multcomp")
  unloadNamespace("TH.data")
  unloadNamespace("effects")
  unloadNamespace("survey")
  unloadNamespace("survival")
})


skip_if_not_installed("withr")

withr::with_package(
  "survival",
  test_that("model_parameters coxph-panel", {
    set.seed(123)
    # a time transform model
    mod <- survival::coxph(
      survival::Surv(time, status) ~ ph.ecog + tt(age),
      data = lung,
      tt = function(x, t, ...) pspline(x + t / 365.25)
    )
    expect_snapshot(print(model_parameters(mod)))
  })
)

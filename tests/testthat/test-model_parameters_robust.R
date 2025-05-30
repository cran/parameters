skip_if_not_installed("sandwich")
skip_if_not_installed("clubSandwich")

data(mtcars)
mtcars$am <- as.factor(mtcars$am)
model <- lm(mpg ~ wt * am + cyl + gear, data = mtcars)

test_that("model_parameters, robust CL", {
  params1 <- model_parameters(
    model,
    vcov = "CL",
    vcov_args = list(type = "HC1"),
    verbose = FALSE
  )
  robust_se <- unname(sqrt(diag(sandwich::vcovCL(model))))
  expect_equal(params1$SE, robust_se, tolerance = 1e-3)
  expect_equal(params1$p, c(0, 0.00695, 0.00322, 0.00435, 0.94471, 0.00176), tolerance = 1e-3)
})

test_that("model_parameters, robust", {
  params <- model_parameters(model, vcov = "HC", verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.0259, 0.01478, 0.01197, 0.95238, 0.01165), tolerance = 1e-3)
})

test_that("ci, robust", {
  params <- ci(model, vcov = "HC", verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model))))
  upper_ci <- as.vector(coef(model) + qt(0.975, df.residual(model)) * robust_se)
  expect_equal(params$CI_high, upper_ci, tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("model_parameters, robust CL", {
  params <- model_parameters(model, vcov = "vcovCL", verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovCL(model))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.00695, 0.00322, 0.00435, 0.94471, 0.00176), tolerance = 1e-3)
})

model2 <- lm(mpg ~ wt * am + cyl + gear, data = datawizard::standardize(mtcars))

test_that("model_parameters, robust", {
  params <- model_parameters(model, standardize = "refit", vcov = "HC", verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model2))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0.28624, 0.0259, 0.43611, 0.01197, 0.95238, 0.01165), tolerance = 1e-3)
})


# cluster-robust standard errors, using clubSandwich
data(iris)
model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

test_that("model_parameters, robust CR", {
  params <- model_parameters(
    model,
    vcov = "CR1",
    vcov_args = list(cluster = iris$cluster),
    verbose = FALSE
  )
  robust_se <- unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$cluster))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0.01246, 0.04172, 0.18895, 0.57496, 0, 0), tolerance = 1e-3)
})

test_that("model_parameters, normal", {
  params <- model_parameters(model)
  expect_equal(params$p, c(0.13267, 0.21557, 0.36757, 0.77012, 3e-05, 0), tolerance = 1e-3)
})

data(mtcars)
mtcars$am <- as.factor(mtcars$am)
model <- lm(mpg ~ wt * am + cyl + gear, data = mtcars)

test_that("model_parameters, robust", {
  params <- model_parameters(model, vcov = "HC3")
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.0259, 0.01478, 0.01197, 0.95238, 0.01165), tolerance = 1e-3)
})

test_that("ci, robust", {
  params <- ci(model, vcov = "HC3")
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model))))
  upper_ci <- as.vector(coef(model) + qt(0.975, df.residual(model)) * robust_se)
  expect_equal(params$CI_high, upper_ci, tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("model_parameters, robust CL", {
  params <- model_parameters(model, vcov = "vcovCL", vcov_args = list(type = "HC1"))
  robust_se <- unname(sqrt(diag(sandwich::vcovCL(model))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.00695, 0.00322, 0.00435, 0.94471, 0.00176), tolerance = 1e-3)
})

d <- datawizard::standardize(mtcars)
model2 <- lm(mpg ~ wt * am + cyl + gear, data = d)

test_that("model_parameters, robust", {
  params <- model_parameters(model, standardize = "refit", vcov = "HC3")
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model2))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0.28624, 0.0259, 0.43611, 0.01197, 0.95238, 0.01165), tolerance = 1e-3)
})


# cluster-robust standard errors, using clubSandwich
data(iris)
model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

test_that("model_parameters, robust CR", {
  params <- model_parameters(model, vcov = "vcovCR", vcov_args = list(type = "CR1", cluster = iris$cluster))
  robust_se <- unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$cluster))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0.01246, 0.04172, 0.18895, 0.57496, 0, 0), tolerance = 1e-3)
})

test_that("model_parameters, normal", {
  params <- model_parameters(model)
  expect_equal(params$p, c(0.13267, 0.21557, 0.36757, 0.77012, 3e-05, 0), tolerance = 1e-3)
})

test_that("ci_ml1, robust", {
  skip("TODO: this one actually is not correct.")
  skip_if_not(packageVersion("parameters") < "0.16.9.9")
  skip_if_not_installed("lme4")
  model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  params <- ci_ml1(model, vcov = "CR", vcov_args = list(cluster = iris$Species))
  robust_se <- unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$Species))))
  upper_ci <- fixef(model) + qt(0.975, dof_ml1(model)) * robust_se
})

skip_if_not_installed("VGAM")

skip_on_cran()

data("pneumo", package = "VGAM")
data("hunua", package = "VGAM")

set.seed(123)
pneumo <- transform(pneumo, let = log(exposure.time))

m1 <- suppressWarnings(VGAM::vgam(
  cbind(normal, mild, severe) ~ VGAM::s(let) + exposure.time,
  VGAM::cumulative(parallel = TRUE),
  data = pneumo,
  trace = FALSE
))

set.seed(123)
hunua$x <- rnorm(nrow(hunua))
m2 <- VGAM::vgam(
  agaaus ~ VGAM::s(altitude, df = 2) + VGAM::s(x) + beitaw + corlae, VGAM::binomialff,
  data = hunua
)

test_that("model_parameters.vgam", {
  skip("TODO: model_parameters doesn't work with 'VGAM::' in the formula")
  params <- suppressWarnings(model_parameters(m1))
  expect_equal(params$Coefficient, as.vector(m1@coefficients[params$Parameter]), tolerance = 1e-3)
  expect_identical(params$Parameter, c("(Intercept):1", "(Intercept):2", "exposure.time", "s(let)"))
  expect_equal(params$df, c(NA, NA, NA, 2.6501), tolerance = 1e-3)
  expect_equal(as.vector(na.omit(params$df)), as.vector(m1@nl.df), tolerance = 1e-3)
})

test_that("model_parameters.vgam", {
  skip("TODO: model_parameters doesn't work with 'VGAM::' in the formula")
  params <- suppressWarnings(model_parameters(m2))
  expect_equal(params$Coefficient, as.vector(m2@coefficients[params$Parameter]), tolerance = 1e-3)
  expect_identical(params$Parameter, c("(Intercept)", "beitaw", "corlae", "s(altitude, df = 2)", "s(x)"))
  expect_equal(params$df, c(NA, NA, NA, 0.82686, 2.8054), tolerance = 1e-3)
  expect_equal(as.vector(na.omit(params$df)), as.vector(m2@nl.df), tolerance = 1e-3)
  expect_named(params, c(
    "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "Chi2",
    "df_error", "p", "Component"
  ))
})

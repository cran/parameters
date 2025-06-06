data(iris)
model <- lm(Sepal.Length ~ Species, data = iris)

test_that("p_function ci-levels", {
  out <- p_function(model)
  expect_equal(
    out$CI_low,
    c(
      4.982759, 0.897132, 1.549132, 4.956774, 0.860384, 1.512384,
      4.92192, 0.811093, 1.463093, 4.862126, 0.726531, 1.378531
    ),
    tolerance = 1e-4
  )

  expect_identical(dim(out), c(12L, 5L))

  expect_equal(
    out$CI,
    c(0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.95, 0.95, 0.95),
    tolerance = 1e-4
  )

  ref <- ci(model)
  expect_equal(
    out$CI_low[out$CI == 0.95],
    ref$CI_low,
    tolerance = 1e-4
  )

  ref <- ci(model, ci = 0.5)
  expect_equal(
    out$CI_low[out$CI == 0.5],
    ref$CI_low,
    tolerance = 1e-4
  )

  out <- p_function(model, ci_levels = c(0.3, 0.6, 0.9))
  expect_equal(
    out$CI,
    c(0.3, 0.3, 0.3, 0.6, 0.6, 0.6, 0.9, 0.9, 0.9),
    tolerance = 1e-4
  )

  skip_if_not_installed("sandwich")
  out <- p_function(model, vcov = "HC3")
  expect_equal(
    out$CI_low,
    c(
      4.989925, 0.901495, 1.548843, 4.971951, 0.869624, 1.511772,
      4.947844, 0.826875, 1.462047, 4.906485, 0.753538, 1.376742
    ),
    tolerance = 1e-4
  )
})


test_that("p_function keep-drop", {
  out <- p_function(model, keep = "Speciesversicolor")

  expect_identical(dim(out), c(4L, 5L))

  expect_equal(
    out$CI,
    c(0.25, 0.5, 0.75, 0.95),
    tolerance = 1e-4
  )

  expect_identical(
    out$Parameter,
    c(
      "Speciesversicolor", "Speciesversicolor", "Speciesversicolor",
      "Speciesversicolor"
    )
  )
})


test_that("p_function print", {
  out <- p_function(model)
  ref <- capture.output(print(out))

  expect_identical(
    ref,
    c(
      "Consonance Function",
      "",
      "Parameter            |       25% CI |       50% CI |       75% CI |       95% CI",
      "--------------------------------------------------------------------------------",
      "(Intercept)          | [4.98, 5.03] | [4.96, 5.06] | [4.92, 5.09] | [4.86, 5.15]",
      "Species [versicolor] | [0.90, 0.96] | [0.86, 1.00] | [0.81, 1.05] | [0.73, 1.13]",
      "Species [virginica]  | [1.55, 1.61] | [1.51, 1.65] | [1.46, 1.70] | [1.38, 1.79]"
    )
  )
})

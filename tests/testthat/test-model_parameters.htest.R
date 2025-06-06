skip_if_not_installed("effectsize")

## TODO: add more tests for different htest objects and effectsize types

test_that("model_parameters.htest", {
  params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
  expect_named(
    params,
    c(
      "Parameter1", "Parameter2", "r", "CI", "CI_low", "CI_high",
      "t", "df_error", "p", "Method", "Alternative"
    )
  )
  expect_equal(params$r, -0.852, tolerance = 0.05)

  expect_warning({
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "spearman"))
  })
  expect_equal(params$rho, -0.9108, tolerance = 0.05)

  expect_warning({
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "kendall"))
  })
  expect_equal(params$tau, -0.795, tolerance = 0.05)

  params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length))
  expect_equal(params$Difference, -2.786, tolerance = 0.05)

  params <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs))
  expect_equal(params$Difference, -7.940, tolerance = 0.05)

  params <- model_parameters(t.test(iris$Sepal.Width, mu = 1))
  expect_equal(params$Difference, 2.0573, tolerance = 0.05)
})

test_that("model_parameters.htest-2", {
  x <- c(A = 20, B = 15, C = 25)
  mp <- model_parameters(chisq.test(x))
  expect_named(mp, c("Chi2", "df", "p", "Method"))
})


test_that("model_parameters-chisq-test NULL", {
  mp <- model_parameters(stats::chisq.test(table(mtcars$am)))
  expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
  expect_named(mp, c("Chi2", "df", "p", "Method"))
})


test_that("model_parameters-chisq-test two way table", {
  mp2 <- suppressWarnings(model_parameters(stats::chisq.test(table(mtcars$am, mtcars$cyl))))
  expect_equal(mp2$Chi2, 8.740733, tolerance = 1e-3)
  expect_named(mp2, c("Chi2", "df", "p", "Method"))
})

test_that("model_parameters-chisq-test works with `svychisq` objects", {
  skip_if_not_installed("survey")
  data(api, package = "survey")

  set.seed(123)
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  m <- survey::svychisq(~ sch.wide + stype, dclus1)
  mp <- model_parameters(m)

  expect_equal(mp$F, 5.19337, tolerance = 1e-3)
  expect_named(mp, c("F", "df", "df_error", "p", "Method"))
})

test_that("model_parameters-chisq-test adjusted", {
  expect_message({
    mp <- model_parameters(stats::chisq.test(table(mtcars$am)), es_type = "phi", ci = 0.95)
  })
  expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
  expect_named(mp, c("Chi2", "df", "p", "Method"))
})

test_that("model_parameters-t-test standardized d", {
  params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length), es_type = "cohens_d")
  expect_equal(params$Cohens_d, -4.210417, tolerance = 0.05)
  expect_equal(params$d_CI_low, -4.655306, tolerance = 0.05)
  expect_named(
    params,
    c(
      "Parameter1", "Parameter2", "Mean_Parameter1", "Mean_Parameter2",
      "Difference", "CI", "CI_low", "CI_high", "Cohens_d", "d_CI_low",
      "d_CI_high", "t", "df_error", "p", "Method", "Alternative"
    )
  )
})

test_that("model_parameters-t-test standardized d", {
  mp <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs), es_type = "cohens_d", verbose = FALSE)
  expect_equal(mp$Cohens_d, -1.696032, tolerance = 1e-3)
  expect_named(
    mp,
    c(
      "Parameter", "Group", "Mean_Group1", "Mean_Group2", "Difference", "CI",
      "CI_low", "CI_high", "Cohens_d", "d_CI_low", "d_CI_high", "t", "df_error",
      "p", "Method", "Alternative"
    )
  )
})

test_that("model_parameters-t-test reports the same unregarding of interface", {
  g1 <- 1:10
  g2 <- 7:20
  df <- data.frame(y = c(g1, g2), x = rep(c(0, 1), c(length(g1), length(g2))))
  compare_only <- c("Difference", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Method")
  default_ttest <- model_parameters(t.test(x = g1, y = g2))[compare_only]
  formula_ttest <- model_parameters(t.test(y ~ x, df))[compare_only]
  expect_equal(default_ttest, formula_ttest, ignore_attr = TRUE)
})

test_that("model_parameters-Box.test works, and ignores partial matching", {
  set.seed(123)
  ts1 <- ts(rnorm(200, mean = 10, sd = 3))
  result1 <- Box.test(ts1, lag = 5, type = "Box-Pierce", fitdf = 2)
  result2 <- Box.test(ts1, lag = 5, type = "Ljung-Box", fitdf = 2)

  out1 <- model_parameters(result1)
  out2 <- model_parameters(result1, effects = "all")
  expect_equal(out1, out2, ignore_attr = TRUE)
  expect_named(out1, c("Parameter", "Chi2", "df_error", "p", "Method"))

  out1 <- model_parameters(result2)
  out2 <- model_parameters(result2, effects = "all")
  expect_equal(out1, out2, ignore_attr = TRUE)
})

test_that("model_parameters-htests removes $ from parameter and group names", {
  data(sleep)
  sleep2 <- reshape(sleep, direction = "wide", idvar = "ID", timevar = "group")
  out <- format(model_parameters(t.test(sleep2$extra.1, sleep2$extra.2, paired = TRUE)))
  expect_identical(out$Parameter, "extra.1")
  expect_identical(out$Group, "extra.2")
})

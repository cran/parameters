skip_on_os("linux")

test_that("model_parameters.BFBayesFactor", {
  skip_on_cran()
  skip("TODO")
  skip_if_not_installed("BayesFactor")
  model <- BayesFactor::ttestBF(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)
  expect_equal(model_parameters(model)$BF, c(492.770567186302, NA), tolerance = 1e-2)
})


# make sure BF is returned, even if NA
# see https://github.com/easystats/correlation/issues/269
test_that("model_parameters.BFBayesFactor", {
  skip_if_not_installed("BayesFactor")
  var_x <- c(
    12.1, 8.7, 10.1, 17.4, 12.5, 2.7, 6.2, 19.4, 11, 14.5, 15.8,
    10.4, 13.5, 3.5, 5.6, 5.2, 6.3, 12.5, 9.8
  )
  var_y <- c(
    11.9, 15.3, 13.9, 6.6, 11.5, 21.35, 17.8, 4.6, 13, 9.5, 8.2,
    13.6, 10.5, 20.5, 18.45, 18.8, 17.7, 11.5, 14.2
  )
  expect_warning({
    model <- BayesFactor::correlationBF(var_x, var_y, rscale = "medium")
  })
  params <- model_parameters(model)
  expect_identical(
    colnames(params),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "Prior_Distribution",
      "Prior_Location", "Prior_Scale", "BF", "Method"
    )
  )
  expect_true(is.na(params$BF))
})

test_that("model_parameters.BFBayesFactor", {
  skip_if_not_installed("BayesFactor")
  model <- BayesFactor::correlationBF(iris$Sepal.Width, iris$Petal.Length)
  expect_equal(model_parameters(model)$BF, 348853.6, tolerance = 10)
})

test_that("model_parameters.BFBayesFactor", {
  skip_if_not_installed("BayesFactor")
  set.seed(123)
  model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris, progress = FALSE)
  expect_equal(
    model_parameters(model, centrality = "median")$Median,
    c(5.8431, -0.8266, 0.092, 0.734, 0.2681, 2.0415),
    tolerance = 2
  )
})

#   test_that("model_parameters.BFBayesFactor", {
#   skip_on_cran()

#     model <- BayesFactor::ttestBF(formula = mpg ~ am, data = df)
#     expect_equal(model_parameters(model)$BF, c(86.58973, NA), tolerance = 1)
#   })

test_that("model_parameters.BFBayesFactor", {
  skip_if_not_installed("BayesFactor")
  df <- mtcars
  df$gear <- as.factor(df$gear)
  df$am <- as.factor(df$am)
  set.seed(123)
  model <- suppressMessages(BayesFactor::anovaBF(mpg ~ gear * am, data = df, progress = FALSE))
  expect_equal(
    suppressMessages(model_parameters(model, centrality = "mean", verbose = FALSE))$Mean,
    c(20.7099, -3.24884, 3.24884, 26.51413, 5.30506, NA, NA, NA),
    tolerance = 1L
  )
})

test_that("model_parameters.BFBayesFactor", {
  skip_on_cran()
  skip_if_not_installed("BayesFactor")
  data(raceDolls, package = "BayesFactor")

  bf <- BayesFactor::contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols")

  mp <- suppressWarnings(model_parameters(bf,
    centrality = "mean",
    dispersion = TRUE,
    verbose = FALSE,
    es_type = "cramers_v",
    adjust = TRUE,
    include_proportions = TRUE
  ))
  mp2 <- suppressWarnings(model_parameters(bf, verbose = FALSE))

  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Mean", "CI", "CI_low", "CI_high", "SD", "Cramers_v_adjusted",
      "pd", "Prior_Distribution", "Prior_Location",
      "Prior_Scale", "BF", "Method"
    )
  )
  expect_identical(dim(mp), c(6L, 13L))

  expect_identical(
    colnames(mp2),
    c(
      "Parameter", "Prior_Distribution", "Prior_Location", "Prior_Scale",
      "BF", "Method", "CI"
    )
  )
  expect_identical(dim(mp2), c(1L, 7L))
})

test_that("model_parameters.BFBayesFactor", {
  skip_on_cran()
  skip_if_not_installed("BayesFactor")
  data(puzzles, package = "BayesFactor")
  result <- BayesFactor::anovaBF(RT ~ shape * color + ID,
    data = puzzles, whichRandom = "ID",
    whichModels = "top", progress = FALSE
  )
  mp <- suppressMessages(model_parameters(
    result,
    centrality = "median",
    dispersion = TRUE,
    verbose = FALSE
  ))
  expect_identical(colnames(mp), c(
    "Parameter", "Median", "MAD", "CI", "CI_low", "CI_high", "pd",
    "Prior_Distribution", "Prior_Location", "Prior_Scale", "Effects",
    "Component", "BF", "Method"
  ))
  expect_identical(mp$Effects, c(
    "fixed", "fixed", "fixed", "fixed", "fixed", "random", "random",
    "random", "random", "random", "random", "random", "random", "random",
    "random", "random", "random", "fixed", "fixed", "fixed", "fixed"
  ))
})

# one-sample t-test

test_that("model_parameters.BFBayesFactor, without effectsize", {
  skip_if_not_installed("BayesFactor")
  set.seed(123)
  df_t <- as.data.frame(parameters(BayesFactor::ttestBF(mtcars$wt, mu = 3)))

  expect_identical(
    colnames(df_t),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd",
      "Prior_Distribution", "Prior_Location", "Prior_Scale", "BF",
      "Method"
    )
  )
  expect_identical(dim(df_t), c(1L, 11L))
})

test_that("model_parameters.BFBayesFactor, with effectsize", {
  skip_if_not_installed("BayesFactor")
  set.seed(123)
  df_t_es <- as.data.frame(
    parameters(BayesFactor::ttestBF(mtcars$wt, mu = 3), es_type = "cohens_d")
  )

  # TODO: fix column order
  expect_identical(
    colnames(df_t_es),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "Cohens_d",
      "d_CI_low", "d_CI_high", "pd", "Prior_Distribution", "Prior_Location",
      "Prior_Scale", "BF", "Method"
    )
  )
  expect_identical(dim(df_t_es), c(1L, 14L))
})

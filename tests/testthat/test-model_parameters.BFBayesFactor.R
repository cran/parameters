if (require("testthat") &&
  require("parameters") &&
  suppressPackageStartupMessages(require("BayesFactor", quietly = TRUE)) &&
  require("logspline") &&
  getRversion() >= "3.6") {
  .runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

  if (.runThisTest) {
    test_that("model_parameters.BFBayesFactor", {
      model <- BayesFactor::ttestBF(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)
      expect_equal(parameters::model_parameters(model)$BF, c(492.77057, NA), tolerance = 1e-2)
    })
  }

  test_that("model_parameters.BFBayesFactor", {
    model <- BayesFactor::correlationBF(iris$Sepal.Width, iris$Petal.Length)
    expect_equal(parameters::model_parameters(model)$BF, 348853.6, tolerance = 10)
  })

  test_that("model_parameters.BFBayesFactor", {
    set.seed(123)
    model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)
    expect_equal(parameters::model_parameters(model, centrality = "median")$Median, c(5.8431, -0.8266, 0.092, 0.734, 0.2681, 2.0415), tolerance = 2)
  })

  df <- mtcars
  df$gear <- as.factor(df$gear)
  df$am <- as.factor(df$am)

  if (.runThisTest) {
    test_that("model_parameters.BFBayesFactor", {
      model <- BayesFactor::ttestBF(formula = mpg ~ am, data = df)
      expect_equal(model_parameters(model)$BF, c(86.58973, NA), tolerance = 1)
    })
  }

  test_that("model_parameters.BFBayesFactor", {
    set.seed(123)
    model <- BayesFactor::anovaBF(mpg ~ gear * am, data = df)
    expect_equal(model_parameters(model, centrality = "mean")$Mean, c(20.7099, -3.24884, 3.24884, 26.51413, 5.30506, NA, NA, NA), tolerance = 1L)
  })

  if (.runThisTest) {
    data(raceDolls)
    bf <- contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols")
    mp <- suppressWarnings(model_parameters(bf, centrality = "mean", dispersion = TRUE, verbose = FALSE))

    test_that("model_parameters.BFBayesFactor", {
      expect_equal(colnames(mp), c(
        "Parameter", "Mean", "CI", "CI_low", "CI_high", "pd", "ROPE_Percentage",
        "SD", "Prior_Distribution", "Prior_Location", "Prior_Scale", "BF", "Method"
      ))
    })

    data(puzzles)
    result <- anovaBF(RT ~ shape * color + ID,
      data = puzzles, whichRandom = "ID",
      whichModels = "top", progress = FALSE
    )
    mp <- model_parameters(result, centrality = "median", dispersion = TRUE, verbose = FALSE)

    test_that("model_parameters.BFBayesFactor", {
      expect_equal(colnames(mp), c(
        "Parameter", "Median", "MAD", "CI", "CI_low", "CI_high", "pd", "ROPE_Percentage",
        "Prior_Distribution", "Prior_Location", "Prior_Scale", "Effects",
        "Component", "BF", "Method"
      ))
      expect_equal(mp$Effects, c(
        "fixed", "fixed", "fixed", "fixed", "fixed", "random", "random",
        "random", "random", "random", "random", "random", "random", "random",
        "random", "random", "random", "fixed", "fixed", "fixed", "fixed"
      ))
    })
  }
}

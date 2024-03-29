skip_if_not_installed("mice")
skip_if_not_installed("nnet")
skip_if_not(packageVersion("insight") > "0.19.1")

test_that("param ordinal", {
  set.seed(1234)
  d <- suppressWarnings(mice::ampute(mtcars)) ## Ampute mtcars and impute two data sets
  imp <- suppressWarnings(mice::mice(d$amp, m = 2, printFlag = FALSE))
  imp.l <- mice::complete(imp, action = "long")
  model <- list() ## Fit and pool models
  for (i in 1:2) {
    capture.output({
      model[[i]] <- nnet::multinom(cyl ~ disp + hp, data = imp.l, subset = .imp == i)
    })
  }
  pooled <- mice::pool(model)

  mp <- model_parameters(pooled)
  expect_snapshot(print(mp))
})


test_that("param normal", {
  set.seed(1234)
  d <- suppressWarnings(mice::ampute(mtcars)) ## Ampute mtcars and impute two data sets
  imp <- suppressWarnings(mice::mice(d$amp, m = 2, printFlag = FALSE))
  imp.l <- mice::complete(imp, action = "long")
  model <- list() ## Fit and pool models
  for (i in 1:2) model[[i]] <- lm(mpg ~ disp + hp, data = imp.l, subset = .imp == i)
  pooled <- mice::pool(model)

  mp <- model_parameters(pooled)
  expect_snapshot(print(mp))
})

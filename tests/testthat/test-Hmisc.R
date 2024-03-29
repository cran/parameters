test_that("issue 697", {
  skip_if_not_installed("Hmisc")
  skip_if_not_installed("rms")

  # for some reason, Hmisc::transcan() doesn't find na.retain (which is an internal
  # function in Hmisc)
  na.retain <<- Hmisc:::na.retain

  set.seed(1)
  n <- 100
  df <- data.frame(
    y = round(runif(n), 2),
    x1 = sample(c(-1, 0, 1), n, TRUE),
    x2 = sample(c(-1, 0, 1), n, TRUE)
  )
  df$x1[c(0, 1, 2)] <- NA
  imputer <- suppressWarnings(Hmisc::transcan(
    ~ x1 + x2,
    data = df,
    imputed = TRUE,
    n.impute = 2,
    pr = FALSE,
    pl = FALSE
  ))

  suppressWarnings(
    mod <- Hmisc::fit.mult.impute(
      y ~ x1 + x2,
      fitter = rms::orm,
      xtrans = imputer,
      data = df,
      pr = FALSE
    )
  )

  expect_s3_class(parameters(mod), "parameters_model")
  expect_s3_class(standard_error(mod), "data.frame")
  expect_s3_class(p_value(mod), "data.frame")

  expect_identical(nrow(parameters(mod)), 3L)
  expect_identical(nrow(standard_error(mod)), 3L)
  expect_identical(nrow(p_value(mod)), 3L)
})

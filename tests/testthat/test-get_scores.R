skip_on_cran()

test_that("get_scores", {
  skip_if_not_installed("psych")
  data(mtcars)
  pca <- principal_components(mtcars[, 1:7], n = 2, rotation = "varimax")
  scores <- get_scores(pca)
  expect_equal(head(scores$Component_1), c(38.704, 38.755, 28.194, 58.339, 78.658, 51.064), tolerance = 1e-2)
  expect_equal(head(scores$Component_2), c(63.23, 63.51, 55.805, 64.72, 96.01, 62.61), tolerance = 1e-2)
})

if (require("testthat") && require("parameters")) {
  test_that("equivalence_test", {
    data(mtcars)
    m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
    x <- equivalence_test(m)
    expect_equal(c(nrow(x), ncol(x)), c(5, 8))

    expect_true(is.character(capture.output(equivalence_test(m))))
  })
}

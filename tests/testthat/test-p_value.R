test_that("p_value", {
  expect_equal(p_value(c(1, 1, 1)), p_value(-c(1, 1, 1)), tolerance = 1e-3)

  set.seed(123)
  x <- rnorm(100, mean = 1.5)
  expect_equal(p_value(x), p_value(-x), tolerance = 1e-3)
  expect_gt(p_value(x, null = 1), p_value(x))
  expect_gt(p_value(x), p_value(x, null = -1))
  expect_equal(p_value(x, null = -1), p_value(-x, null = 1), tolerance = 1e-3)
})



skip_on_cran()

test_that("p_value", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("lme4")

  # h-tests
  model <- insight::download_model("htest_1")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0.04136799, tolerance = 0.01)

  model <- insight::download_model("htest_2")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0.1518983, tolerance = 0.01)

  model <- insight::download_model("htest_3")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0.182921, tolerance = 0.01)

  model <- insight::download_model("htest_4")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0, tolerance = 0.01)

  model <- insight::download_model("htest_5")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0, tolerance = 0.01)

  model <- insight::download_model("htest_6")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0, tolerance = 0.01)

  model <- insight::download_model("htest_7")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0, tolerance = 0.01)

  model <- insight::download_model("htest_8")
  skip_if(is.null(model))
  expect_equal(p_value(model), 0, tolerance = 0.01)

  # ANOVAs
  model <- insight::download_model("aov_1")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p, 0, tolerance = 0.01)

  model <- insight::download_model("anova_1")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p, 0, tolerance = 0.01)

  model <- insight::download_model("aovlist_1")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p, 0, tolerance = 0.01)

  model <- insight::download_model("aov_2")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

  model <- insight::download_model("anova_2")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

  model <- insight::download_model("aovlist_2")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

  model <- insight::download_model("aov_3")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

  model <- insight::download_model("anova_3")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

  model <- insight::download_model("aovlist_3")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

  model <- insight::download_model("anova_4")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[2], 0, tolerance = 0.01)

  # ANOVA lmer
  model <- insight::download_model("anova_lmerMod_0")
  skip_if(is.null(model))
  expect_identical(p_value(model), NA)

  model <- insight::download_model("anova_lmerMod_1")
  skip_if(is.null(model))
  expect_identical(p_value(model), NA)

  model <- insight::download_model("anova_lmerMod_2")
  skip_if(is.null(model))
  expect_identical(p_value(model), NA)

  model <- insight::download_model("anova_lmerMod_3")
  skip_if(is.null(model))
  expect_identical(p_value(model), NA)

  model <- insight::download_model("anova_lmerMod_4")
  skip_if(is.null(model))
  expect_identical(p_value(model), NA)

  model <- insight::download_model("anova_lmerMod_5")
  skip_if(is.null(model))
  expect_identical(p_value(model), NA)

  model <- insight::download_model("anova_lmerMod_6")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[2], 0, tolerance = 0.01)


  # Mixed models
  model <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  expect_equal(p_value(model)$p[1], 0.206219, tolerance = 0.01)
  expect_equal(p_value(model, method = "normal")$p[1], 0.1956467, tolerance = 0.01)
  expect_equal(p_value(model, method = "kr")$p[1], 0.319398, tolerance = 0.01)

  model <- insight::download_model("merMod_1")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0.06578, tolerance = 0.01)

  model <- insight::download_model("merMod_2")
  skip_if(is.null(model))
  expect_equal(p_value(model)$p[1], 0.29912, tolerance = 0.01)
})

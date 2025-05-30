skip_if_not_installed("performance")
skip_if_not_installed("domir")
skip_if_not_installed("datawizard")

DA_test_model <- lm(mpg ~ vs + cyl + carb, data = mtcars)

DA_performance <- dominance_analysis(DA_test_model)

DA_domir <- domir::domin(mpg ~ vs + cyl + carb, lm, list(performance::r2, "R2"), data = mtcars)

test_that("dominance_analysis$general_dominance", {
  gnrl_domir <- c(NA, DA_domir$General_Dominance)
  names(gnrl_domir) <- NULL

  gnrl_da <- DA_performance$General$General_Dominance

  expect_identical(gnrl_domir, gnrl_da)
})

test_that("dominance_analysis$conditional_dominance", {
  cdl_domir <- DA_domir$Conditional_Dominance
  dimnames(cdl_domir) <- c(NULL, NULL)

  cdl_da <- as.matrix(DA_performance$Conditional[, -1])
  dimnames(cdl_da) <- c(NULL, NULL)

  expect_identical(cdl_domir, cdl_da)
})

test_that("dominance_analysis$complete_dominance", {
  cpt_domir <- DA_domir$Complete_Dominance
  dimnames(cpt_domir) <- list(NULL, NULL)

  cpt_da <- t(DA_performance$Complete[, -1])
  dimnames(cpt_da) <- list(NULL, NULL)

  expect_identical(cpt_domir, cpt_da)
})

DA_performance2 <- dominance_analysis(
  DA_test_model,
  all = ~vs,
  sets = c(~carb),
  complete = FALSE,
  conditional = FALSE
)

DA_domir2 <- domir::domin(
  mpg ~ cyl,
  lm,
  list(performance::r2, "R2"),
  all = "vs",
  sets = list("carb"),
  data = mtcars,
  complete = FALSE,
  conditional = FALSE
)

test_that("dominance_analysis$general_dominance with sets/all", {
  domir_all_sub_r2 <- DA_domir2$Fit_Statistic_All_Subsets
  names(domir_all_sub_r2) <- NULL

  expect_identical(
    domir_all_sub_r2,
    with(DA_performance2$General, General_Dominance[Subset == "all"])
  )

  gnrl_domir2 <- DA_domir2$General_Dominance
  names(gnrl_domir2) <- NULL

  gnrl_da2 <- aggregate(
    DA_performance2$General$General_Dominance,
    list(DA_performance2$General$Subset), mean
  )
  gnrl_da2 <- gnrl_da2[which(gnrl_da2$Group.1 %in% c("cyl", "set1")), ]
  gnrl_da2 <- gnrl_da2$x
  names(gnrl_da2) <- NULL

  expect_identical(gnrl_domir2, gnrl_da2)
})

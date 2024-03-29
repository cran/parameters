skip_on_cran()

skip_if_not(getRversion() >= "4.0.0")
skip_if_not_installed("glmmTMB")

data(Salamanders, package = "glmmTMB")
model <- suppressWarnings(glmmTMB::glmmTMB(
  count ~ spp + mined + spp * mined,
  ziformula = ~ spp + mined + spp * mined,
  family = glmmTMB::truncated_poisson,
  data = Salamanders
))

mp <- model_parameters(model, effects = "fixed", component = "conditional")
test_that("model_parameters", {
  expect_equal(mp$Coefficient, as.vector(glmmTMB::fixef(model)[[1]]), tolerance = 1e-3)
  expect_equal(mp$Parameter, names(glmmTMB::fixef(model)[[1]]))
})

mp <- model_parameters(model, effects = "fixed", component = "all")
test_that("model_parameters", {
  expect_equal(mp$Coefficient, as.vector(unlist(glmmTMB::fixef(model))), tolerance = 1e-3)
  expect_equal(mp$Parameter, gsub("^(cond\\.|zi\\.)", "", names(unlist(glmmTMB::fixef(model)))))
  expect_equal(
    mp$Component,
    c(
      "conditional", "conditional", "conditional", "conditional",
      "conditional", "conditional", "conditional", "conditional", "conditional",
      "conditional", "conditional", "conditional", "conditional", "conditional",
      "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated",
      "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated",
      "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated",
      "zero_inflated", "zero_inflated"
    )
  )
})


sim1 <- function(nfac = 40, nt = 100, facsd = 0.1, tsd = 0.15, mu = 0, residsd = 1) {
  dat <- expand.grid(fac = factor(letters[1:nfac]), t = 1:nt)
  n <- nrow(dat)
  dat$REfac <- rnorm(nfac, sd = facsd)[dat$fac]
  dat$REt <- rnorm(nt, sd = tsd)[dat$t]
  dat$x <- rnorm(n, mean = mu, sd = residsd) + dat$REfac + dat$REt
  dat
}

set.seed(101)
d1 <- sim1(mu = 100, residsd = 10)
d2 <- sim1(mu = 200, residsd = 5)
d1$sd <- "ten"
d2$sd <- "five"
dat <- rbind(d1, d2)

model <- suppressWarnings(glmmTMB::glmmTMB(x ~ sd + (1 | t), dispformula = ~sd, data = dat))
mp <- model_parameters(model, effects = "fixed")

test_that("model_parameters", {
  expect_equal(mp$Coefficient, as.vector(unlist(glmmTMB::fixef(model))), tolerance = 1e-3)
  expect_equal(mp$Component, c("conditional", "conditional", "dispersion", "dispersion"))
})

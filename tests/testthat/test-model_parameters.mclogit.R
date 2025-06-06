skip_if_not_installed("mclogit")
skip_if_not_installed("withr")
skip_if_not(packageVersion("insight") > "0.19.1")

skip_on_cran()

withr::with_options(
  list(parameters_exponentiate = FALSE),
  {
    data(Transport, package = "mclogit")
    invisible(capture.output({
      m1 <- mclogit::mclogit(
        cbind(resp, suburb) ~ distance + cost,
        data = Transport
      )
    }))

    data(housing, package = "MASS")
    invisible(capture.output({
      m2 <- mclogit::mblogit(Sat ~ Infl + Type + Cont,
        weights = Freq,
        data = housing
      )
    }))

    test_that("model_parameters.mclogit", {
      params <- model_parameters(m1)
      expect_snapshot(params)
    })
    test_that("model_parameters.mblogit", {
      params <- model_parameters(m2)
      expect_snapshot(params)
    })

    skip_on_os(c("mac", "linux"))
    test_that("simulate_parameters.mblogit", {
      set.seed(1234)
      params <- simulate_parameters(m2)
      expect_snapshot(params)
    })
  }
)

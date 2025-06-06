skip_if_not_installed("withr")
skip_if_not_installed("glmmTMB")
skip_if_not(getRversion() >= "4.0.0")

data("fish", package = "parameters")
data("Salamanders", package = "glmmTMB")

skip_on_cran()

withr::with_options(
  list(parameters_exponentiate = FALSE),
  {
    m1 <- suppressWarnings(glmmTMB::glmmTMB(
      count ~ child + camper + (1 | persons),
      ziformula = ~ child + camper + (1 | persons),
      data = fish,
      family = glmmTMB::truncated_poisson()
    ))

    m2 <- suppressWarnings(glmmTMB::glmmTMB(
      count ~ child + camper + (1 | persons),
      data = fish,
      family = poisson()
    ))

    m3 <- suppressWarnings(glmmTMB::glmmTMB(
      count ~ spp + mined + (1 | site),
      ziformula = ~ spp + mined,
      family = glmmTMB::nbinom2,
      data = Salamanders
    ))

    test_that("unsupported args", {
      expect_message(model_parameters(m1, vcov = "HC3", effects = "fixed", component = "conditional"))
      expect_message(model_parameters(m1, vcov = "HC3"))
    })

    test_that("ci", {
      expect_equal(
        ci(m1)$CI_low,
        c(0.33067, -1.32402, 0.55037, -1.66786, 1.44667, -1.64177),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m1, component = "cond")$CI_low,
        c(0.33067, -1.32402, 0.55037),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m1, component = "zi")$CI_low,
        c(-1.66786, 1.44667, -1.64177),
        tolerance = 1e-3
      )

      expect_equal(
        ci(m2)$CI_low,
        c(-0.47982, -1.85096, 0.76044),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m2, component = "cond")$CI_low,
        c(-0.47982, -1.85096, 0.76044),
        tolerance = 1e-3
      )

      expect_message(
        expect_null(ci(m2, component = "zi")),
        "no zero-inflation component"
      )
    })


    test_that("se", {
      expect_equal(
        standard_error(m1)$SE,
        c(0.47559, 0.09305, 0.09346, 0.65229, 0.3099, 0.32324),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, effects = "random")$persons$`(Intercept)`,
        c(0.69856, 0.68935, 0.68749, 0.68596),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, component = "cond")$SE,
        c(0.47559, 0.09305, 0.09346),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, component = "zi")$SE,
        c(0.65229, 0.3099, 0.32324),
        tolerance = 1e-3
      )

      expect_equal(
        standard_error(m2)$SE,
        c(0.62127, 0.08128, 0.08915),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m2, component = "cond")$SE,
        c(0.62127, 0.08128, 0.08915),
        tolerance = 1e-3
      )

      expect_message(
        expect_null(standard_error(m2, component = "zi")),
        "no zero-inflation component"
      )
    })


    test_that("p_value", {
      expect_equal(
        p_value(m1)$p,
        c(0.00792, 0, 0, 0.55054, 0, 0.00181),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m1, component = "cond")$p,
        c(0.00792, 0, 0),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m1, component = "zi")$p,
        c(0.55054, 0, 0.00181),
        tolerance = 1e-3
      )

      expect_equal(
        p_value(m2)$p,
        c(0.23497, 0, 0),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m2, component = "cond")$p,
        c(0.23497, 0, 0),
        tolerance = 1e-3
      )

      expect_message(
        expect_null(p_value(m2, component = "zi")),
        "no zero-inflation component"
      )
    })


    test_that("model_parameters", {
      expect_equal(
        model_parameters(m1, effects = "fixed")$Coefficient,
        c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823),
        tolerance = 1e-3
      )
      expect_equal(
        model_parameters(m1, effects = "all")$Coefficient,
        c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823, 0.9312, 1.17399),
        tolerance = 1e-3
      )
      expect_equal(
        model_parameters(m2, effects = "fixed")$Coefficient,
        c(0.73785, -1.69166, 0.93516),
        tolerance = 1e-3
      )
      expect_equal(
        model_parameters(m3, effects = "fixed")$Coefficient,
        c(
          -0.61038, -0.9637, 0.17068, -0.38706, 0.48795, 0.58949, -0.11327,
          1.42935, 0.91004, 1.16141, -0.93932, 1.04243, -0.56231, -0.893,
          -2.53981, -2.56303, 1.51165
        ),
        tolerance = 1e-2
      )
      expect_identical(
        model_parameters(m1)$Component,
        c(
          "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "zero_inflated"
        )
      )
      expect_null(model_parameters(m2, effects = "fixed")$Component)
      expect_identical(
        model_parameters(m2)$Component,
        c("conditional", "conditional", "conditional", "conditional")
      )
      expect_identical(
        model_parameters(m3, effects = "fixed")$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "conditional", "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated",
          "zero_inflated", "zero_inflated", "zero_inflated", "dispersion"
        )
      )
      expect_equal(
        model_parameters(m3, effects = "fixed")$SE,
        c(
          0.4052, 0.6436, 0.2353, 0.3424, 0.2383, 0.2278, 0.2439, 0.3666,
          0.6279, 1.3346, 0.8005, 0.714, 0.7263, 0.7535, 2.1817, 0.6045,
          NA
        ),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-random", {
      params <- model_parameters(m1, effects = "random", group_level = TRUE)
      expect_identical(c(nrow(params), ncol(params)), c(8L, 10L))
      expect_named(
        params,
        c(
          "Parameter", "Level", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "Component", "Effects", "Group"
        )
      )
      expect_identical(
        as.vector(params$Parameter),
        c(
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)",
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(-1.24, -0.3456, 0.3617, 1.2553, 1.5719, 0.3013, -0.3176, -1.5665),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-random grouplevel", {
      params <- model_parameters(m1, effects = "grouplevel")
      expect_identical(c(nrow(params), ncol(params)), c(8L, 10L))
      expect_named(
        params,
        c(
          "Parameter", "Level", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "Component", "Effects", "Group"
        )
      )
      expect_identical(
        as.vector(params$Parameter),
        c(
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)",
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(-1.24, -0.3456, 0.3617, 1.2553, 1.5719, 0.3013, -0.3176, -1.5665),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-random total", {
      params <- model_parameters(m1, effects = "total")
      expect_identical(c(nrow(params), ncol(params)), c(8L, 6L))
      expect_named(
        params,
        c(
          "Group", "Level", "Parameter", "Coefficient", "Component",
          "Effects"
        )
      )
      expect_identical(
        as.vector(params$Parameter),
        c(
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)",
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(0.02278, 0.91717, 1.62455, 2.51811, 1.18248, -0.08808, -0.70703, -1.95584),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-ran_pars", {
      params <- model_parameters(m1, effects = "random")
      expect_identical(c(nrow(params), ncol(params)), c(2L, 9L))
      expect_named(
        params,
        c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "Effects", "Group", "Component")
      )
      expect_identical(
        params$Parameter,
        c("SD (Intercept)", "SD (Intercept)")
      )
      expect_identical(
        params$Component,
        c("conditional", "zero_inflated")
      )
      expect_equal(
        params$Coefficient,
        c(0.9312, 1.17399),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-all_pars", {
      params <- model_parameters(m1, effects = "all")
      expect_identical(c(nrow(params), ncol(params)), c(8L, 12L))
      expect_named(
        params,
        c(
          "Parameter", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "z", "df_error", "p", "Effects", "Group", "Component"
        )
      )
      expect_identical(
        params$Parameter,
        c(
          "(Intercept)", "child", "camper1", "(Intercept)", "child",
          "camper1", "SD (Intercept)", "SD (Intercept)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823, 0.9312, 1.17399),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-all", {
      params <- model_parameters(m1, effects = "all", group_level = TRUE)
      expect_identical(c(nrow(params), ncol(params)), c(14L, 13L))
      expect_named(
        params,
        c(
          "Parameter", "Level", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "z", "df_error", "p", "Component", "Effects",
          "Group"
        )
      )
      expect_identical(
        params$Parameter,
        c(
          "(Intercept)", "child", "camper1", "(Intercept)", "child",
          "camper1", "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)",
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "conditional",
          "conditional", "conditional", "zero_inflated", "zero_inflated",
          "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(
          1.2628, -1.1417, 0.7335, -0.3894, 2.0541, -1.0082, -1.24, -0.3456,
          0.3617, 1.2553, 1.5719, 0.3013, -0.3176, -1.5665
        ),
        tolerance = 1e-2
      )
    })


    data(mtcars)
    mdisp <- glmmTMB::glmmTMB(hp ~ 0 + wt / mpg, mtcars)
    test_that("model_parameters, dispersion", {
      mp <- model_parameters(mdisp)
      expect_equal(mp$Coefficient, c(59.50992, -0.80396, 48.97731), tolerance = 1e-2)
      expect_identical(mp$Parameter, c("wt", "wt:mpg", "(Intercept)"))
      expect_identical(mp$Component, c("conditional", "conditional", "dispersion"))
    })

    mdisp <- glmmTMB::glmmTMB(hp ~ 0 + wt / mpg + (1 | gear), mtcars)
    test_that("model_parameters, dispersion", {
      mp <- model_parameters(mdisp)
      expect_equal(mp$Coefficient, c(58.25869, -0.87868, 47.01676, 36.99492), tolerance = 1e-2)
      expect_identical(mp$Parameter, c("wt", "wt:mpg", "SD (Intercept)", "SD (Observations)"))
      expect_identical(mp$Component, c("conditional", "conditional", "conditional", "conditional"))
    })


    m4 <- suppressWarnings(glmmTMB::glmmTMB(
      count ~ child + camper + (1 + xb | persons),
      ziformula = ~ child + camper + (1 + zg | persons),
      data = fish,
      family = glmmTMB::truncated_poisson()
    ))

    test_that("model_parameters.mixed-ran_pars", {
      expect_message(
        {
          params <- model_parameters(m4, effects = "random")
        },
        regex = "Your model may"
      )
      expect_identical(c(nrow(params), ncol(params)), c(6L, 9L))
      expect_named(
        params,
        c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "Effects", "Group", "Component")
      )
      expect_identical(
        params$Parameter,
        c(
          "SD (Intercept)", "SD (xb)", "Cor (Intercept~xb)",
          "SD (Intercept)", "SD (zg)", "Cor (Intercept~zg)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(3.40563, 1.21316, -1, 2.73583, 1.56833, 1),
        tolerance = 1e-2
      )
    })


    # exponentiate for dispersion = sigma parameters -----------------------

    set.seed(101)
    ## rbeta() function parameterized by mean and shape
    my_rbeta <- function(n, mu, shape0) {
      rbeta(n, shape1 = mu * shape0, shape2 = (1 - mu) * shape0)
    }
    n <- 100
    ng <- 10
    dd <- data.frame(x = rnorm(n), f = factor(rep(1:(n / ng), ng)))
    dd <- transform(dd, y = my_rbeta(n, mu = plogis(-1 + 2 * x + rnorm(ng)[f]), shape0 = 5))

    m_exp <- glmmTMB::glmmTMB(y ~ x + (1 | f), family = glmmTMB::beta_family(), dd)

    test_that("model_parameters, exp, glmmTMB", {
      mp1 <- model_parameters(m_exp, exponentiate = TRUE)
      mp2 <- model_parameters(m_exp, exponentiate = FALSE)
      expect_equal(mp1$Coefficient, c(0.49271, 6.75824, 5.56294, 1.14541), tolerance = 1e-3)
      expect_equal(mp1$Coefficient[3:4], mp2$Coefficient[3:4], tolerance = 1e-3)
    })

    test_that("model_parameters, no dispersion, glmmTMB", {
      mp1 <- model_parameters(m_exp, effects = "fixed", component = "conditional", exponentiate = TRUE)
      mp2 <- model_parameters(m_exp, effects = "fixed", component = "conditional", exponentiate = FALSE)
      expect_equal(mp1$Coefficient, unname(exp(unlist(glmmTMB::fixef(m_exp)$cond))), tolerance = 1e-3)
      expect_equal(mp2$Coefficient, unname(unlist(glmmTMB::fixef(m_exp)$cond)), tolerance = 1e-3)
    })


    # proper printing ---------------------

    test_that("print-model_parameters glmmTMB", {
      skip_on_os(c("mac", "linux", "solaris"))
      skip_if_not(getRversion() >= "4.3.3")

      mp <- model_parameters(m4, effects = "fixed", component = "conditional")
      out <- utils::capture.output(print(mp))
      expect_snapshot(out[-5])


      mp <- model_parameters(m4, ci_random = TRUE, effects = "random", component = "conditional", verbose = FALSE)
      out <- utils::capture.output(print(mp))
      expect_identical(
        attributes(mp)$pretty_labels,
        c(
          `SD (Intercept)` = "SD (Intercept)", `SD (xb)` = "SD (xb)",
          `Cor (Intercept~xb)` = "Cor (Intercept~xb)"
        )
      )
      expect_identical(
        substr(out, 1, 30),
        c(
          "# Random Effects",
          "",
          "Parameter                   | ",
          "------------------------------",
          "SD (Intercept: persons)     | ",
          "SD (xb: persons)            | ",
          "Cor (Intercept~xb: persons) | "
        )
      )
      expect_equal(mp$Coefficient, c(3.40563, 1.21316, -1), tolerance = 1e-3)
      expect_equal(mp$CI_low, c(1.64567, 0.5919, -1), tolerance = 1e-3)


      mp <- model_parameters(m4, ci_random = TRUE, effects = "fixed", component = "zero_inflated")
      out <- utils::capture.output(print(mp))
      expect_identical(
        attributes(mp)$pretty_labels,
        c(`(Intercept)` = "(Intercept)", child = "child", camper1 = "camper [1]")
      )
      expect_identical(
        substr(out, 1, 12),
        c(
          "# Fixed Effe", "", "Parameter   ", "------------", "(Intercept) ",
          "child       ", "camper [1]  "
        )
      )
      expect_equal(mp$Coefficient, c(1.88964, 0.15712, -0.17007), tolerance = 1e-3)
      expect_equal(mp$CI_low, c(0.5878, -0.78781, -0.92836), tolerance = 1e-3)


      mp <- model_parameters(m4, ci_random = TRUE, effects = "random", component = "zero_inflated", verbose = FALSE)
      out <- utils::capture.output(print(mp))
      expect_identical(
        attributes(mp)$pretty_labels,
        c(
          `SD (Intercept)` = "SD (Intercept)", `SD (zg)` = "SD (zg)",
          `Cor (Intercept~zg)` = "Cor (Intercept~zg)"
        )
      )
      expect_identical(
        substr(out, 1, 30),
        c(
          "# Random Effects (Zero-Inflati", "", "Parameter                   | ",
          "------------------------------", "SD (Intercept: persons)     | ",
          "SD (zg: persons)            | ", "Cor (Intercept~zg: persons) | "
        )
      )
      expect_equal(mp$Coefficient, c(2.73583, 1.56833, 1), tolerance = 1e-3)
      expect_equal(mp$CI_low, c(1.16329, 0.64246, -1), tolerance = 1e-3)


      mp <- model_parameters(m4, ci_random = TRUE, effects = "all", component = "conditional", verbose = FALSE)
      out <- utils::capture.output(print(mp))
      expect_identical(
        attributes(mp)$pretty_labels,
        c(
          `(Intercept)` = "(Intercept)", child = "child", camper1 = "camper [1]",
          `SD (Intercept)` = "SD (Intercept)", `SD (xb)` = "SD (xb)",
          `Cor (Intercept~xb)` = "Cor (Intercept~xb)"
        )
      )
      expect_identical(
        substr(out, 1, 30),
        c(
          "# Fixed Effects", "", "Parameter   | Log-Mean |   SE ",
          "------------------------------",
          "(Intercept) |     2.55 | 0.25 ", "child       |    -1.09 | 0.10 ",
          "camper [1]  |     0.27 | 0.10 ", "", "# Random Effects", "",
          "Parameter                   | ", "------------------------------",
          "SD (Intercept: persons)     | ", "SD (xb: persons)            | ",
          "Cor (Intercept~xb: persons) | "
        )
      )
      expect_equal(mp$Coefficient, c(2.54713, -1.08747, 0.2723, 3.40563, 1.21316, -1), tolerance = 1e-3)
      expect_equal(mp$CI_low, c(2.06032, -1.27967, 0.07461, 1.64567, 0.5919, -1), tolerance = 1e-3)


      mp <- model_parameters(m4, effects = "all", ci_random = TRUE, component = "zero_inflated", verbose = FALSE)
      out <- utils::capture.output(print(mp))
      expect_identical(
        attributes(mp)$pretty_labels,
        c(
          `(Intercept)` = "(Intercept)", child = "child", camper1 = "camper [1]",
          `SD (Intercept)` = "SD (Intercept)", `SD (zg)` = "SD (zg)",
          `Cor (Intercept~zg)` = "Cor (Intercept~zg)"
        )
      )
      expect_identical(
        substr(out, 1, 30),
        c(
          "# Fixed Effects (Zero-Inflatio", "", "Parameter   | Log-Mean |   SE ",
          "------------------------------", "(Intercept) |     1.89 | 0.66 ",
          "child       |     0.16 | 0.48 ", "camper [1]  |    -0.17 | 0.39 ",
          "", "# Random Effects (Zero-Inflati", "", "Parameter                   | ",
          "------------------------------", "SD (Intercept: persons)     | ",
          "SD (zg: persons)            | ", "Cor (Intercept~zg: persons) | "
        )
      )
      expect_equal(mp$Coefficient, c(1.88964, 0.15712, -0.17007, 2.73583, 1.56833, 1), tolerance = 1e-3)
      expect_equal(mp$CI_low, c(0.5878, -0.78781, -0.92836, 1.16329, 0.64246, -1), tolerance = 1e-3)


      mp <- model_parameters(m4, effects = "all", component = "all", ci_random = TRUE, verbose = FALSE)
      out <- utils::capture.output(print(mp))
      expect_identical(
        attributes(mp)$pretty_labels,
        c(
          `(Intercept)` = "(Intercept)", child = "child", camper1 = "camper [1]",
          `(Intercept)` = "(Intercept)", child = "child", camper1 = "camper1", # nolint
          `SD (Intercept)` = "SD (Intercept)", `SD (xb)` = "SD (xb)",
          `Cor (Intercept~xb)` = "Cor (Intercept~xb)",
          `SD (Intercept)` = "SD (Intercept)", `SD (zg)` = "SD (zg)", # nolint
          `Cor (Intercept~zg)` = "Cor (Intercept~zg)"
        )
      )
      expect_identical(
        substr(out, 1, 30),
        c(
          "# Fixed Effects (Count Model)", "", "Parameter   | Log-Mean |   SE ",
          "------------------------------", "(Intercept) |     2.55 | 0.25 ",
          "child       |    -1.09 | 0.10 ", "camper [1]  |     0.27 | 0.10 ",
          "", "# Fixed Effects (Zero-Inflatio", "", "Parameter   | Log-Odds |   SE ",
          "------------------------------", "(Intercept) |     1.89 | 0.66 ",
          "child       |     0.16 | 0.48 ", "camper [1]  |    -0.17 | 0.39 ",
          "", "# Random Effects Variances", "", "Parameter                   | ",
          "------------------------------", "SD (Intercept: persons)     | ",
          "SD (xb: persons)            | ", "Cor (Intercept~xb: persons) | ",
          "", "# Random Effects (Zero-Inflati", "", "Parameter                   | ",
          "------------------------------", "SD (Intercept: persons)     | ",
          "SD (zg: persons)            | ", "Cor (Intercept~zg: persons) | "
        )
      )
      expect_equal(
        mp$Coefficient,
        c(
          2.54713, -1.08747, 0.2723, 1.88964, 0.15712, -0.17007, 3.40563,
          1.21316, -1, 2.73583, 1.56833, 1
        ),
        tolerance = 1e-3
      )
      expect_equal(
        mp$CI_low,
        c(
          2.06032, -1.27967, 0.07461, 0.5878, -0.78781, -0.92836, 1.64567,
          0.5919, -1, 1.16329, 0.64246, -1
        ),
        tolerance = 1e-3
      )
    })


    # proper printing of digits ---------------------

    test_that("print-model_parameters glmmTMB digits", {
      skip_on_os(c("mac", "linux", "solaris"))
      skip_if_not(getRversion() >= "4.3.3")

      mp <- model_parameters(m4, ci_random = TRUE, effects = "all", component = "all")
      out <- utils::capture.output(print(mp, digits = 4, ci_digits = 5))
      expect_snapshot(out[-c(5, 14)])

      mp <- model_parameters(m4, effects = "all", component = "all", ci_random = TRUE, digits = 4, ci_digits = 5)
      out <- utils::capture.output(print(mp))
      expect_snapshot(out[-c(5, 14)])
    })


    # proper alignment of CIs ---------------------
    test_that("print-model_parameters glmmTMB CI alignment", {
      skip_if_not_installed("curl")
      skip_if_offline()
      skip_on_os(c("mac", "linux", "solaris"))
      skip_if_not(getRversion() >= "4.3.3")

      model_pr <- tryCatch(
        {
          load(url("https://github.com/d-morrison/parameters/raw/glmmTMB/data/pressure_durations.RData"))
          glmmTMB::glmmTMB(
            formula = n_samples ~ Surface + Side + Jaw + (1 | Participant / Session),
            ziformula = ~ Surface + Side + Jaw + (1 | Participant / Session),
            dispformula = ~1,
            family = glmmTMB::nbinom2(),
            data = pressure_durations
          )
        },
        error = function(e) {
          NULL
        }
      )

      mp <- model_parameters(model_pr, effects = "random", component = "all", ci_random = TRUE)
      expect_snapshot(print(mp))

      mp <- model_parameters(model_pr, effects = "fixed", component = "all")
      expect_snapshot(print(mp))
    })


    test_that("model_parameters.mixed-all", {
      skip_on_os(c("mac", "linux", "solaris"))
      skip_if_not(getRversion() >= "4.3.3")

      params <- model_parameters(m4, effects = "all")
      expect_identical(c(nrow(params), ncol(params)), c(12L, 12L))
      expect_identical(
        colnames(params),
        c(
          "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
          "z", "df_error", "p", "Effects", "Group", "Component"
        )
      )
      expect_identical(
        params$Parameter,
        c(
          "(Intercept)", "child", "camper1", "(Intercept)", "child",
          "camper1", "SD (Intercept)", "SD (xb)", "Cor (Intercept~xb)",
          "SD (Intercept)", "SD (zg)", "Cor (Intercept~zg)"
        )
      )
      expect_identical(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "conditional",
          "conditional", "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(
          2.54713, -1.08747, 0.2723, 1.88964, 0.15712, -0.17007, 3.40563,
          1.21316, -1, 2.73583, 1.56833, 1
        ),
        tolerance = 1e-2
      )
    })

    test_that("print-model_parameters", {
      skip_on_os(c("mac", "linux", "solaris"))
      skip_if_not(getRversion() >= "4.3.3")

      mp <- model_parameters(m1, effects = "fixed", verbose = FALSE)
      expect_snapshot(mp)

      mp <- model_parameters(m1, effects = "fixed", exponentiate = TRUE, verbose = FALSE)
      expect_snapshot(mp)

      mp <- model_parameters(m1, effects = "all", verbose = FALSE)
      expect_snapshot(mp)
    })

    test_that("print-model_parameters, random dispersion", {
      data(Salamanders, package = "glmmTMB")
      m <- glmmTMB::glmmTMB(
        count ~ spp + cover + mined + (1 | site),
        ziformula = ~ spp + mined,
        dispformula = ~ DOY + (1 | site),
        data = Salamanders,
        family = glmmTMB::nbinom1()
      )
      out <- as.data.frame(model_parameters(m, effects = "fixed", component = "all"))
      expect_identical(nrow(out), 19L)
      out <- as.data.frame(model_parameters(m, effects = "random", component = "all"))
      expect_identical(nrow(out), 1L)
      out <- as.data.frame(model_parameters(m, effects = "random", component = "all", group_level = TRUE))
      expect_identical(nrow(out), 46L)
      expect_equal(out$Coefficient, unlist(glmmTMB::ranef(m)), ignore_attr = TRUE, tolerance = 1e-4)
    })
  }
)

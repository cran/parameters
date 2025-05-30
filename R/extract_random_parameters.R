.extract_random_parameters <- function(model, ...) {
  UseMethod(".extract_random_parameters")
}


.extract_random_parameters.merMod <- function(model,
                                              ci = 0.95,
                                              effects = "random",
                                              ...) {
  insight::check_if_installed("lme4")

  out <- as.data.frame(lme4::ranef(model, condVar = TRUE), stringsAsFactors = FALSE)
  colnames(out) <- c("Group", "Parameter", "Level", "Coefficient", "SE")

  # coerce to character
  out$Parameter <- as.character(out$Parameter)
  out$Level <- as.character(out$Level)
  out$Group <- as.character(out$Group)
  out$Effects <- "random"

  if (length(ci) == 1) {
    fac <- stats::qnorm((1 + ci) / 2)
    out$CI_low <- out$Coefficient - fac * out$SE
    out$CI_high <- out$Coefficient + fac * out$SE
    ci_cols <- c("CI_low", "CI_high")
  } else {
    ci_cols <- NULL
    for (i in ci) {
      fac <- stats::qnorm((1 + i) / 2)
      ci_low <- paste0("CI_low_", i)
      ci_high <- paste0("CI_high_", i)
      out[[ci_low]] <- out$Coefficient - fac * out$SE
      out[[ci_high]] <- out$Coefficient + fac * out$SE
      ci_cols <- c(ci_cols, ci_low, ci_high)
    }
  }

  stat_column <- gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)

  # to match rbind
  out[[stat_column]] <- NA
  out$df_error <- NA
  out$p <- NA

  out <- out[c("Parameter", "Level", "Coefficient", "SE", ci_cols, stat_column, "df_error", "p", "Effects", "Group")]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p")] <- NULL
  }

  out
}


.extract_random_parameters.glmmTMB <- function(model,
                                               ci = 0.95,
                                               effects = "random",
                                               component = "conditional",
                                               ...) {
  insight::check_if_installed("lme4")
  out <- as.data.frame(lme4::ranef(model, condVar = TRUE))
  colnames(out) <- c("Component", "Group", "Parameter", "Level", "Coefficient", "SE")

  # filter component
  out <- switch(component,
    zi = ,
    zero_inflated = out[out$Component == "zi", ],
    cond = ,
    conditional = out[out$Component == "cond", ],
    disp = ,
    dispersion = out[out$Component == "disp", ],
    out
  )

  # coerce to character
  out$Parameter <- as.character(out$Parameter)
  out$Level <- as.character(out$Level)
  out$Group <- as.character(out$Group)
  out$Effects <- "random"

  # rename
  out$Component[out$Component == "zi"] <- "zero_inflated"
  out$Component[out$Component == "cond"] <- "conditional"
  out$Component[out$Component == "disp"] <- "dispersion"

  if (length(ci) == 1) {
    fac <- stats::qnorm((1 + ci) / 2)
    out$CI_low <- out$Coefficient - fac * out$SE
    out$CI_high <- out$Coefficient + fac * out$SE
    ci_cols <- c("CI_low", "CI_high")
  } else {
    ci_cols <- NULL
    for (i in ci) {
      fac <- stats::qnorm((1 + i) / 2)
      ci_low <- paste0("CI_low_", i)
      ci_high <- paste0("CI_high_", i)
      out[[ci_low]] <- out$Coefficient - fac * out$SE
      out[[ci_high]] <- out$Coefficient + fac * out$SE
      ci_cols <- c(ci_cols, ci_low, ci_high)
    }
  }

  stat_column <- gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)

  # to match rbind
  out[[stat_column]] <- NA
  out$df_error <- NA
  out$p <- NA

  out <- out[c(
    "Parameter", "Level", "Coefficient", "SE", ci_cols, stat_column,
    "df_error", "p", "Component", "Effects", "Group"
  )]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p")] <- NULL
  }
  out
}


.extract_random_parameters.MixMod <- function(model, ...) {
  NULL
}

#' @title Dominance Analysis
#' @name dominance_analysis
#' @inheritParams domir::domin
#'
#' @description Computes Dominance Analysis Statistics and Designations
#'
#' @param model A model object supported by `performance::r2()`. See 'Details'.
#'
#' @param sets A (named) list of formula objects with no left hand
#' side/response.  If the list has names, the name provided each element
#' will be used as the label for the set.  Unnamed list elements will be
#' provided a set number name based on its position among the sets as entered.
#'
#' Predictors in each formula are bound together as a set in the dominance
#' analysis and dominance statistics and designations are computed for
#' the predictors together.  Predictors in `sets` must be present in the model
#' submitted to the `model` argument and cannot be in the `all` argument.
#'
#' @param all A formula with no left hand side/response.
#'
#' Predictors in the formula are included in each subset in the dominance
#' analysis and the R2 value associated with them is subtracted from the
#' overall value.  Predictors in `all` must be present in the model
#' submitted to the `model` argument and cannot be in the `sets` argument.
#'
#' @param quote_args A character vector of arguments in the model submitted to
#' `model` to `quote()` prior to submitting to the dominance analysis.  This
#' is necessary for data masked arguments (e.g., `weights`) to prevent them
#' from being evaluated before being applied to the model and causing an error.
#'
#' @param contrasts A named list of [`contrasts`] used by the model object.
#' This list is required in order for the correct mapping of parameters to
#' predictors in the output when the model creates indicator codes for factor
#' variables using [`insight::get_modelmatrix()`]. By default, the `contrast`
#' element from the model object submitted is used. If the model object does
#' not have a `contrast` element the user can supply this named list.
#'
#' @param ...  Not used at current.
#'
#' @return Object of class `"parameters_da"`.
#'
#' An object of class `"parameters_da"` is a list of `data.frame`s composed
#' of the following elements:
#' \describe{
#'  \item{`General`}{A `data.frame` which associates dominance statistics with
#'  model parameters. The variables in this `data.frame` include:
#'   \describe{
#'     \item{`Parameter`}{Parameter names.}
#'      \item{`General_Dominance`}{Vector of general dominance statistics.
#'      The R2 ascribed to variables in the `all` argument are also reported
#'      here though they are not general dominance statistics.}
#'      \item{`Percent`}{Vector of general dominance statistics normalized
#'      to sum to 1.}
#'      \item{`Ranks`}{Vector of ranks applied to the general dominance
#'      statistics.}
#'      \item{`Subset`}{Names of the subset to which the parameter belongs in
#'      the dominance analysis.  Each other `data.frame` returned will refer
#'      to these subset names.}}}
#'  \item{`Conditional`}{A `data.frame` of conditional dominance
#'  statistics.  Each observation represents a subset and each variable
#'  represents an the average increment to R2 with a specific number of
#'  subsets in the model.  `NULL` if `conditional` argument is `FALSE`.}
#'  \item{`Complete`}{A `data.frame` of complete dominance
#'  designations. The subsets in the observations are compared to the
#'  subsets referenced in each variable. Whether the subset
#'  in each variable dominates the subset in each observation is
#'  represented in the  logical value. `NULL` if `complete`
#'  argument is `FALSE`.}
#' }
#'
#' @details Computes two decompositions of the model's R2 and returns
#' a matrix of designations from which predictor relative importance
#' determinations can be obtained.
#'
#' Note in the output that the "constant" subset is associated with a
#' component of the model that does not directly contribute to the R2 such
#' as an intercept. The "all" subset is apportioned a component of the fit
#' statistic but is not considered a part of the dominance analysis and
#' therefore does not receive a rank, conditional dominance statistics, or
#' complete dominance designations.
#'
#' The input model is parsed using `insight::find_predictors()`, does not
#' yet support interactions, transformations, or offsets applied in the R
#' formula, and will fail with an error if any such terms are detected.
#'
#' The model submitted must accept an formula object as a `formula`
#' argument.  In addition, the model object must accept the data on which
#' the model is estimated as a `data` argument.  Formulas submitted
#' using object references (i.e., `lm(mtcars$mpg ~ mtcars$vs)`) and
#' functions that accept data as a non-`data` argument
#' (e.g., `survey::svyglm()` uses `design`) will fail with an error.
#'
#' Models that return `TRUE` for the `insight::model_info()`
#' function's values "is_bayesian", "is_mixed", "is_gam",
#' is_multivariate", "is_zero_inflated",
#' or "is_hurdle" are not supported at current.
#'
#' When `performance::r2()` returns multiple values, only the first is used
#' by default.
#'
#' @references
#' - Azen, R., & Budescu, D. V. (2003). The dominance analysis approach
#'   for comparing predictors in multiple regression. Psychological Methods,
#'   8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#'
#' - Budescu, D. V. (1993). Dominance analysis: A new approach to the
#'   problem of relative importance of predictors in multiple regression.
#'   Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#'
#' - Groemping, U. (2007). Estimators of relative importance in linear
#'   regression based on variance decomposition. The American Statistician,
#'   61(2), 139-147. doi:10.1198/000313007X188252
#'
#' @seealso [domir::domin()]
#'
#' @author Joseph Luchman
#'
#' @examplesIf require("domir") && require("performance")
#' data(mtcars)
#'
#' # Dominance Analysis with Logit Regression
#' model <- glm(vs ~ cyl + carb + mpg, data = mtcars, family = binomial())
#'
#' performance::r2(model)
#' dominance_analysis(model)
#'
#' # Dominance Analysis with Weighted Logit Regression
#' model_wt <- glm(vs ~ cyl + carb + mpg,
#'   data = mtcars,
#'   weights = wt, family = quasibinomial()
#' )
#'
#' dominance_analysis(model_wt, quote_args = "weights")
#' @export
dominance_analysis <- function(model, sets = NULL, all = NULL,
                               conditional = TRUE, complete = TRUE,
                               quote_args = NULL, contrasts = model$contrasts,
                               ...) {
  # Exit Conditions ----
  insight::check_if_installed("domir")
  insight::check_if_installed("performance")

  if (!insight::is_regression_model(model)) {
    insight::format_error(
      paste(deparse(substitute(model)), "is not a supported {.pkg insight} model."),
      "You may be able to dominance analyze this model using the {.pkg domir} package."
    )
  }

  if (!any(utils::.S3methods("r2", class = class(model)[[1]], envir = getNamespace("performance")) %in%
    paste0("r2.", class(model)))) {
    insight::format_error(
      paste(deparse(substitute(model)), "does not have a {.pkg perfomance}-supported `r2()` method."),
      "You may be able to dominance analyze this model using the {.pkg domir} package."
    )
  }

  model_info <- insight::model_info(model)
  if (any(unlist(model_info[c("is_bayesian", "is_mixed", "is_gam", "is_multivariate", "is_zero_inflated", "is_hurdle")]))) {
    insight::format_error(
      paste0("`dominance_analysis()` does not yet support models of class `", class(model)[[1]], "`."),
      "You may be able to dominance analyze this model using the {.pkg domir} package."
    )
  }

  if (length(insight::find_predictors(model, flatten = TRUE)) < 2) {
    insight::format_error("Too few predictors for a dominance analysis.")
  }

  if (!is.null(insight::find_offset(model))) {
    insight::format_error(
      "Offsets in the model are not allowed in this version of `dominance_analysis()`.",
      "Try using package {.pkg domir}."
    )
  }

  if (!all(insight::find_predictors(model, flatten = TRUE) %in% insight::find_terms(model)$conditional)) {
    insight::format_error(
      "Predictors do not match terms.",
      "This usually occurs when there are in-formula predictor transformations such as `log(x)` or `I(x+z)`.",
      "`dominance_analysis()` cannot yet accommodate such terms. Reformat your model to ensure all parameters",
      "match predictors in the data or use the {.pkg domir} package."
    )
  }

  if (!is.null(insight::find_interactions(model))) {
    insight::format_error("Interactions in the model formula are not allowed.")
  }

  if (!is.null(sets)) {
    if (!is.list(sets)) {
      insight::format_error("`sets` argument must be submitted as list.")
    }

    if (length(sets) != length(unlist(sets))) {
      insight::format_error("Nested lists are not allowed in `sets`.")
    }

    if (!all(sapply(sets, inherits, "formula"))) {
      insight::format_error("Each element of list in `sets` must be a formula.")
    }

    if (any(sapply(sets, function(x) attr(stats::terms(x), "response") == 1))) {
      insight::format_error("Formulas in `sets` argument must not have responses/left hand sides.")
    }
  }

  if (!is.null(all)) {
    if (!inherits(all, "formula")) {
      insight::format_error("`all` argument must be submitted as a formula.")
    }

    if (attr(stats::terms(all), "response") == 1) {
      insight::format_error("Formula in `all` argument must not have a response/left hand side.")
    }
  }

  if (!is.null(quote_args) && !all(is.character(quote_args))) {
    insight::format_error("All arguments in `quote_args` must be characters.")
  }

  # Collect components for arguments ----
  ivs <- insight::find_predictors(model, flatten = TRUE)

  dv <- insight::find_response(model)

  # reg <- insight::model_name(model) # insight::get_call + as.list() and take first element? glm.nb doesn't work...
  reg <- as.list(insight::get_call(model))[[1]]

  # Process sets ----
  if (!is.null(sets)) {
    # gather predictors from each set
    sets_processed <- lapply(sets, function(x) attr(stats::terms(x), "term.labels"))

    # remove predictors from `ivs` list if in sets
    set_remove_loc <- unlist(lapply(sets_processed, function(x) which(ivs %in% x)))

    if (length(set_remove_loc) != length(unlist(sets_processed))) {
      wrong_set_terms <- unlist(sets_processed)[which(!(unlist(sets_processed) %in% ivs))]

      insight::format_error(
        "Terms",
        paste(wrong_set_terms, sep = " "),
        "in `sets` argument do not match any predictors in model."
      )
    }

    ivs <- ivs[-set_remove_loc]

    # apply names to sets
    set_names <- names(sets)

    missing_set_names <- which(set_names == "")

    if (length(missing_set_names) > 0) {
      set_names[missing_set_names] <- paste0("set", missing_set_names)
    }

    if (any(set_names %in% c("all", "constant"))) {
      insight::format_error(
        "Names \"all\" and \"constant\" are reserved for subset names in the `dominance_analysis()` function.",
        "Please rename any sets currently named \"all\" or \"constant\"."
      )
    }

    if (any(set_names %in% ivs)) {
      repeat_names <- set_names[which(set_names %in% ivs)]

      insight::format_error(
        "Set names",
        paste(repeat_names, sep = " "), "are also the names of invidiual predictors.",
        "Please rename these sets."
      )
    }
  } else {
    sets_processed <- NULL
  }

  # Process all ----
  if (!is.null(all)) {
    # gather predictors in all
    all_processed <- attr(stats::terms(all), "term.labels")

    # remove predictors in all from `ivs` list
    all_remove_loc <- which(ivs %in% all_processed)

    if (any(all_processed %in% unlist(sets_processed))) {
      reused_terms <-
        all_processed[which(all_processed %in% unlist(sets_processed))]

      insight::format_error(
        "Terms",
        paste(reused_terms, sep = " "),
        "in all argument are also used in `sets` argument."
      )
    }

    if (length(all_remove_loc) != length(unlist(all_processed))) {
      wrong_all_terms <- all_processed[which(!(all_processed) %in% ivs)]

      insight::format_error(
        "Terms",
        paste(wrong_all_terms, sep = " "),
        "in `all` argument do not match any predictors in model."
      )
    }

    ivs <- ivs[-all_remove_loc] # update IVs
  } else {
    all_processed <- NULL
  }

  # name collisions across subsets - exit
  if (any(ivs %in% c("all", "constant"))) {
    insight::format_error(
      "Names 'all' and 'constant' are reserved for subset names in the `dominance_analysis()` function.",
      "Please rename any predictors currently named 'all' or 'constant.'",
      "Alternatively, put the predictor in a set by itself."
    )
  }

  # big DA warning
  if (length(c(ivs, unlist(sets_processed))) > 15) {
    insight::format_warning(
      paste0("Total of ", 2^length(ivs) - 1, " models to be estimated."),
      "Process may take some time."
    )
  }

  # Build non-formula model arguments to `domin` ----
  if (length(ivs) == 0) ivs <- "1"
  fml <- stats::reformulate(ivs, response = dv, intercept = insight::has_intercept(model))

  data <- insight::get_data(model, verbose = FALSE)

  args <- as.list(insight::get_call(model), collapse = "") # extract all arguments from call

  loc <- which(!(names(args) %in% c("formula", "data"))) # find formula and data arguments

  if (length(which(names(args) %in% c("formula", "data"))) != 2) {
    # exit if formula and data arguments missing
    insight::format_error("Model submitted does not have a formula and `data` argument.")
  }

  args <- args[loc] # remove formula and data arguments
  args <- args[-1] # remove function name

  # quote arguments for domin
  for (arg in quote_args) {
    if (arg %in% names(args)) {
      args[[arg]] <- str2lang(paste0("quote(", deparse(args[[arg]]), ")", collapse = ""))
    } else {
      insight::format_error(arg, " in `quote_args` not among arguments in model.")
    }
  }

  # Internal wrapper to ensure r2 values conform to domin ----
  .r2_wrap <- function(model, ...) {
    list(fitstat = performance::r2(model, ...)[[1]])
  }

  # Finalize and implement DA
  args2domin <- append(list(
    formula_overall = fml, reg = reg, fitstat = list(.r2_wrap, "fitstat"),
    data = data, conditional = conditional, complete = complete,
    sets = sets_processed, all = all_processed
  ), args)

  utils::capture.output({
    domir_res <- do.call(domir::domin, args2domin)
  })

  # Set up returned data.frames ----
  # Apply set names to domin results
  if (!is.null(sets)) {
    names(domir_res$General_Dominance) <-
      c(
        names(domir_res$General_Dominance)[1:(length(domir_res$General_Dominance) - length(set_names))],
        set_names
      )

    if (conditional) {
      rownames(domir_res$Conditional_Dominance) <-
        names(domir_res$General_Dominance)
    }
  }

  if (complete) {
    colnames(domir_res$Complete_Dominance) <-
      paste0("dmn_", names(domir_res$General_Dominance))

    dimnames(domir_res$Complete_Dominance) <- list(
      colnames(domir_res$Complete_Dominance),
      names(domir_res$General_Dominance)
    )

    domir_res$Complete_Dominance <- t(domir_res$Complete_Dominance)
  }

  # Map parameter names to subsets - structure set-up
  da_df_res <-
    da_df_cat <-
    .data_frame(parameter = insight::find_parameters(model, flatten = TRUE))

  da_df_cat <- .data_frame(da_df_cat, subset = NA_character_)

  # if parameter is same as domin name, copy it to 'subset'
  da_df_cat$subset <-
    ifelse((da_df_res$parameter %in%
      names(domir_res$General_Dominance)) &
      (is.na(da_df_cat$subset)),
    da_df_res$parameter,
    da_df_cat$subset
    )

  # Expand contrast names
  if (!is.null(contrasts)) {
    contr_names <-
      lapply(
        names(contrasts),
        function(name) {
          pred_loc <- which(insight::find_predictors(model, flatten = TRUE) == name)

          pred_names <-
            colnames(insight::get_modelmatrix(model))[
              which(attr(insight::get_modelmatrix(model), "assign") == pred_loc)
            ]
        }
      )

    names(contr_names) <- names(contrasts)
    contr_map <- rep(names(contr_names), lengths(contr_names))
    names(contr_map) <- unlist(contr_names)

    for (subset in which(is.na(da_df_cat$subset))) {
      if ((da_df_res$parameter[[subset]] %in% names(contr_map))) {
        da_df_cat$subset[[subset]] <-
          contr_map[[which(names(contr_map) == da_df_res$parameter[[subset]])]]
      }
    }
  }

  # Apply set names
  if (!is.null(sets)) {
    for (set in seq_along(sets)) {
      set_name <- if (!is.null(names(sets)[[set]])) {
        names(sets)[[set]]
      } else {
        paste0("set", set)
      }

      da_df_cat$subset <-
        replace(
          da_df_cat$subset,
          da_df_res$parameter %in% all.vars(sets[[set]]), set_name
        )

      da_df_cat$subset <-
        replace(
          da_df_cat$subset,
          da_df_cat$subset %in% all.vars(sets[[set]]), set_name
        )
    }
  }

  # Apply 'all' names
  if (!is.null(all)) {
    da_df_cat$subset <-
      replace(
        da_df_cat$subset,
        da_df_res$parameter %in% all.vars(all), "all"
      )

    da_df_cat$subset <-
      replace(
        da_df_cat$subset,
        da_df_cat$subset %in% all.vars(all), "all"
      )
  }

  # assume remaining parameters are part of 'constant'
  da_df_cat$subset <-
    replace(
      da_df_cat$subset,
      is.na(da_df_cat$subset), "constant"
    )

  # merge subsets and DA results to parameter names
  da_df_res <-
    datawizard::data_merge(
      da_df_cat,
      .data_frame(
        subset = names(domir_res$General_Dominance),
        general_dominance = domir_res$General_Dominance
      )
    )

  # plug in value of 'all' in 'all' subsets/parameters
  if (!is.null(all)) {
    da_df_res$general_dominance <-
      replace(
        da_df_res$general_dominance,
        da_df_res$subset == "all",
        domir_res$Fit_Statistic_All_Subsets
      )
  }

  # merge standardized general dominance stat values
  da_df_res <-
    datawizard::data_merge(
      da_df_res,
      .data_frame(
        subset = names(domir_res$General_Dominance),
        standardized = domir_res$Standardized
      )
    )

  # merge  ranks based on general dominance stat values
  da_df_res <-
    datawizard::data_merge(
      da_df_res,
      .data_frame(
        subset = names(domir_res$General_Dominance),
        ranks = domir_res$Ranks
      )
    )

  da_df_res <-
    datawizard::data_relocate(da_df_res, "subset", after = "ranks")

  if (conditional) {
    da_df_cdl <- .data_frame(Subset = names(domir_res$General_Dominance))

    da_df_cdl <- datawizard::data_merge(
      da_df_cdl,
      .data_frame(
        Subset = names(domir_res$General_Dominance),
        domir_res$Conditional_Dominance
      )
    )

    cols_to_select <- colnames(da_df_cdl)[2:length(da_df_cdl)]
    da_df_cdl <- datawizard::data_rename(
      da_df_cdl,
      select = cols_to_select,
      replacement = colnames(domir_res$Conditional_Dominance)
    )
  } else {
    da_df_cdl <- NULL
  }

  if (complete) {
    da_df_cpt <- .data_frame(Subset = names(domir_res$General_Dominance))

    da_df_cpt <- datawizard::data_merge(
      da_df_cpt,
      .data_frame(
        Subset = names(domir_res$General_Dominance),
        domir_res$Complete_Dominance
      )
    )

    cols_to_select <- colnames(da_df_cpt)[2:length(da_df_cpt)]
    da_df_cpt <- datawizard::data_rename(
      da_df_cpt,
      select = cols_to_select,
      replacement = colnames(domir_res$Complete_Dominance)
    )
  } else {
    da_df_cpt <- NULL
  }

  da_df_res <- datawizard::data_rename(
    da_df_res,
    replacement = c(
      "Parameter", "General_Dominance",
      "Percent", "Ranks", "Subset"
    )
  )

  da_list <- list(
    General = da_df_res,
    Conditional = da_df_cdl,
    Complete = da_df_cpt
  )

  # add attributes and class
  attr(da_list, "model_R2") <- domir_res$Fit_Statistic_Overall
  attr(da_list$General, "table_title") <- "General Dominance Statistics"
  if (conditional) attr(da_list$Conditional, "table_title") <- "Conditional Dominance Statistics"
  if (complete) attr(da_list$Complete, "table_title") <- "Complete Dominance Designations"

  class(da_list) <- "parameters_da"

  da_list
}


# methods ------------------------------


#' @export
print.parameters_da <- function(x, digits = 3, ...) {
  insight::print_color("# Dominance Analysis Results", "blue")
  cat("\n\n")

  cat("Model R2 Value: ", sprintf("%.*f", digits, attr(x, "model_R2")), "\n\n")

  printed_x <- x

  printed_x$General <- datawizard::data_rename(x$General,
    select = "General_Dominance",
    replacement = "General Dominance"
  )

  if (!is.null(x$Conditional)) {
    cdl_col <- ncol(x$Conditional)

    cdl_names <- paste0("IVs_", 1:(cdl_col - 1))

    cdl_names_rep <- paste("IVs:", 1:(cdl_col - 1))

    printed_x$Conditional <-
      datawizard::data_rename(x$Conditional,
        select = cdl_names,
        replacement = cdl_names_rep
      )
  }

  if (!is.null(x$Complete)) {
    cpt_names <- names(x$Complete)[-1]

    cpt_names_rep <- gsub(
      "dmn_", "< ",
      cpt_names,
      fixed = TRUE
    )

    printed_x$Complete <-
      datawizard::data_rename(x$Complete,
        select = cpt_names,
        replacement = cpt_names_rep
      )
  }

  cat(insight::export_table(printed_x, digits = digits, ...))

  invisible(x)
}

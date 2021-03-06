#' @title Between-within approximation for SEs, CIs and p-values
#' @name p_value_betwithin
#'
#' @description Approximation of degrees of freedom based on a "between-within" heuristic.
#'
#' @param model A mixed model.
#' @param dof Degrees of Freedom.
#' @inheritParams ci.merMod
#'
#' @details \subsection{Small Sample Cluster corrected Degrees of Freedom}{
#' Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statistics (see \cite{Li and Redden 2015}). The
#' \emph{Between-within} denominator degrees of freedom approximation is
#' recommended in particular for (generalized) linear mixed models with repeated
#' measurements (longitudinal design). \code{dof_betwithin}) implements a heuristic
#' based on the between-within approach. \strong{Note} that this implementation
#' does not return exactly the same results as shown in \cite{Li and Redden 2015},
#' but similar.
#' }
#' \subsection{Degrees of Freedom for Longitudinal Designs (Repeated Measures)}{
#' In particular for repeated measure designs (longitudinal data analysis),
#' the \emph{between-within} heuristic is likely to be more accurate than simply
#' using the residual or infinite degrees of freedom, because \code{dof_betwithin()}
#' returns different degrees of freedom for within-cluster and between-cluster effects.
#' }
#' @seealso \code{dof_betwithin()} and \code{se_betwithin()} are small helper-functions
#' to calculate approximated degrees of freedom and standard errors of model
#' parameters, based on the "between-within" heuristic.
#'
#' @examples
#' \donttest{
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'   dof_betwithin(model)
#'   p_value_betwithin(model)
#' }
#' }
#' @return A data frame.
#' @references \itemize{
#'   \item Elff, M.; Heisig, J.P.; Schaeffer, M.; Shikano, S. (2019). Multilevel Analysis with Few Clusters: Improving Likelihood-based Methods to Provide Unbiased Estimates and Accurate Inference, British Journal of Political Science.
#'   \item Li, P., Redden, D. T. (2015). Comparing denominator degrees of freedom approximations for the generalized linear mixed model in analyzing binary outcome in small sample cluster-randomized trials. BMC Medical Research Methodology, 15(1), 38. \doi{10.1186/s12874-015-0026-x}
#' }
#' @export
p_value_betwithin <- function(model, dof = NULL) {
  if (is.null(dof)) {
    dof <- dof_betwithin(model)
  }
  .p_value_dof(model, dof, method = "betwithin")
}

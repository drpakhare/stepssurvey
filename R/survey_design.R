#' Set up a complex survey design for STEPS data
#'
#' Creates a survey design object that accounts for sampling weights,
#' stratification, and clustering. Automatically detects which design
#' elements are present and adapts accordingly.
#'
#' @param data A data frame (typically from [clean_steps_data()]).
#'
#' @return A survey design object (from the `survey` package) suitable
#' for weighted estimation with [svyprop()] and [svymn()].
#'
#' @details
#' The function handles five design cases:
#' 1. Full complex design: weights + strata + clusters
#' 2. Weights + clusters, no strata
#' 3. Weights + strata, no clusters
#' 4. Weights only
#' 5. Unweighted (simple random sampling)
#'
#' Extreme weights are trimmed to the \code{[0.1, 10]} range if weights are present.
#'
#' @export
setup_survey_design <- function(data) {

  has_weight  <- "wt_final" %in% names(data) && !all(data$wt_final == 1)
  has_strata  <- "stratum"  %in% names(data)
  has_cluster <- "psu"      %in% names(data)

  # -- Case 1: Full complex design (weights + strata + clusters) -------------
  if (has_weight & has_strata & has_cluster) {
    message("  Design: Stratified cluster sampling with weights")
    design <- survey::svydesign(
      ids     = ~psu,
      strata  = ~stratum,
      weights = ~wt_final,
      data    = data,
      nest    = TRUE
    )

  # -- Case 2: Weights + clusters, no strata ---------------------------------
  } else if (has_weight & has_cluster & !has_strata) {
    message("  Design: Cluster sampling with weights (no strata)")
    design <- survey::svydesign(
      ids     = ~psu,
      weights = ~wt_final,
      data    = data
    )

  # -- Case 3: Weights + strata, no clusters ---------------------------------
  } else if (has_weight & has_strata & !has_cluster) {
    message("  Design: Stratified sampling with weights (no clusters)")
    design <- survey::svydesign(
      ids     = ~1,
      strata  = ~stratum,
      weights = ~wt_final,
      data    = data
    )

  # -- Case 4: Weights only ---------------------------------------------------
  } else if (has_weight & !has_strata & !has_cluster) {
    message("  \u26a0 Design: Weights only (no strata or cluster info) - SE may be underestimated")
    design <- survey::svydesign(
      ids     = ~1,
      weights = ~wt_final,
      data    = data
    )

  # -- Case 5: Unweighted ----------------------------------------------------
  } else {
    message("  \u26a0 Design: Unweighted (simple random sampling assumed)")
    design <- survey::svydesign(ids = ~1, data = data)
  }

  # Trim extreme weights (optional but recommended)
  if (has_weight) {
    design <- survey::trimWeights(design, lower = 0.1, upper = 10, strict = FALSE)
    message("  - Weights trimmed to [0.1, 10] range")
  }

  n    <- nrow(data)
  wn   <- round(sum(stats::weights(design)))
  message(glue::glue("  \u2192 Unweighted n = {format(n, big.mark=',')}"))
  message(glue::glue("  \u2192 Weighted  N = {format(wn, big.mark=',')}"))

  return(design)
}

#' Weighted proportion estimation with 95% CI
#'
#' Calculates weighted proportions (as percentages) with 95% confidence
#' intervals for a yes/no variable, optionally stratified by a grouping
#' variable.
#'
#' @param formula A formula (e.g., `~variable` or using binary variables).
#' @param design A survey design object (from [setup_survey_design()]).
#' @param by Optional formula for stratification (e.g., `~sex`).
#' @param na.rm Logical; if TRUE (default), omit NA values.
#'
#' @return A data frame with columns:
#' - `estimate`: estimated proportion (%)
#' - `lower`: 95% CI lower bound (%)
#' - `upper`: 95% CI upper bound (%)
#' - `se`: standard error (%)
#' - If `by` is specified: grouping column(s) prepended
#'
#' @export
svyprop <- function(formula, design, by = NULL, na.rm = TRUE) {

  if (is.null(by)) {
    est <- survey::svymean(formula, design, na.rm = na.rm)
    ci  <- confint(est)
    data.frame(
      estimate = as.numeric(est) * 100,
      lower    = ci[, 1] * 100,
      upper    = ci[, 2] * 100,
      se       = sqrt(diag(attr(est, "var"))) * 100
    )
  } else {
    result <- survey::svyby(formula, by, design, survey::svymean, na.rm = na.rm, vartype = "ci")
    # Rename formula-derived estimate column to 'estimate' for consistency
    est_col <- setdiff(names(result), c(all.vars(by), "ci_l", "ci_u", "se"))
    result |>
      dplyr::rename(estimate = dplyr::all_of(est_col), lower = ci_l, upper = ci_u) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ . * 100))
  }
}

#' Weighted mean estimation with 95% CI
#'
#' Calculates weighted means with 95% confidence intervals for a
#' continuous variable, optionally stratified by a grouping variable.
#'
#' @param formula A formula (e.g., `~age`).
#' @param design A survey design object (from [setup_survey_design()]).
#' @param by Optional formula for stratification (e.g., `~sex`).
#' @param na.rm Logical; if TRUE (default), omit NA values.
#'
#' @return A data frame with columns:
#' - `estimate`: estimated mean
#' - `lower`: 95% CI lower bound
#' - `upper`: 95% CI upper bound
#' - `se`: standard error
#' - If `by` is specified: grouping column(s) prepended
#'
#' @export
svymn <- function(formula, design, by = NULL, na.rm = TRUE) {
  if (is.null(by)) {
    est <- survey::svymean(formula, design, na.rm = na.rm)
    ci  <- confint(est)
    data.frame(
      estimate = as.numeric(est),
      lower    = ci[, 1],
      upper    = ci[, 2],
      se       = sqrt(diag(attr(est, "var")))
    )
  } else {
    result <- survey::svyby(formula, by, design, survey::svymean, na.rm = na.rm, vartype = "ci")
    est_col <- setdiff(names(result), c(all.vars(by), "ci_l", "ci_u", "se"))
    result |>
      dplyr::rename(estimate = dplyr::all_of(est_col), lower = ci_l, upper = ci_u)
  }
}

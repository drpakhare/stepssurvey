#' Create a single survey design for a given weight column
#'
#' Internal helper that builds one [survey::svydesign] object.
#'
#' @param data A data frame (typically from [clean_steps_data()]).
#' @param wt_col Character name of the weight column to use.
#' @param label Label for messages (e.g. "Step 1").
#'
#' @return A [survey::svydesign] object.
#' @keywords internal
.make_design <- function(data, wt_col, label = "") {

  has_weight  <- wt_col %in% names(data) && !all(data[[wt_col]] == 1)
  has_strata  <- "stratum"  %in% names(data)
  has_cluster <- "psu"      %in% names(data)

  # Temporarily rename wt_col to a fixed name for formula interface
  data$.wt <- data[[wt_col]]

  # Drop rows with NA or zero weights (survey::svydesign fails on NA weights)
  if (has_weight) {
    n_na_wt <- sum(is.na(data$.wt) | data$.wt == 0)
    if (n_na_wt > 0) {
      data <- data[!is.na(data$.wt) & data$.wt > 0, ]
      if (nchar(label)) message(glue::glue("  - Dropped {n_na_wt} rows with NA/zero weights"))
    }
  }

  # -- Case 1: Full complex design (weights + strata + clusters) -------------
  if (has_weight & has_strata & has_cluster) {
    if (nchar(label)) message(glue::glue("  {label}: Stratified cluster sampling with weights"))
    design <- survey::svydesign(
      ids     = ~psu,
      strata  = ~stratum,
      weights = ~.wt,
      data    = data,
      nest    = TRUE
    )

  # -- Case 2: Weights + clusters, no strata ---------------------------------
  } else if (has_weight & has_cluster & !has_strata) {
    if (nchar(label)) message(glue::glue("  {label}: Cluster sampling with weights (no strata)"))
    design <- survey::svydesign(
      ids     = ~psu,
      weights = ~.wt,
      data    = data
    )

  # -- Case 3: Weights + strata, no clusters ---------------------------------
  } else if (has_weight & has_strata & !has_cluster) {
    if (nchar(label)) message(glue::glue("  {label}: Stratified sampling with weights (no clusters)"))
    design <- survey::svydesign(
      ids     = ~1,
      strata  = ~stratum,
      weights = ~.wt,
      data    = data
    )

  # -- Case 4: Weights only ---------------------------------------------------
  } else if (has_weight & !has_strata & !has_cluster) {
    if (nchar(label)) message(glue::glue("  {label}: \u26a0 Weights only (no strata or cluster info) - SE may be underestimated"))
    design <- survey::svydesign(
      ids     = ~1,
      weights = ~.wt,
      data    = data
    )

  # -- Case 5: Unweighted ----------------------------------------------------
  } else {
    if (nchar(label)) message(glue::glue("  {label}: \u26a0 Unweighted (simple random sampling assumed)"))
    design <- survey::svydesign(ids = ~1, data = data)
  }

  # NOTE: WHO official STEPS scripts do NOT trim weights.
  # Previous versions of this package applied trimWeights(median/10, median*10),
  # but this caused systematic ~2.5pp discrepancies in tobacco indicators.
  # Removed to align with WHO methodology.

  return(design)
}


#' Set up survey designs for STEPS data (one per Step)
#'
#' Creates up to three survey design objects — one per WHO STEPS Step —
#' each using the appropriate step-specific weight column
#' (`wt_step1`, `wt_step2`, `wt_step3`).
#'
#' The returned object is a list of class `"steps_designs"` with elements
#' `$step1`, `$step2`, `$step3`.  For backward compatibility it can also
#' be used directly as a single design (it delegates to `$step1`).
#'
#' @param data A data frame (typically from [clean_steps_data()]).
#'
#' @return A list of class `"steps_designs"` with three
#'   [survey::svydesign] objects (step1, step2, step3).
#'
#' @details
#' The function handles five design cases per step:
#' 1. Full complex design: weights + strata + clusters
#' 2. Weights + clusters, no strata
#' 3. Weights + strata, no clusters
#' 4. Weights only
#' 5. Unweighted (simple random sampling)
#'
#' Weights are used as-is without trimming, consistent with the
#' WHO official STEPS analysis scripts.
#'
#' @export
setup_survey_design <- function(data) {

  message("  Setting up survey designs (per WHO STEPS Step)...")

  # Match WHO STEPS official scripts: handle lone PSUs gracefully

  options(survey.lonely.psu = "adjust")
  options(survey.adjust.domain.lonely = TRUE)

  # Check whether we actually have 3 distinct weight columns
  has_s1 <- "wt_step1" %in% names(data)
  has_s2 <- "wt_step2" %in% names(data)
  has_s3 <- "wt_step3" %in% names(data)

  # Determine if the step weights actually differ
  steps_differ <- FALSE
  if (has_s1 && has_s2 && !identical(data$wt_step1, data$wt_step2)) steps_differ <- TRUE
  if (has_s1 && has_s3 && !identical(data$wt_step1, data$wt_step3)) steps_differ <- TRUE

  if (steps_differ) {
    message("  \u2713 Distinct step-specific weights detected (WStep1/WStep2/WStep3)")
    d1 <- .make_design(data, "wt_step1", "Step 1 (behavioural)")
    d2 <- .make_design(data, "wt_step2", "Step 2 (physical)")
    d3 <- .make_design(data, "wt_step3", "Step 3 (biochemical)")
  } else {
    # All weights are the same — create one and reuse
    wt_col <- if (has_s1) "wt_step1" else "wt_final"
    d1 <- .make_design(data, wt_col, "Design")
    d2 <- d1
    d3 <- d1
  }

  n    <- nrow(data)
  wn   <- round(sum(stats::weights(d1)))
  message(glue::glue("  \u2192 Unweighted n = {format(n, big.mark=',')}"))
  message(glue::glue("  \u2192 Weighted  N (Step 1) = {format(wn, big.mark=',')}"))
  if (steps_differ) {
    wn2 <- round(sum(stats::weights(d2)))
    wn3 <- round(sum(stats::weights(d3)))
    message(glue::glue("  \u2192 Weighted  N (Step 2) = {format(wn2, big.mark=',')}"))
    message(glue::glue("  \u2192 Weighted  N (Step 3) = {format(wn3, big.mark=',')}"))
  }
  message("  Survey design created")

  designs <- list(step1 = d1, step2 = d2, step3 = d3)
  class(designs) <- c("steps_designs", "list")
  return(designs)
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
    # Dynamically detect CI column names (may vary by survey package version)
    ci_l_col <- grep("^ci_l", names(result), value = TRUE)[1]
    ci_u_col <- grep("^ci_u", names(result), value = TRUE)[1]
    if (is.na(ci_l_col) || is.na(ci_u_col)) {
      stop("Could not detect CI columns in svyby result. Columns: ",
           paste(names(result), collapse = ", "))
    }
    # Rename formula-derived estimate column to 'estimate' for consistency
    est_col <- setdiff(names(result), c(all.vars(by), ci_l_col, ci_u_col, "se"))
    result |>
      dplyr::rename(estimate = dplyr::all_of(est_col),
                     lower = dplyr::all_of(ci_l_col),
                     upper = dplyr::all_of(ci_u_col)) |>
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
    # Dynamically detect CI column names (may vary by survey package version)
    ci_l_col <- grep("^ci_l", names(result), value = TRUE)[1]
    ci_u_col <- grep("^ci_u", names(result), value = TRUE)[1]
    if (is.na(ci_l_col) || is.na(ci_u_col)) {
      stop("Could not detect CI columns in svyby result. Columns: ",
           paste(names(result), collapse = ", "))
    }
    est_col <- setdiff(names(result), c(all.vars(by), ci_l_col, ci_u_col, "se"))
    result |>
      dplyr::rename(estimate = dplyr::all_of(est_col),
                     lower = dplyr::all_of(ci_l_col),
                     upper = dplyr::all_of(ci_u_col))
  }
}

#' Compute Tobacco Use Indicators
#'
#' Calculates prevalence of tobacco use from a survey design object.
#' Computes proportions of current and daily tobacco use, stratified by
#' sex and age group where available.
#'
#' When both smoking and smokeless tobacco variables are present,
#' `current_tobacco_any` (either smoking or smokeless) is preferred
#' as the headline tobacco indicator.  The function also reports
#' `current_smoker` and `current_smokeless` separately if available.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains proportion
#'   estimates (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `current_tobacco_any_total/by_sex/by_age`: any current tobacco use
#'     (smoking or smokeless) -- preferred headline variable
#'   - `current_tobacco_total/by_sex/by_age`: current tobacco smoking
#'   - `current_smoker_total/by_sex/by_age`: current smoker
#'   - `current_smokeless_total/by_sex/by_age`: current smokeless tobacco
#'   - `daily_tobacco_total/by_sex/by_age`: daily tobacco use
#'   (only elements for variables present in design are returned)
#'
#' @seealso [compute_all_indicators()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   design <- setup_survey_design(data = survey_data)
#'   tobacco_results <- compute_tobacco_indicators(design)
#' }
compute_tobacco_indicators <- function(design) {
  message("  Computing tobacco indicators...")
  results <- list()

  # Look for all tobacco-related variables in the design
  candidate_vars <- c(
    "current_tobacco_any",   # combined smoking + smokeless (preferred headline)
    "current_tobacco",       # tobacco smoking (may be same as current_smoker)
    "current_smoker",        # current smoker
    "current_smokeless",     # current smokeless tobacco
    "daily_smokeless",       # daily smokeless (fallback if current_smokeless absent)
    "daily_tobacco"          # daily tobacco use
  )
  vars <- candidate_vars[candidate_vars %in% names(design$variables)]

  for (v in vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  return(results)
}

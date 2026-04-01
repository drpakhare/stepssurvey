#' Compute Tobacco Use Indicators
#'
#' Calculates prevalence of tobacco use from a survey design object.
#' Computes proportions of current and daily tobacco use, stratified by
#' sex and age group where available.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains proportion
#'   estimates (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `current_tobacco_total`: current tobacco use, overall
#'   - `current_tobacco_by_sex`: current tobacco use, by sex
#'   - `current_tobacco_by_age`: current tobacco use, by age group
#'   - `daily_tobacco_total`: daily tobacco use, overall
#'   - `daily_tobacco_by_sex`: daily tobacco use, by sex
#'   - `daily_tobacco_by_age`: daily tobacco use, by age group
#'   (if the corresponding variables exist in design)
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

  vars <- c("current_tobacco", "daily_tobacco")
  vars <- vars[vars %in% names(design$variables)]

  for (v in vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  return(results)
}

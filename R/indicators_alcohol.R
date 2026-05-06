#' Compute Alcohol Use Indicators
#'
#' Calculates prevalence of alcohol use from a survey design object.
#' Computes proportions of current alcohol use and heavy episodic drinking,
#' stratified by sex and age group where available.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains proportion
#'   estimates (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `current_alcohol_total`: current alcohol use, overall
#'   - `current_alcohol_by_sex`: current alcohol use, by sex
#'   - `current_alcohol_by_age`: current alcohol use, by age group
#'   - `heavy_episodic_total`: heavy episodic drinking, overall
#'   - `heavy_episodic_by_sex`: heavy episodic drinking, by sex
#'   - `heavy_episodic_by_age`: heavy episodic drinking, by age group
#'   (if the corresponding variables exist in design)
#'
#' @seealso [compute_all_indicators()]
#'
#' @export
#'
#' @examples
#' \donttest{
#'   test_data <- generate_test_data(n = 500, seed = 42)
#'   cols <- detect_steps_columns(test_data)
#'   clean <- clean_steps_data(test_data, cols)
#'   design <- setup_survey_design(clean)
#'   alcohol_results <- compute_alcohol_indicators(design)
#' }
compute_alcohol_indicators <- function(design) {
  message("  Computing alcohol indicators...")
  results <- list()

  vars <- c("current_alcohol", "heavy_episodic")
  vars <- vars[vars %in% names(design$variables)]

  for (v in vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  return(results)
}

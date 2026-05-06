#' Compute Biochemical Indicators
#'
#' Calculates prevalence of raised glucose, diabetes, impaired glucose
#' tolerance, and raised cholesterol, plus mean fasting glucose and total
#' cholesterol from a survey design object.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains estimates
#'   (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `raised_glucose_total`: raised fasting glucose, overall
#'   - `raised_glucose_by_sex`: raised fasting glucose, by sex
#'   - `raised_glucose_by_age`: raised fasting glucose, by age group
#'   - `diabetes_total`: diabetes, overall
#'   - `diabetes_by_sex`: diabetes, by sex
#'   - `diabetes_by_age`: diabetes, by age group
#'   - `impaired_glucose_total`: impaired fasting glucose, overall
#'   - `impaired_glucose_by_sex`: impaired fasting glucose, by sex
#'   - `impaired_glucose_by_age`: impaired fasting glucose, by age group
#'   - `raised_chol_total`: raised total cholesterol, overall
#'   - `raised_chol_by_sex`: raised total cholesterol, by sex
#'   - `raised_chol_by_age`: raised total cholesterol, by age group
#'   - `fasting_glucose_mean_total`: mean fasting glucose, overall
#'   - `fasting_glucose_mean_by_sex`: mean fasting glucose, by sex
#'   - `total_chol_mean_total`: mean total cholesterol, overall
#'   - `total_chol_mean_by_sex`: mean total cholesterol, by sex
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
#'   biochemical_results <- compute_biochemical_indicators(design)
#' }
compute_biochemical_indicators <- function(design) {
  message("  Computing biochemical indicators...")
  results <- list()

  bin_vars <- c("raised_glucose", "diabetes", "impaired_glucose", "raised_chol")
  bin_vars <- bin_vars[bin_vars %in% names(design$variables)]
  for (v in bin_vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  cont_vars <- c("fasting_glucose", "total_chol")
  cont_vars <- cont_vars[cont_vars %in% names(design$variables)]
  for (v in cont_vars) {
    f <- as.formula(paste0("~", v))
    results[[paste0(v, "_mean_total")]]  <- svymn(f, design)
    results[[paste0(v, "_mean_by_sex")]] <- svymn(f, design, by = ~sex)
  }

  return(results)
}

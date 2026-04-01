#' Compute Blood Pressure Indicators
#'
#' Calculates prevalence of raised blood pressure and mean systolic and
#' diastolic blood pressure from a survey design object.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains estimates
#'   (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `raised_bp_total`: raised blood pressure, overall
#'   - `raised_bp_by_sex`: raised blood pressure, by sex
#'   - `raised_bp_by_age`: raised blood pressure, by age group
#'   - `mean_sbp_mean_total`: mean systolic BP, overall
#'   - `mean_sbp_mean_by_sex`: mean systolic BP, by sex
#'   - `mean_sbp_mean_by_age`: mean systolic BP, by age group
#'   - `mean_dbp_mean_total`: mean diastolic BP, overall
#'   - `mean_dbp_mean_by_sex`: mean diastolic BP, by sex
#'   - `mean_dbp_mean_by_age`: mean diastolic BP, by age group
#'   (if the corresponding variables exist in design)
#'
#' @seealso [compute_all_indicators()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   design <- setup_survey_design(data = survey_data)
#'   bp_results <- compute_bp_indicators(design)
#' }
compute_bp_indicators <- function(design) {
  message("  Computing blood pressure indicators...")
  results <- list()

  bin_vars <- c("raised_bp")
  bin_vars <- bin_vars[bin_vars %in% names(design$variables)]
  for (v in bin_vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  cont_vars <- c("mean_sbp", "mean_dbp")
  cont_vars <- cont_vars[cont_vars %in% names(design$variables)]
  for (v in cont_vars) {
    f <- as.formula(paste0("~", v))
    results[[paste0(v, "_mean_total")]]  <- svymn(f, design)
    results[[paste0(v, "_mean_by_sex")]] <- svymn(f, design, by = ~sex)
    results[[paste0(v, "_mean_by_age")]] <- svymn(f, design, by = ~age_group)
  }

  return(results)
}

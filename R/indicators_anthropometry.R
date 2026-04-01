#' Compute Anthropometry Indicators
#'
#' Calculates prevalence of overweight, obesity, and central obesity, plus
#' mean BMI and waist circumference, from a survey design object.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains estimates
#'   (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `overweight_obese_total`: overweight or obese (BMI >=25), overall
#'   - `overweight_obese_by_sex`: overweight or obese, by sex
#'   - `overweight_obese_by_age`: overweight or obese, by age group
#'   - `obese_total`: obese (BMI >=30), overall
#'   - `obese_by_sex`: obese, by sex
#'   - `obese_by_age`: obese, by age group
#'   - `central_obesity_total`: central obesity, overall
#'   - `central_obesity_by_sex`: central obesity, by sex
#'   - `central_obesity_by_age`: central obesity, by age group
#'   - `bmi_mean_total`: mean BMI, overall
#'   - `bmi_mean_by_sex`: mean BMI, by sex
#'   - `waist_cm_mean_total`: mean waist circumference, overall
#'   - `waist_cm_mean_by_sex`: mean waist circumference, by sex
#'   (if the corresponding variables exist in design)
#'
#' @seealso [compute_all_indicators()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   design <- setup_survey_design(data = survey_data)
#'   anthropometry_results <- compute_anthropometry_indicators(design)
#' }
compute_anthropometry_indicators <- function(design) {
  message("  Computing anthropometry indicators...")
  results <- list()

  bin_vars <- c("overweight_obese", "obese", "central_obesity")
  bin_vars <- bin_vars[bin_vars %in% names(design$variables)]
  for (v in bin_vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  cont_vars <- c("bmi", "waist_cm")
  cont_vars <- cont_vars[cont_vars %in% names(design$variables)]
  for (v in cont_vars) {
    f <- as.formula(paste0("~", v))
    results[[paste0(v, "_mean_total")]]  <- svymn(f, design)
    results[[paste0(v, "_mean_by_sex")]] <- svymn(f, design, by = ~sex)
  }

  return(results)
}

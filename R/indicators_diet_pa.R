#' Compute Diet and Physical Activity Indicators
#'
#' Calculates prevalence of insufficient physical activity and low fruit &
#' vegetable intake, plus mean metabolic equivalent (MET) values, from a
#' survey design object.
#'
#' @param design A survey design object from [setup_survey_design()].
#'
#' @return A named list of survey estimates. Each element contains estimates
#'   (as tibble with columns: estimate, lower, upper, etc.) for:
#'   - `insufficient_pa_total`: insufficient physical activity, overall
#'   - `insufficient_pa_by_sex`: insufficient physical activity, by sex
#'   - `insufficient_pa_by_age`: insufficient physical activity, by age group
#'   - `low_fruit_veg_total`: low fruit & vegetable intake, overall
#'   - `low_fruit_veg_by_sex`: low fruit & vegetable intake, by sex
#'   - `low_fruit_veg_by_age`: low fruit & vegetable intake, by age group
#'   - `met_mean_total`: mean MET (if available)
#'   - `met_mean_by_sex`: mean MET by sex (if available)
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
#'   diet_pa_results <- compute_diet_pa_indicators(design)
#' }
compute_diet_pa_indicators <- function(design) {
  message("  Computing diet & physical activity indicators...")
  results <- list()

  bin_vars <- c("insufficient_pa", "low_fruit_veg")
  bin_vars <- bin_vars[bin_vars %in% names(design$variables)]
  for (v in bin_vars) {
    f <- as.formula(paste0("~as.numeric(", v, ")"))
    results[[paste0(v, "_total")]]  <- svyprop(f, design)
    results[[paste0(v, "_by_sex")]] <- svyprop(f, design, by = ~sex)
    results[[paste0(v, "_by_age")]] <- svyprop(f, design, by = ~age_group)
  }

  if ("met_total" %in% names(design$variables)) {
    results$met_mean_total  <- svymn(~met_total, design)
    results$met_mean_by_sex <- svymn(~met_total, design, by = ~sex)
  }

  return(results)
}

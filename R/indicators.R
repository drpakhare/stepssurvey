#' Compute All STEPS Indicators
#'
#' Runs all indicator modules (tobacco, alcohol, diet & physical activity,
#' anthropometry, blood pressure, and biochemical), using the appropriate
#' step-specific survey design for each domain per WHO STEPS methodology:
#' - Step 1 (behavioural): tobacco, alcohol, diet & physical activity
#' - Step 2 (physical):    anthropometry, blood pressure
#' - Step 3 (biochemical): biochemical measures
#'
#' @param design A `steps_designs` list from [setup_survey_design()]
#'   (with elements `$step1`, `$step2`, `$step3`), or a single
#'   [survey::svydesign] object for backward compatibility.
#'
#' @return A list with two elements:
#'   - `results`: a named list containing indicator results grouped by domain
#'     (tobacco, alcohol, diet_pa, anthropometry, blood_pressure, biochemical)
#'   - `key_indicators`: a tibble with columns domain, indicator, estimate,
#'     lower, and upper, summarising headline estimates across all domains
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   designs <- setup_survey_design(data = survey_data)
#'   all_indicators <- compute_all_indicators(designs)
#'   all_indicators$results
#'   all_indicators$key_indicators
#' }
compute_all_indicators <- function(design) {
  # Support both old (single design) and new (steps_designs list) format

  if (inherits(design, "steps_designs")) {
    d1 <- design$step1
    d2 <- design$step2
    d3 <- design$step3
  } else {
    # Backward compatibility: single design for all steps
    d1 <- design
    d2 <- design
    d3 <- design
  }

  # Step 1 — Behavioural indicators
  message("Computing tobacco indicators...")
  tobacco <- compute_tobacco_indicators(d1)
  message("Computing alcohol indicators...")
  alcohol <- compute_alcohol_indicators(d1)
  message("Computing diet & physical activity indicators...")
  diet_pa <- compute_diet_pa_indicators(d1)

  # Step 2 — Physical measurement indicators
  message("Computing anthropometry indicators...")
  anthropometry <- compute_anthropometry_indicators(d2)
  message("Computing blood pressure indicators...")
  blood_pressure <- compute_bp_indicators(d2)

  # Step 3 — Biochemical indicators
  message("Computing biochemical indicators...")
  biochemical <- compute_biochemical_indicators(d3)

  indicators <- list(
    tobacco        = tobacco,
    alcohol        = alcohol,
    diet_pa        = diet_pa,
    anthropometry  = anthropometry,
    blood_pressure = blood_pressure,
    biochemical    = biochemical
  )

  # Prefer current_tobacco_any (smoking + smokeless) as headline; fall back

  tob_key <- if (!is.null(indicators$tobacco[["current_tobacco_any_total"]])) {
    "current_tobacco_any_total"
  } else {
    "current_tobacco_total"
  }

  key_indicators <- dplyr::bind_rows(
    extract_key(indicators$tobacco,       "Tobacco",       tob_key,                    "Current tobacco use"),
    extract_key(indicators$alcohol,       "Alcohol",       "current_alcohol_total",    "Current alcohol use"),
    extract_key(indicators$alcohol,       "Alcohol",       "heavy_episodic_total",     "Heavy episodic drinking"),
    extract_key(indicators$diet_pa,       "Physical Activity", "insufficient_pa_total", "Insufficient physical activity"),
    extract_key(indicators$diet_pa,       "Diet",          "low_fruit_veg_total",      "Low fruit & vegetable intake (<5 servings/day)"),
    extract_key(indicators$anthropometry, "Obesity",       "overweight_obese_total",   "Overweight or obese (BMI >=25)"),
    extract_key(indicators$anthropometry, "Obesity",       "obese_total",              "Obese (BMI >=30)"),
    extract_key(indicators$blood_pressure,"Blood Pressure","raised_bp_total",          "Raised blood pressure"),
    extract_key(indicators$biochemical,   "Diabetes",      "raised_glucose_total",     "Raised fasting blood glucose"),
    extract_key(indicators$biochemical,   "Cholesterol",   "raised_chol_total",        "Raised total cholesterol")
  )

  message(glue::glue("\u2713 Computed {nrow(key_indicators)} key indicators across all domains."))

  return(list(results = indicators, key_indicators = key_indicators))
}


#' Extract Key Indicator for Fact Sheet
#'
#' Internal helper function to extract a single key indicator from an
#' indicator list and format it as a tibble row with domain and label.
#'
#' @param ind_list A list of indicator results (e.g., from compute_tobacco_indicators)
#' @param domain The domain name (e.g., "Tobacco", "Alcohol")
#' @param key The result name to extract (e.g., "current_tobacco_total")
#' @param label The display label for the indicator
#'
#' @return A tibble row with columns: domain, indicator, estimate, lower, upper,
#'   or NULL if the key does not exist in ind_list
#'
#' @keywords internal
extract_key <- function(ind_list, domain, key, label) {
  if (!is.null(ind_list[[key]])) {
    row <- ind_list[[key]] |>
      head(1) |>
      dplyr::mutate(domain = domain, indicator = label) |>
      dplyr::select(domain, indicator, estimate, lower, upper)
    return(row)
  }
  return(NULL)
}

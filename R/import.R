#' Import raw STEPS survey data
#'
#' Reads a raw STEPS data file (CSV, Excel, Stata, or SPSS) and
#' standardises column names to lowercase with underscores.
#'
#' @param path Character. Path to the data file.
#' @return A data frame with cleaned column names.
#' @export
#' @examples
#' \dontrun{
#' raw <- import_steps_data("data/raw/steps_data.csv")
#' }
import_steps_data <- function(path) {
  if (!file.exists(path)) {
    stop(glue::glue("Data file not found: {path}"), call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  data <- switch(ext,
    "csv"  = readr::read_csv(path, show_col_types = FALSE),
    "xlsx" = readxl::read_excel(path),
    "xls"  = readxl::read_excel(path),
    "dta"  = haven::read_dta(path),
    "sav"  = haven::read_spss(path),
    stop(glue::glue("Unsupported file type: .{ext}. Use CSV, Excel, Stata, or SPSS."),
         call. = FALSE)
  )

  data <- data |> janitor::clean_names()
  message(glue::glue("\u2713 Loaded {nrow(data)} rows \u00d7 {ncol(data)} columns from {basename(path)}"))
  return(data)
}

#' Detect a STEPS column by alias
#'
#' Tries to find a column in the data matching one of several candidate
#' names (case-insensitive).
#'
#' @param data A data frame.
#' @param candidates Character vector of possible column names.
#' @param label Optional label for progress messages.
#' @return The matched column name (character) or `NULL`.
#' @export
detect_col <- function(data, candidates, label = NULL) {
  found <- intersect(tolower(candidates), tolower(names(data)))
  if (length(found) == 0) {
    if (!is.null(label)) message(glue::glue("  \u26a0 Could not auto-detect column for: {label}"))
    return(NULL)
  }
  match <- names(data)[tolower(names(data)) %in% found][1]
  if (!is.null(label)) message(glue::glue("  \u2713 {label}: '{match}'"))
  return(match)
}

#' Auto-detect all standard STEPS columns
#'
#' Scans a data frame for standard WHO STEPS variable names across
#' versions 3.1 and 3.2.
#'
#' @param data A data frame (typically from [import_steps_data()]).
#' @return A named list of detected column names (or `NULL` for missing).
#' @export
detect_steps_columns <- function(data) {
  message("  Detecting STEPS columns...")

  cols <- list(
    # Demographics
    age       = detect_col(data, c("age", "c1", "age_years"), "Age"),
    sex       = detect_col(data, c("sex", "gender", "c2", "c3"), "Sex"),
    weight_var = detect_col(data, c("wt_final", "weight", "wt", "finalwt", "sampleweight"), "Survey weight"),
    strata    = detect_col(data, c("stratum", "strata", "stratum_id"), "Stratum"),
    psu       = detect_col(data, c("psu", "cluster", "psu_id", "clusterid"), "PSU"),

    # Step 1 - Tobacco
    tobacco_current = detect_col(data, c("t1", "smk_current", "current_smoker", "tobacco_current"), "Current tobacco use"),
    tobacco_daily   = detect_col(data, c("t2", "smk_daily", "daily_smoker"), "Daily tobacco use"),

    # Step 1 - Alcohol
    alcohol_current = detect_col(data, c("a1", "alc_current", "current_drinker"), "Current alcohol use"),
    heavy_episode   = detect_col(data, c("a5", "heavy_drinking", "binge"), "Heavy episodic drinking"),

    # Step 1 - Physical activity
    pa_low          = detect_col(data, c("p1", "pa_low", "insufficient_pa"), "Insufficient physical activity"),
    met_total       = detect_col(data, c("met_total", "total_met", "pa_met"), "Total MET minutes"),

    # Step 1 - Diet
    fruit_days      = detect_col(data, c("d1", "fruit_days", "days_fruit"), "Fruit days/week"),
    veg_days        = detect_col(data, c("d3", "veg_days", "days_veg"), "Vegetable days/week"),
    fruit_servings  = detect_col(data, c("d2", "fruit_serv", "servings_fruit"), "Fruit servings/day"),
    veg_servings    = detect_col(data, c("d4", "veg_serv", "servings_veg"), "Vegetable servings/day"),

    # Step 2 - Anthropometry
    height   = detect_col(data, c("m1", "height", "height_cm"), "Height (cm)"),
    weight   = detect_col(data, c("m2", "weight_kg", "bodyweight"), "Weight (kg)"),
    waist    = detect_col(data, c("m3", "waist", "waist_cm"), "Waist circumference (cm)"),

    # Step 2 - Blood pressure
    sbp1 = detect_col(data, c("b1", "sbp1", "sys1", "systolic1"), "SBP reading 1"),
    sbp2 = detect_col(data, c("b3", "sbp2", "sys2", "systolic2"), "SBP reading 2"),
    sbp3 = detect_col(data, c("b5", "sbp3", "sys3", "systolic3"), "SBP reading 3"),
    dbp1 = detect_col(data, c("b2", "dbp1", "dia1", "diastolic1"), "DBP reading 1"),
    dbp2 = detect_col(data, c("b4", "dbp2", "dia2", "diastolic2"), "DBP reading 2"),
    dbp3 = detect_col(data, c("b6", "dbp3", "dia3", "diastolic3"), "DBP reading 3"),
    bp_meds = detect_col(data, c("b7", "bp_meds", "antihypertensive", "htn_meds"), "BP medications"),

    # Step 3 - Biochemical
    fasting_glucose = detect_col(data, c("c1_mmol", "glucose_fasting", "fasting_glucose", "fbg"), "Fasting blood glucose"),
    random_glucose  = detect_col(data, c("c1_rand", "glucose_random", "rbg"), "Random blood glucose"),
    fasting_status  = detect_col(data, c("fasting", "fasting_status", "c1_fast"), "Fasting status"),
    dm_meds         = detect_col(data, c("c5", "dm_meds", "diabetes_meds", "insulin"), "Diabetes medications"),
    total_chol      = detect_col(data, c("c6", "cholesterol", "total_chol", "tc_mmol"), "Total cholesterol"),
    chol_meds       = detect_col(data, c("c10", "chol_meds", "statin"), "Cholesterol medications")
  )

  n_found <- sum(!sapply(cols, is.null))
  message(glue::glue("  \u2192 {n_found}/{length(cols)} columns detected automatically"))
  return(cols)
}

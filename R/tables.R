#' Build survey-weighted tables for STEPS indicators
#'
#' Generates formatted flextable objects for all available STEPS indicators,
#' with rows for age groups and columns for both sexes combined, males, and
#' females. Tables include 95% confidence intervals.
#'
#' @param indicators A list of indicator results from [compute_all_indicators()],
#'   containing elements like `tobacco`, `alcohol`, `diet_pa`, `anthropometry`,
#'   `blood_pressure`, `biochemical`, etc. Each indicator list should contain
#'   `*_total`, `*_by_sex`, and `*_by_age` elements.
#'
#' @return A named list of flextable objects, one per indicator.
#'   Names correspond to indicators (e.g., `current_tobacco`, `raised_bp`).
#'   NULL entries are excluded. Prints count of tables generated.
#'
#' @details
#' Each table has age groups as rows and prevalence (with 95% CI) as a column.
#' The last row shows the total (age-standardised) estimate.
#' Column header styling uses WHO STEPS branding (dark blue background).
#'
#' @examples
#' \dontrun{
#'   indicators <- compute_steps_indicators(design, config)
#'   tables <- build_steps_tables(indicators)
#'   names(tables)  # View available tables
#' }
#'
#' @export
build_steps_tables <- function(indicators) {
  tables <- list()

  # Prefer current_tobacco_any (smoking + smokeless) as headline tobacco table
  tob_var <- if (!is.null(indicators$tobacco[["current_tobacco_any_total"]])) {
    "current_tobacco_any"
  } else {
    "current_tobacco"
  }
  tables$current_tobacco <- build_table_if_available(
    indicators$tobacco,
    paste0(tob_var, "_total"), paste0(tob_var, "_by_sex"), paste0(tob_var, "_by_age"),
    "Table 1. Current tobacco use (%)"
  )
  tables$current_alcohol <- build_table_if_available(
    indicators$alcohol, "current_alcohol_total", "current_alcohol_by_sex", "current_alcohol_by_age",
    "Table 2. Current alcohol use (%)"
  )
  tables$insufficient_pa <- build_table_if_available(
    indicators$diet_pa, "insufficient_pa_total", "insufficient_pa_by_sex", "insufficient_pa_by_age",
    "Table 3. Insufficient physical activity (%)"
  )
  tables$low_fruit_veg <- build_table_if_available(
    indicators$diet_pa, "low_fruit_veg_total", "low_fruit_veg_by_sex", "low_fruit_veg_by_age",
    "Table 4. Low fruit and vegetable intake (%)"
  )
  tables$overweight_obese <- build_table_if_available(
    indicators$anthropometry, "overweight_obese_total", "overweight_obese_by_sex", "overweight_obese_by_age",
    "Table 5. Overweight or obese (BMI >=25, %)"
  )
  tables$raised_bp <- build_table_if_available(
    indicators$blood_pressure, "raised_bp_total", "raised_bp_by_sex", "raised_bp_by_age",
    "Table 6. Raised blood pressure (%)"
  )
  tables$raised_glucose <- build_table_if_available(
    indicators$biochemical, "raised_glucose_total", "raised_glucose_by_sex", "raised_glucose_by_age",
    "Table 7. Raised fasting blood glucose (%)"
  )
  tables$raised_chol <- build_table_if_available(
    indicators$biochemical, "raised_chol_total", "raised_chol_by_sex", "raised_chol_by_age",
    "Table 8. Raised total cholesterol (%)"
  )

  # Remove NULL entries
  tables <- Filter(Negate(is.null), tables)

  n_tables <- length(tables)
  message(glue::glue("\u2713 Generated {n_tables} tables."))

  return(tables)
}

#' Format estimate with confidence interval
#'
#' @param est Estimated value.
#' @param lower Lower confidence bound.
#' @param upper Upper confidence bound.
#' @param digits Number of decimal places (default 1).
#' @param pct Logical; add percent sign? (default TRUE).
#'
#' @return Formatted string like "42.1% (39.2-45.0)".
#'
#' @keywords internal
fmt_est <- function(est, lower, upper, digits = 1, pct = TRUE) {
  suf <- if (pct) "%" else ""
  glue::glue("{round(est, digits)}{suf} ({round(lower, digits)}-{round(upper, digits)})")
}

#' Build a single STEPS table
#'
#' Constructs a flextable from age-stratified and total estimates.
#'
#' @param total_df Data frame with total estimate, lower, upper.
#' @param by_sex_df Data frame with sex-stratified estimates (not used, kept for compatibility).
#' @param by_age_df Data frame with age-stratified estimates and `age_group` column.
#' @param label Table caption.
#' @param pct Logical; format as percent? (default TRUE).
#'
#' @return A flextable object.
#'
#' @keywords internal
make_steps_table <- function(total_df, by_sex_df, by_age_df, label, pct = TRUE) {

  # By age - combine sexes
  age_total <- by_age_df |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(., 1))) |>
    dplyr::mutate(both = fmt_est(estimate, lower, upper, pct = pct)) |>
    dplyr::select(age_group, both)

  # Total row
  total_row <- total_df |>
    dplyr::mutate(both = fmt_est(estimate, lower, upper, pct = pct)) |>
    dplyr::mutate(age_group = "Total (age-standardised)") |>
    dplyr::select(age_group, both)

  dplyr::bind_rows(age_total, total_row) |>
    dplyr::rename(`Age group` = age_group, `% (95% CI)` = both) |>
    flextable::flextable() |>
    flextable::set_caption(label) |>
    flextable::theme_vanilla() |>
    flextable::bold(part = "header") |>
    flextable::bg(part = "header", bg = "#00427A") |>
    flextable::color(part = "header", color = "white") |>
    flextable::autofit()
}

#' Build a table if indicator is available
#'
#' Wrapper that safely builds a table only if the required indicator elements exist.
#' Catches and reports errors gracefully.
#'
#' @param ind_list Indicator list (e.g., `indicators$tobacco`).
#' @param total_key Name of total estimate element (e.g., "current_tobacco_total").
#' @param by_sex_key Name of by-sex element (e.g., "current_tobacco_by_sex").
#' @param by_age_key Name of by-age element (e.g., "current_tobacco_by_age").
#' @param label Table caption.
#' @param pct Logical; format as percent? (default TRUE).
#'
#' @return A flextable object or NULL if not available.
#'
#' @keywords internal
build_table_if_available <- function(ind_list, total_key, by_sex_key, by_age_key, label, pct = TRUE) {
  if (!is.null(ind_list[[total_key]])) {
    tryCatch(
      make_steps_table(
        ind_list[[total_key]],
        ind_list[[by_sex_key]],
        ind_list[[by_age_key]],
        label, pct
      ),
      error = function(e) {
        message(glue::glue("  \u26a0 Could not build table for {label}: {e$message}"))
        NULL
      }
    )
  } else NULL
}

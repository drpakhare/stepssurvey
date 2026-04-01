#' Recode a yes/no variable to logical
#'
#' Converts various representations of yes/no (numeric, text, case-insensitive)
#' to logical TRUE/FALSE values.
#'
#' @param x A vector of yes/no values (numeric or character).
#' @return A logical vector, with NA for unrecognized values.
#' @keywords internal
#' @export
#' @examples
#' recode_yn(c(1, 2, "yes", "no", NA))
recode_yn <- function(x) {
  dplyr::case_when(
    x %in% c(1, "1", "yes", "Yes", "YES", "y", "Y") ~ TRUE,
    x %in% c(0, "0", 2, "2", "no", "No", "NO", "n", "N") ~ FALSE,
    TRUE ~ NA
  )
}

#' Clean and recode WHO STEPS data
#'
#' Processes raw STEPS survey data: renames columns, coerces types,
#' derives standard indicators, handles missing values, and applies
#' plausibility checks.
#'
#' @param data A data frame (typically from [import_steps_data()]).
#' @param cols A named list of column names, as returned by [detect_steps_columns()].
#' @param age_min Minimum age for inclusion (default 18).
#' @param age_max Maximum age for inclusion (default 69).
#'
#' @return A data frame with standardised and derived variables, ready for
#' survey design setup.
#'
#' @details
#' The function performs the following transformations:
#' - Renames columns to standard names (age, sex, wt_final, etc.)
#' - Converts numeric strings to appropriate types
#' - Restricts age to \code{[age_min, age_max]}
#' - Creates WHO standard age groups (18-24, 25-34, etc.)
#' - Harmonises sex coding to Male/Female
#' - Derives body mass index (BMI) and categories
#' - Averages blood pressure readings (last 2 of 3)
#' - Recodes yes/no variables to logical
#' - Creates derived risk indicators (raised BP, diabetes, etc.)
#' - Applies plausibility checks to measurements
#' - Drops records with missing age or sex
#'
#' @export
clean_steps_data <- function(data, cols, age_min = 18, age_max = 69) {

  d <- data

  # -- Age & Sex --------------------------------------------------------------
  if (!is.null(cols$age)) {
    d <- d |>
      dplyr::rename(age = dplyr::all_of(cols$age)) |>
      dplyr::mutate(
        age = as.numeric(age),
        # Restrict to survey age range
        age = dplyr::if_else(age < age_min | age > age_max, NA_real_, age),
        # WHO standard age groups for STEPS
        age_group = cut(age,
          breaks = c(18, 25, 35, 45, 55, 65, Inf),
          labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
          right  = FALSE
        )
      )
  }

  if (!is.null(cols$sex)) {
    d <- d |>
      dplyr::rename(sex = dplyr::all_of(cols$sex)) |>
      dplyr::mutate(
        # Harmonise sex coding: 1=Male, 2=Female (WHO STEPS standard)
        sex = dplyr::case_when(
          sex %in% c(1, "1", "m", "M", "male", "Male")   ~ "Male",
          sex %in% c(2, "2", "f", "F", "female", "Female") ~ "Female",
          TRUE ~ NA_character_
        ),
        sex = factor(sex, levels = c("Male", "Female"))
      )
  }

  # -- Sampling design variables ----------------------------------------------
  if (!is.null(cols$weight_var)) {
    d <- d |> dplyr::rename(wt_final = dplyr::all_of(cols$weight_var)) |>
      dplyr::mutate(wt_final = as.numeric(wt_final))
  } else {
    # If no weight, use equal weight of 1 (unweighted analysis)
    message("  \u26a0 No weight variable found - using equal weights (wt_final = 1)")
    d <- d |> dplyr::mutate(wt_final = 1)
  }

  if (!is.null(cols$strata)) d <- d |> dplyr::rename(stratum = dplyr::all_of(cols$strata))
  if (!is.null(cols$psu))    d <- d |> dplyr::rename(psu     = dplyr::all_of(cols$psu))

  # -- Tobacco ---------------------------------------------------------------
  if (!is.null(cols$tobacco_current)) {
    d <- d |>
      dplyr::rename(tobacco_current_raw = dplyr::all_of(cols$tobacco_current)) |>
      dplyr::mutate(current_tobacco = recode_yn(tobacco_current_raw))
  }
  if (!is.null(cols$tobacco_daily)) {
    d <- d |>
      dplyr::rename(tobacco_daily_raw = dplyr::all_of(cols$tobacco_daily)) |>
      dplyr::mutate(daily_tobacco = recode_yn(tobacco_daily_raw))
  }

  # -- Alcohol ---------------------------------------------------------------
  if (!is.null(cols$alcohol_current)) {
    d <- d |>
      dplyr::rename(alcohol_current_raw = dplyr::all_of(cols$alcohol_current)) |>
      dplyr::mutate(current_alcohol = recode_yn(alcohol_current_raw))
  }
  if (!is.null(cols$heavy_episode)) {
    d <- d |>
      dplyr::rename(heavy_episode_raw = dplyr::all_of(cols$heavy_episode)) |>
      dplyr::mutate(heavy_episodic = recode_yn(heavy_episode_raw))
  }

  # -- Physical activity (MET-based) -----------------------------------------
  if (!is.null(cols$met_total)) {
    d <- d |>
      dplyr::rename(met_total = dplyr::all_of(cols$met_total)) |>
      dplyr::mutate(
        met_total            = as.numeric(met_total),
        insufficient_pa      = met_total < 600,    # WHO threshold: <600 MET-min/week
        low_pa               = met_total < 3000,   # Moderate activity threshold
        pa_category = dplyr::case_when(
          met_total >= 3000 ~ "High",
          met_total >= 600  ~ "Moderate",
          TRUE              ~ "Low"
        )
      )
  }

  # -- Diet ------------------------------------------------------------------
  if (!is.null(cols$fruit_days) & !is.null(cols$fruit_servings)) {
    d <- d |>
      dplyr::rename(fruit_days = dplyr::all_of(cols$fruit_days),
             fruit_serv = dplyr::all_of(cols$fruit_servings)) |>
      dplyr::mutate(
        fruit_days = as.numeric(fruit_days),
        fruit_serv = as.numeric(fruit_serv),
        avg_fruit_servings = (fruit_days * fruit_serv) / 7
      )
  }
  if (!is.null(cols$veg_days) & !is.null(cols$veg_servings)) {
    d <- d |>
      dplyr::rename(veg_days = dplyr::all_of(cols$veg_days),
             veg_serv = dplyr::all_of(cols$veg_servings)) |>
      dplyr::mutate(
        veg_days = as.numeric(veg_days),
        veg_serv = as.numeric(veg_serv),
        avg_veg_servings = (veg_days * veg_serv) / 7
      )
  }
  # Combined <5 servings fruit+veg per day
  if (all(c("avg_fruit_servings", "avg_veg_servings") %in% names(d))) {
    d <- d |>
      dplyr::mutate(low_fruit_veg = (avg_fruit_servings + avg_veg_servings) < 5)
  }

  # -- Anthropometry ---------------------------------------------------------
  if (!is.null(cols$height) & !is.null(cols$weight)) {
    d <- d |>
      dplyr::rename(height_cm = dplyr::all_of(cols$height),
             weight_kg = dplyr::all_of(cols$weight)) |>
      dplyr::mutate(
        height_cm = as.numeric(height_cm),
        weight_kg = as.numeric(weight_kg),
        # Plausibility checks
        height_cm = dplyr::if_else(height_cm < 100 | height_cm > 250, NA_real_, height_cm),
        weight_kg = dplyr::if_else(weight_kg < 20  | weight_kg > 300, NA_real_, weight_kg),
        bmi       = weight_kg / (height_cm / 100)^2,
        bmi_category = dplyr::case_when(
          bmi < 18.5              ~ "Underweight",
          bmi < 25.0              ~ "Normal",
          bmi < 30.0              ~ "Overweight",
          bmi >= 30.0             ~ "Obese",
          TRUE                    ~ NA_character_
        ),
        overweight_obese = bmi >= 25,
        obese            = bmi >= 30
      )
  }

  if (!is.null(cols$waist)) {
    d <- d |>
      dplyr::rename(waist_cm = dplyr::all_of(cols$waist)) |>
      dplyr::mutate(
        waist_cm = as.numeric(waist_cm),
        waist_cm = dplyr::if_else(waist_cm < 40 | waist_cm > 200, NA_real_, waist_cm),
        # WHO central obesity thresholds
        central_obesity = dplyr::case_when(
          sex == "Male"   & waist_cm >= 102 ~ TRUE,
          sex == "Female" & waist_cm >= 88  ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }

  # -- Blood pressure --------------------------------------------------------
  # Average last 2 of 3 readings (WHO STEPS protocol)
  bp_cols_sbp <- c(cols$sbp1, cols$sbp2, cols$sbp3)
  bp_cols_dbp <- c(cols$dbp1, cols$dbp2, cols$dbp3)
  bp_cols_sbp <- bp_cols_sbp[!sapply(bp_cols_sbp, is.null)]
  bp_cols_dbp <- bp_cols_dbp[!sapply(bp_cols_dbp, is.null)]

  if (length(bp_cols_sbp) >= 2) {
    d <- d |>
      dplyr::rename_with(~ paste0("sbp", seq_along(bp_cols_sbp)), dplyr::all_of(unlist(bp_cols_sbp))) |>
      dplyr::rename_with(~ paste0("dbp", seq_along(bp_cols_dbp)), dplyr::all_of(unlist(bp_cols_dbp)))

    if (length(bp_cols_sbp) == 3) {
      d <- d |> dplyr::mutate(
        mean_sbp = (as.numeric(sbp2) + as.numeric(sbp3)) / 2,
        mean_dbp = (as.numeric(dbp2) + as.numeric(dbp3)) / 2
      )
    } else {
      d <- d |> dplyr::mutate(
        mean_sbp = as.numeric(sbp1),
        mean_dbp = as.numeric(dbp1)
      )
    }

    # Plausibility
    d <- d |> dplyr::mutate(
      mean_sbp = dplyr::if_else(mean_sbp < 60 | mean_sbp > 300, NA_real_, mean_sbp),
      mean_dbp = dplyr::if_else(mean_dbp < 30 | mean_dbp > 200, NA_real_, mean_dbp)
    )
  }

  if (!is.null(cols$bp_meds)) {
    d <- d |>
      dplyr::rename(bp_meds_raw = dplyr::all_of(cols$bp_meds)) |>
      dplyr::mutate(on_bp_meds = recode_yn(bp_meds_raw))
  }

  if (all(c("mean_sbp", "mean_dbp") %in% names(d))) {
    d <- d |> dplyr::mutate(
      raised_bp = (mean_sbp >= 140 | mean_dbp >= 90) | (dplyr::if_else(!is.na(on_bp_meds), on_bp_meds, FALSE)),
      bp_stage = dplyr::case_when(
        mean_sbp >= 180 | mean_dbp >= 110 ~ "Stage 3",
        mean_sbp >= 160 | mean_dbp >= 100 ~ "Stage 2",
        mean_sbp >= 140 | mean_dbp >= 90  ~ "Stage 1",
        mean_sbp >= 130 | mean_dbp >= 80  ~ "Elevated",
        TRUE                               ~ "Normal"
      )
    )
  }

  # -- Blood glucose ---------------------------------------------------------
  if (!is.null(cols$fasting_glucose)) {
    d <- d |>
      dplyr::rename(fasting_glucose = dplyr::all_of(cols$fasting_glucose)) |>
      dplyr::mutate(
        fasting_glucose = as.numeric(fasting_glucose),
        fasting_glucose = dplyr::if_else(fasting_glucose < 1 | fasting_glucose > 40, NA_real_, fasting_glucose)
      )
  }
  if (!is.null(cols$dm_meds)) {
    d <- d |>
      dplyr::rename(dm_meds_raw = dplyr::all_of(cols$dm_meds)) |>
      dplyr::mutate(on_dm_meds = recode_yn(dm_meds_raw))
  }
  if ("fasting_glucose" %in% names(d)) {
    d <- d |> dplyr::mutate(
      raised_glucose = fasting_glucose >= 7.0 | (dplyr::if_else(!is.na(on_dm_meds), on_dm_meds, FALSE)),
      diabetes       = fasting_glucose >= 7.0 | (dplyr::if_else(!is.na(on_dm_meds), on_dm_meds, FALSE)),
      impaired_glucose = fasting_glucose >= 6.1 & fasting_glucose < 7.0
    )
  }

  # -- Cholesterol -----------------------------------------------------------
  if (!is.null(cols$total_chol)) {
    d <- d |>
      dplyr::rename(total_chol = dplyr::all_of(cols$total_chol)) |>
      dplyr::mutate(
        total_chol     = as.numeric(total_chol),
        total_chol     = dplyr::if_else(total_chol < 1 | total_chol > 20, NA_real_, total_chol),
        raised_chol    = total_chol >= 5.0
      )
  }

  # -- Drop incomplete cases for key variables --------------------------------
  n_before <- nrow(d)
  d <- d |> dplyr::filter(!is.na(age), !is.na(sex))
  n_after  <- nrow(d)
  if (n_before > n_after)
    message(glue::glue("  \u26a0 Removed {n_before - n_after} rows with missing age or sex"))

  message(glue::glue("\u2713 Cleaning complete. Final dataset: {nrow(d)} rows \u00d7 {ncol(d)} columns"))
  return(d)
}

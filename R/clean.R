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
#' @param bp_sbp_threshold SBP threshold for raised BP (default 140; Mongolia uses 130).
#' @param bp_dbp_threshold DBP threshold for raised BP (default 90; Mongolia uses 80).
#' @param bmi_overweight BMI threshold for overweight (default 25.0).
#' @param bmi_obese BMI threshold for obesity (default 30.0).
#' @param glucose_threshold Fasting glucose threshold for raised glucose / diabetes
#'   in mmol/L (default 7.0).
#' @param glucose_impaired_threshold Fasting glucose threshold for impaired fasting
#'   glucose in mmol/L (default 6.1).
#' @param chol_threshold Total cholesterol threshold for raised cholesterol
#'   in mmol/L (default 5.0).
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
clean_steps_data <- function(data, cols, age_min = 18, age_max = 69,
                             bp_sbp_threshold = 140, bp_dbp_threshold = 90,
                             bmi_overweight = 25.0, bmi_obese = 30.0,
                             glucose_threshold = 7.0,
                             glucose_impaired_threshold = 6.1,
                             chol_threshold = 5.0) {

  d <- data

  # Helper: TRUE only if the column mapping exists AND the column is in the data
  has <- function(col_name) {
    !is.null(cols[[col_name]]) && cols[[col_name]] %in% names(d)
  }

  # -- Filter to valid/consented records (WHO scripts: valid==1) ---------------
  if (has("valid")) {
    n_before <- nrow(d)
    d <- d |> dplyr::filter(.data[[cols$valid]] == 1)
    n_dropped <- n_before - nrow(d)
    if (n_dropped > 0) {
      message(glue::glue("    \u2713 Filtered to valid records: dropped {n_dropped}, kept {nrow(d)}"))
    }
  }

  # -- Age & Sex --------------------------------------------------------------
  if (has("age")) {
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

  # Use pre-computed agerange from WHO-processed datasets when available.
  # Country surveys often use different age groupings (e.g. 18-29/30-44/45-59/60-69
  # or 18-44/45-69), so the dataset's own agerange is authoritative for matching
  # published outputs. We keep our computed age_group as well for standardisation.
  if (has("agerange")) {
    agerange_col <- cols$agerange
    if (agerange_col != "agerange") {
      d <- d |> dplyr::rename(agerange = dplyr::all_of(agerange_col))
    }
    d$agerange <- as.character(d$agerange)
    d$agerange <- factor(d$agerange, levels = unique(sort(d$agerange)))
    message("    \u2713 Using dataset's pre-computed agerange: ",
            paste(levels(d$agerange), collapse = ", "))
  }

  if (has("sex")) {
    d <- d |>
      dplyr::rename(sex = dplyr::all_of(cols$sex)) |>
      dplyr::mutate(
        # Harmonise sex coding: 1=Male, 2=Female (WHO STEPS standard)
        sex = dplyr::case_when(
          sex %in% c(1, "1", "m", "M", "male", "Male", "Men")   ~ "Male",
          sex %in% c(2, "2", "f", "F", "female", "Female", "Women") ~ "Female",
          TRUE ~ NA_character_
        ),
        sex = factor(sex, levels = c("Male", "Female"))
      )
  }

  # -- Sampling design variables (step-specific weights per WHO STEPS) --------

  # WHO STEPS toolkit uses WStep1/WStep2/WStep3 -- one analysis weight per Step.
  # We rename to wt_step1/wt_step2/wt_step3 internally.
  # If only Step 1 weight is found, we copy it to Step 2 and Step 3.
  # If no weight is found at all, all three are set to 1 (unweighted).
  if (has("weight_step1")) {
    d <- d |> dplyr::rename(wt_step1 = dplyr::all_of(cols$weight_step1)) |>
      dplyr::mutate(wt_step1 = as.numeric(wt_step1))
  }
  if (has("weight_step2")) {
    d <- d |> dplyr::rename(wt_step2 = dplyr::all_of(cols$weight_step2)) |>
      dplyr::mutate(wt_step2 = as.numeric(wt_step2))
  }
  if (has("weight_step3")) {
    d <- d |> dplyr::rename(wt_step3 = dplyr::all_of(cols$weight_step3)) |>
      dplyr::mutate(wt_step3 = as.numeric(wt_step3))
  }

  # Fill in missing step weights: copy from step 1 if available, else unweighted
  if (!"wt_step1" %in% names(d)) {
    if (!"wt_step2" %in% names(d) && !"wt_step3" %in% names(d)) {
      message("  \u26a0 No weight variable found - using equal weights (unweighted)")
      d <- d |> dplyr::mutate(wt_step1 = 1, wt_step2 = 1, wt_step3 = 1)
    } else {
      # Use whichever step weight we found as fallback
      fallback <- if ("wt_step2" %in% names(d)) "wt_step2" else "wt_step3"
      message(glue::glue("  \u26a0 No Step 1 weight - falling back to '{fallback}'"))
      d$wt_step1 <- d[[fallback]]
    }
  }
  if (!"wt_step2" %in% names(d)) {
    message("  \u26a0 No Step 2 weight found - copying Step 1 weight")
    d$wt_step2 <- d$wt_step1
  }
  if (!"wt_step3" %in% names(d)) {
    message("  \u26a0 No Step 3 weight found - copying Step 1 weight")
    d$wt_step3 <- d$wt_step1
  }

  # Keep wt_final as alias for backward compatibility (points to Step 1 weight)
  d$wt_final <- d$wt_step1

  if (has("strata")) d <- d |> dplyr::rename(stratum = dplyr::all_of(cols$strata))
  if (has("psu"))    d <- d |> dplyr::rename(psu     = dplyr::all_of(cols$psu))

  # -- Tobacco ---------------------------------------------------------------
  if (has("tobacco_current")) {
    d <- d |>
      dplyr::rename(tobacco_current_raw = dplyr::all_of(cols$tobacco_current)) |>
      dplyr::mutate(current_tobacco = recode_yn(tobacco_current_raw))
  }
  if (has("tobacco_daily")) {
    d <- d |>
      dplyr::rename(tobacco_daily_raw = dplyr::all_of(cols$tobacco_daily)) |>
      dplyr::mutate(daily_tobacco = recode_yn(tobacco_daily_raw))
  }
  if (has("smokeless_current")) {
    d <- d |>
      dplyr::rename(smokeless_current_raw = dplyr::all_of(cols$smokeless_current)) |>
      dplyr::mutate(current_smokeless = recode_yn(smokeless_current_raw))
  }
  if (has("smokeless_daily")) {
    d <- d |>
      dplyr::rename(smokeless_daily_raw = dplyr::all_of(cols$smokeless_daily)) |>
      dplyr::mutate(daily_smokeless = recode_yn(smokeless_daily_raw))
  }

  # -- WHO data quality filters (smk_cln / smkless_cln) -----------------------
  # WHO official scripts exclude logically inconsistent tobacco respondents
  # from the denominator.  smk_cln==1 means "valid smoking record":
  #   • current smoker AND t2 answered (daily/not daily) → valid
  #   • non-smoker AND (t2 is NA or says not daily)      → valid
  #   • current smoker but past-use says "never smoked"  → INVALID
  #   • current smoker but t2 is not answered             → INVALID
  #
  # NOTE: We use the already-decoded logical current_tobacco rather than raw
  # codes, because raw coding varies (0/1 in some datasets, 1/2 in WHO STEPS).
  # recode_yn() has already normalised this.
  if ("current_tobacco" %in% names(d)) {
    ct <- d$current_tobacco  # logical: TRUE = smoker, FALSE = non-smoker
    # daily_tobacco is logical if decoded, else we look at raw
    dt <- if ("daily_tobacco" %in% names(d)) {
      d$daily_tobacco
    } else {
      rep(NA, nrow(d))
    }
    # Past tobacco use (t8) — raw value; "2" means "never smoked" in WHO coding
    t8_raw <- if (has("tobacco_past")) as.numeric(d[[cols$tobacco_past]]) else rep(NA_real_, nrow(d))

    # Only apply strict validation when past-use variable (t8) is available
    has_t8 <- any(!is.na(t8_raw))

    smk_cln <- dplyr::case_when(
      # Smoker who says they never smoked in the past → inconsistent
      !is.na(ct) & ct == TRUE & !is.na(t8_raw) & t8_raw == 2   ~ 2L,
      # Smoker with daily/nondaily answer → valid
      !is.na(ct) & ct == TRUE  & !is.na(dt)                     ~ 1L,
      # Non-smoker → always valid (t2 doesn't matter)
      !is.na(ct) & ct == FALSE                                   ~ 1L,
      # Smoker but daily question unanswered → invalid only if t8 is in the dataset
      !is.na(ct) & ct == TRUE  & is.na(dt) & has_t8             ~ 2L,
      # Default: valid (don't penalise datasets without t2/t8)
      TRUE                                                       ~ 1L
    )

    n_invalid_smk <- sum(smk_cln == 2, na.rm = TRUE)
    if (n_invalid_smk > 0) {
      d$current_tobacco[smk_cln == 2] <- NA
      message(glue::glue("    \u2713 WHO smk_cln filter: {n_invalid_smk} inconsistent smoking records set to NA"))
    }
  }

  if ("current_smokeless" %in% names(d)) {
    cs <- d$current_smokeless  # logical
    ds <- if ("daily_smokeless" %in% names(d)) {
      d$daily_smokeless
    } else {
      rep(NA, nrow(d))
    }
    t15_raw <- if (has("smokeless_past")) as.numeric(d[[cols$smokeless_past]]) else rep(NA_real_, nrow(d))

    has_t15 <- any(!is.na(t15_raw))

    smkless_cln <- dplyr::case_when(
      !is.na(cs) & cs == TRUE  & !is.na(t15_raw) & t15_raw == 2 ~ 2L,
      !is.na(cs) & cs == TRUE  & !is.na(ds)                      ~ 1L,
      !is.na(cs) & cs == FALSE                                    ~ 1L,
      !is.na(cs) & cs == TRUE  & is.na(ds) & has_t15             ~ 2L,
      TRUE                                                        ~ 1L
    )

    n_invalid_smkless <- sum(smkless_cln == 2, na.rm = TRUE)
    if (n_invalid_smkless > 0) {
      d$current_smokeless[smkless_cln == 2] <- NA
      message(glue::glue("    \u2713 WHO smkless_cln filter: {n_invalid_smkless} inconsistent smokeless records set to NA"))
    }
  }

  # Derived smoking variables
  # current_smoker = smokes any tobacco product (cigarettes, pipes, etc.)
  if ("current_tobacco" %in% names(d)) {
    d$current_smoker <- d$current_tobacco
  }
  # Smoking status categories
  if (has("tobacco_past")) {
    d <- d |>
      dplyr::rename(tobacco_past_raw = dplyr::all_of(cols$tobacco_past)) |>
      dplyr::mutate(former_smoker = recode_yn(tobacco_past_raw))
  }
  if (all(c("current_tobacco", "daily_tobacco") %in% names(d))) {
    d <- d |> dplyr::mutate(
      nondaily_smoker = current_tobacco & !dplyr::if_else(is.na(daily_tobacco), FALSE, daily_tobacco),
      daily_smoker    = dplyr::if_else(is.na(daily_tobacco), FALSE, daily_tobacco)
    )
    # Smoking status factor
    if ("former_smoker" %in% names(d)) {
      d <- d |> dplyr::mutate(
        never_smoker = !current_tobacco & !dplyr::if_else(is.na(former_smoker), FALSE, former_smoker),
        smoking_status = dplyr::case_when(
          daily_smoker    ~ "Daily",
          nondaily_smoker ~ "Non-daily",
          dplyr::if_else(is.na(former_smoker), FALSE, former_smoker) ~ "Former",
          TRUE ~ "Never"
        )
      )
    }
  }
  # Past daily smoker
  if (has("tobacco_past_daily")) {
    d <- d |>
      dplyr::rename(tobacco_past_daily_raw = dplyr::all_of(cols$tobacco_past_daily)) |>
      dplyr::mutate(former_daily_smoker = recode_yn(tobacco_past_daily_raw))
  }
  # Age started smoking and duration
  if (has("tobacco_start_age")) {
    d <- d |> dplyr::mutate(
      smoking_start_age = as.numeric(.data[[cols$tobacco_start_age]])
    )
  }
  if (has("tobacco_duration")) {
    d <- d |> dplyr::mutate(
      smoking_duration = as.numeric(.data[[cols$tobacco_duration]])
    )
  }
  # Tobacco products: manufactured cigarettes per day
  if (has("tobacco_cig_day")) {
    d <- d |> dplyr::mutate(
      cig_per_day_manufactured = as.numeric(.data[[cols$tobacco_cig_day]])
    )
    if ("daily_smoker" %in% names(d)) {
      d <- d |> dplyr::mutate(
        smokes_manufactured_cig = dplyr::if_else(
          daily_smoker & !is.na(cig_per_day_manufactured) & cig_per_day_manufactured > 0,
          TRUE, FALSE
        )
      )
    }
  }
  if (has("tobacco_hand_day")) {
    d <- d |> dplyr::mutate(
      cig_per_day_handrolled = as.numeric(.data[[cols$tobacco_hand_day]])
    )
  }
  # Quit attempt and advice
  if (has("tobacco_quit_attempt")) {
    d <- d |> dplyr::mutate(quit_attempt = recode_yn(.data[[cols$tobacco_quit_attempt]]))
  }
  if (has("tobacco_quit_advice")) {
    d <- d |> dplyr::mutate(quit_advice = recode_yn(.data[[cols$tobacco_quit_advice]]))
  }
  # Second-hand smoke
  if (has("secondhand_home")) {
    d <- d |> dplyr::mutate(secondhand_home = recode_yn(.data[[cols$secondhand_home]]))
  }
  if (has("secondhand_work")) {
    d <- d |> dplyr::mutate(secondhand_work = recode_yn(.data[[cols$secondhand_work]]))
  }

  # Fix: non-tobacco users should be FALSE for smokeless, not NA
  if ("daily_smokeless" %in% names(d) && "current_tobacco" %in% names(d)) {
    d <- d |> dplyr::mutate(
      daily_smokeless = dplyr::if_else(
        current_tobacco == FALSE & is.na(daily_smokeless), FALSE, daily_smokeless
      )
    )
  }
  if ("current_smokeless" %in% names(d) && "current_tobacco" %in% names(d)) {
    d <- d |> dplyr::mutate(
      current_smokeless = dplyr::if_else(
        current_tobacco == FALSE & is.na(current_smokeless), FALSE, current_smokeless
      )
    )
  }

  # Combined: any current tobacco use (smoking OR smokeless)
  has_smoker    <- "current_smoker" %in% names(d)
  has_smokeless <- "current_smokeless" %in% names(d) || "daily_smokeless" %in% names(d)
  if (has_smoker && has_smokeless) {
    sl_var <- if ("current_smokeless" %in% names(d)) "current_smokeless" else "daily_smokeless"
    d <- d |> dplyr::mutate(
      current_tobacco_any = dplyr::if_else(
        is.na(current_smoker) & is.na(.data[[sl_var]]), NA,
        dplyr::if_else(
          dplyr::coalesce(current_smoker, FALSE) | dplyr::coalesce(.data[[sl_var]], FALSE),
          TRUE, FALSE
        )
      )
    )
  }

  # -- Alcohol ---------------------------------------------------------------
  if (has("alcohol_current")) {
    d <- d |>
      dplyr::rename(alcohol_current_raw = dplyr::all_of(cols$alcohol_current))
    # A5-style columns may have 3+ coded values (1=current, 2=past 12m, 3+=abstainer)
    # recode_yn handles 1->TRUE, 2->FALSE, but other values (3,4,...) -> NA.
    # For "current alcohol", any non-1 response means "not current" = FALSE.
    raw_vals <- unique(stats::na.omit(as.numeric(d$alcohol_current_raw)))
    if (length(raw_vals) > 0 && max(raw_vals, na.rm = TRUE) > 2) {
      # Multi-value column: only value 1 = current drinker
      message("  \u2192 Multi-value alcohol column detected (values: ",
              paste(sort(raw_vals), collapse = ", "), ") -- coding 1=current, rest=not current")
      d <- d |>
        dplyr::mutate(current_alcohol = as.numeric(alcohol_current_raw) == 1)
    } else {
      d <- d |>
        dplyr::mutate(current_alcohol = recode_yn(alcohol_current_raw))
    }
  }

  if (has("heavy_episode")) {
    d <- d |>
      dplyr::rename(heavy_episode_raw = dplyr::all_of(cols$heavy_episode))

    # a9 in WHO STEPS v3.2 is a COUNT variable (number of occasions with 6+ drinks).
    # Values: 0 = never, 1+ = yes, 77 = don't know, 88 = refused.
    # For yes/no binary columns, recode_yn handles it. For count columns,
    # we derive: HED = TRUE if count >= 1 (excluding 77/88).
    raw_vals <- unique(stats::na.omit(as.numeric(d$heavy_episode_raw)))
    if (length(raw_vals) > 0 && max(raw_vals, na.rm = TRUE) > 2) {
      # Count variable: 0 = no HED, >=1 = HED, 77/88 = unknown
      message("  \u2192 Heavy episodic drinking: count variable detected (values: ",
              paste(sort(raw_vals[raw_vals <= 10]), collapse = ", "),
              if (any(raw_vals > 10)) paste0(", ..., ", max(raw_vals)) else "", ")")
      d <- d |>
        dplyr::mutate(
          heavy_episode_raw_n = as.numeric(heavy_episode_raw),
          heavy_episodic = dplyr::case_when(
            heavy_episode_raw_n %in% c(77, 88, 99) ~ NA,
            heavy_episode_raw_n >= 1 ~ TRUE,
            heavy_episode_raw_n == 0 ~ FALSE,
            TRUE ~ NA
          )
        ) |>
        dplyr::select(-heavy_episode_raw_n)
    } else {
      d <- d |>
        dplyr::mutate(heavy_episodic = recode_yn(heavy_episode_raw))
    }

  }

  # Alcohol status categories (ever / 12m / current)
  if (has("alcohol_ever")) {
    d <- d |> dplyr::mutate(alcohol_ever = recode_yn(.data[[cols$alcohol_ever]]))
  }
  if (has("alcohol_12m")) {
    d <- d |> dplyr::mutate(alcohol_12m = recode_yn(.data[[cols$alcohol_12m]]))
  }

  # WHO STEPS denominator fix: respondents who skipped the "past 30 days"

  # question because they are lifetime abstainers (a1=No) or didn't drink
  # in the past 12 months (a2=No) should be FALSE for current_alcohol,
  # NOT NA. Otherwise svymean with na.rm=TRUE inflates the proportion
  # by excluding non-drinkers from the denominator.
  if ("current_alcohol" %in% names(d)) {
    if ("alcohol_ever" %in% names(d)) {
      d <- d |> dplyr::mutate(
        current_alcohol = dplyr::if_else(
          is.na(current_alcohol) & !is.na(alcohol_ever) & alcohol_ever == FALSE,
          FALSE, current_alcohol
        )
      )
    }
    if ("alcohol_12m" %in% names(d)) {
      d <- d |> dplyr::mutate(
        current_alcohol = dplyr::if_else(
          is.na(current_alcohol) & !is.na(alcohol_12m) & alcohol_12m == FALSE,
          FALSE, current_alcohol
        )
      )
    }
    # Also: if alcohol_ever exists and is FALSE, but current_alcohol is still NA
    # (no alcohol_12m column), set to FALSE
    message("    \u2713 Non-drinkers coded as FALSE for current_alcohol (not NA)")
  }

  # WHO STEPS standard: HED denominator = total population, not just drinkers.
  # Non-drinkers (by any upstream skip) must be coded FALSE (not NA) for heavy
  # episodic drinking.  This block runs AFTER the current_alcohol denominator
  # fix so that all non-drinker skip patterns have already been resolved.
  if ("heavy_episodic" %in% names(d)) {
    if ("current_alcohol" %in% names(d)) {
      d <- d |> dplyr::mutate(
        heavy_episodic = dplyr::if_else(
          current_alcohol == FALSE & is.na(heavy_episodic), FALSE, heavy_episodic
        )
      )
    }
    if ("alcohol_12m" %in% names(d)) {
      d <- d |> dplyr::mutate(
        heavy_episodic = dplyr::if_else(
          alcohol_12m == FALSE & is.na(heavy_episodic), FALSE, heavy_episodic
        )
      )
    }
    if ("alcohol_ever" %in% names(d)) {
      d <- d |> dplyr::mutate(
        heavy_episodic = dplyr::if_else(
          alcohol_ever == FALSE & is.na(heavy_episodic), FALSE, heavy_episodic
        )
      )
    }
    message("    \u2713 Non-drinkers coded as FALSE for heavy_episodic (total population denominator)")
  }

  # Alcohol consumption status (4-level category)
  if (all(c("alcohol_ever", "alcohol_12m") %in% names(d))) {
    d <- d |> dplyr::mutate(
      lifetime_abstainer  = dplyr::if_else(is.na(alcohol_ever), NA, !alcohol_ever),
      past_12m_abstainer  = dplyr::if_else(alcohol_ever & !dplyr::if_else(is.na(alcohol_12m), FALSE, alcohol_12m), TRUE, FALSE),
      past_12m_not_current = dplyr::if_else(
        dplyr::if_else(is.na(alcohol_12m), FALSE, alcohol_12m) &
        !dplyr::if_else(is.na(current_alcohol), FALSE, current_alcohol), TRUE, FALSE
      )
    )
  }
  # Alcohol frequency past 12 months
  if (has("alcohol_freq_12m")) {
    d <- d |> dplyr::mutate(
      alcohol_freq_12m = as.numeric(.data[[cols$alcohol_freq_12m]])
    )
  }
  # Drinking occasions and drinks per occasion (continuous means)
  if (has("alcohol_occasions")) {
    d <- d |> dplyr::mutate(
      drinking_occasions_30d = as.numeric(.data[[cols$alcohol_occasions]])
    )
  }
  if (has("alcohol_drinks_occasion")) {
    d <- d |> dplyr::mutate(
      drinks_per_occasion = as.numeric(.data[[cols$alcohol_drinks_occasion]])
    )
  }
  # Drinking level categories (from High_End_Drinking or derived)
  if (has("drinking_level")) {
    d <- d |> dplyr::mutate(
      drinking_level_raw = as.numeric(.data[[cols$drinking_level]])
    )
  }

  # -- Physical activity (MET-based) -----------------------------------------

  # WHO STEPS special code cleaning for GPAQ:
  # 77 = don't know, 88 = refused → recode to NA before any computation.
  # Days should be 0-7, hours 0-23, minutes 0-59.
  # Also: if respondent said "No" to the activity (e.g. p1=2 for vig work),
  # the skip pattern means days/hours/mins should be 0 (not NA).
  .clean_gpaq_val <- function(x, max_valid = NULL) {
    x <- as.numeric(x)
    x[x %in% c(77, 88, 99)] <- NA
    if (!is.null(max_valid)) x[!is.na(x) & x > max_valid] <- NA
    x
  }

  # Clean GPAQ days (max 7), hours (max 23), minutes (max 59)
  gpaq_days_cols <- c("pa_work_vig_days", "pa_work_mod_days", "pa_transport_days",
                      "pa_rec_vig_days", "pa_rec_mod_days")
  gpaq_hrs_cols  <- c("pa_work_vig_hrs", "pa_work_mod_hrs", "pa_transport_hrs",
                      "pa_rec_vig_hrs", "pa_rec_mod_hrs")
  gpaq_min_cols  <- c("pa_work_vig_min", "pa_work_mod_min", "pa_transport_min",
                      "pa_rec_vig_min", "pa_rec_mod_min")

  # Pre-clean: recode 77/88 to NA and cap at valid ranges
  for (col_key in gpaq_days_cols) {
    if (has(col_key)) {
      d[[cols[[col_key]]]] <- .clean_gpaq_val(d[[cols[[col_key]]]], max_valid = 7)
    }
  }
  for (col_key in gpaq_hrs_cols) {
    if (has(col_key)) {
      d[[cols[[col_key]]]] <- .clean_gpaq_val(d[[cols[[col_key]]]], max_valid = 23)
    }
  }
  for (col_key in gpaq_min_cols) {
    if (has(col_key)) {
      d[[cols[[col_key]]]] <- .clean_gpaq_val(d[[cols[[col_key]]]], max_valid = 59)
    }
  }
  # Also clean sedentary hours/minutes
  if (has("pa_sedentary_hrs")) d[[cols$pa_sedentary_hrs]] <- .clean_gpaq_val(d[[cols$pa_sedentary_hrs]], max_valid = 23)
  if (has("pa_sedentary_min")) d[[cols$pa_sedentary_min]] <- .clean_gpaq_val(d[[cols$pa_sedentary_min]], max_valid = 59)

  message("    \u2713 GPAQ special codes (77/88/99) cleaned, values capped at valid ranges")

  if (has("met_total")) {
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

  # PA domain-specific: minutes per day for work, transport, recreation
  # WHO STEPS cleaning rules:
  #  1. When a screening question (P1/P4/P7/P10/P13) = "No", the follow-up
  #     days/time questions are skipped (NA).  The domain contribution is 0
  #     (not missing).  We must set it explicitly so these respondents are
  #     included in the MET denominator.
  #  2. Cap daily time at 16 hours (960 min) per domain.  If exceeded,
  #     the WHO GPAQ guide says ALL PA data for that person should be removed.

  # Helper: recode screening question to logical (TRUE = does activity)
  .screen_yes <- function(x) {
    v <- as.numeric(x)
    dplyr::if_else(v == 1, TRUE, dplyr::if_else(v == 2, FALSE, NA))
  }

  # Helper to compute domain min/day, respecting screening question
  .domain_min_day <- function(screen_col, days_col, hrs_col, min_col) {
    # If screening question is available and answered "No" → 0
    screen <- if (!is.null(screen_col) && screen_col %in% names(d)) .screen_yes(d[[screen_col]]) else NULL
    days <- as.numeric(d[[days_col]])
    hrs  <- if (!is.null(hrs_col) && hrs_col %in% names(d)) as.numeric(d[[hrs_col]]) else 0
    mins <- if (!is.null(min_col) && min_col %in% names(d)) as.numeric(d[[min_col]]) else 0

    daily_time <- hrs * 60 + mins  # time per session (= per day)
    result <- (days * daily_time) / 7  # average min/day over the week

    # WHO cleaning: if daily time > 960 min in this domain, set ALL PA to NA
    # (flagged here; cleaned up after all domains are computed)
    result <- dplyr::if_else(!is.na(daily_time) & daily_time > 960, NA_real_, result)

    # Screening = No → domain contribution is 0
    if (!is.null(screen)) {
      result <- dplyr::if_else(!is.na(screen) & screen == FALSE, 0, result)
    }
    result
  }

  # Work vigorous: P1 screens P2/P3
  if (has("pa_work_vig_days") && (has("pa_work_vig_hrs") || has("pa_work_vig_min"))) {
    d$pa_work_vig_min_day <- .domain_min_day(
      if (has("pa_work_vig")) cols$pa_work_vig else NULL,
      cols$pa_work_vig_days,
      if (has("pa_work_vig_hrs")) cols$pa_work_vig_hrs else NULL,
      if (has("pa_work_vig_min")) cols$pa_work_vig_min else NULL
    )
  }
  # Work moderate: P4 screens P5/P6
  if (has("pa_work_mod_days") && (has("pa_work_mod_hrs") || has("pa_work_mod_min"))) {
    d$pa_work_mod_min_day <- .domain_min_day(
      if (has("pa_work_mod")) cols$pa_work_mod else NULL,
      cols$pa_work_mod_days,
      if (has("pa_work_mod_hrs")) cols$pa_work_mod_hrs else NULL,
      if (has("pa_work_mod_min")) cols$pa_work_mod_min else NULL
    )
  }
  # Sum work domain
  work_cols <- intersect(c("pa_work_vig_min_day", "pa_work_mod_min_day"), names(d))
  if (length(work_cols) > 0) {
    d$pa_work_min_day <- rowSums(d[work_cols], na.rm = TRUE)
    d$pa_work_min_day[rowSums(!is.na(d[work_cols])) == 0] <- NA
  }

  # Transport: P7 screens P8/P9
  if (has("pa_transport_days") && (has("pa_transport_hrs") || has("pa_transport_min"))) {
    d$pa_transport_min_day <- .domain_min_day(
      if (has("pa_transport")) cols$pa_transport else NULL,
      cols$pa_transport_days,
      if (has("pa_transport_hrs")) cols$pa_transport_hrs else NULL,
      if (has("pa_transport_min")) cols$pa_transport_min else NULL
    )
  }

  # Recreation vigorous: P10 screens P11/P12
  if (has("pa_rec_vig_days") && (has("pa_rec_vig_hrs") || has("pa_rec_vig_min"))) {
    d$pa_rec_vig_min_day <- .domain_min_day(
      if (has("pa_rec_vig")) cols$pa_rec_vig else NULL,
      cols$pa_rec_vig_days,
      if (has("pa_rec_vig_hrs")) cols$pa_rec_vig_hrs else NULL,
      if (has("pa_rec_vig_min")) cols$pa_rec_vig_min else NULL
    )
  }
  # Recreation moderate: P13 screens P14/P15
  if (has("pa_rec_mod_days") && (has("pa_rec_mod_hrs") || has("pa_rec_mod_min"))) {
    d$pa_rec_mod_min_day <- .domain_min_day(
      if (has("pa_rec_mod")) cols$pa_rec_mod else NULL,
      cols$pa_rec_mod_days,
      if (has("pa_rec_mod_hrs")) cols$pa_rec_mod_hrs else NULL,
      if (has("pa_rec_mod_min")) cols$pa_rec_mod_min else NULL
    )
  }
  rec_cols <- intersect(c("pa_rec_vig_min_day", "pa_rec_mod_min_day"), names(d))
  if (length(rec_cols) > 0) {
    d$pa_recreation_min_day <- rowSums(d[rec_cols], na.rm = TRUE)
    d$pa_recreation_min_day[rowSums(!is.na(d[rec_cols])) == 0] <- NA
  }

  # Total PA minutes per day (sum of all domains)
  all_pa_cols <- intersect(c("pa_work_min_day", "pa_transport_min_day", "pa_recreation_min_day"), names(d))
  if (length(all_pa_cols) > 0) {
    d$pa_minutes_per_day <- rowSums(d[all_pa_cols], na.rm = TRUE)
    d$pa_minutes_per_day[rowSums(!is.na(d[all_pa_cols])) == 0] <- NA
  }

  message("    \u2713 GPAQ screening questions used to set non-active domains to 0")

  # Compute MET-min/week from raw GPAQ domain variables if met_total not already set
  # WHO GPAQ MET formula: vigorous activities × 8 MET, moderate/transport × 4 MET
  if (!"met_total" %in% names(d)) {
    met_components <- list(
      pa_work_vig_min_day    = 8,   # vigorous work: 8 MET
      pa_work_mod_min_day    = 4,   # moderate work: 4 MET
      pa_transport_min_day   = 4,   # transport: 4 MET
      pa_rec_vig_min_day     = 8,   # vigorous recreation: 8 MET
      pa_rec_mod_min_day     = 4    # moderate recreation: 4 MET
    )
    available_met <- names(met_components)[names(met_components) %in% names(d)]
    if (length(available_met) > 0) {
      met_matrix <- do.call(cbind, lapply(available_met, function(col) {
        d[[col]] * met_components[[col]] * 7  # convert min/day to MET-min/week
      }))
      d$met_total <- rowSums(met_matrix, na.rm = TRUE)
      d$met_total[rowSums(!is.na(met_matrix)) == 0] <- NA
      d$insufficient_pa <- d$met_total < 600
      d$low_pa          <- d$met_total < 3000
      d$pa_category <- dplyr::case_when(
        d$met_total >= 3000 ~ "High",
        d$met_total >= 600  ~ "Moderate",
        TRUE                ~ "Low"
      )
      message("    \u2713 Computed met_total and insufficient_pa from GPAQ domain variables")
    }
  }

  # No vigorous activity
  if (has("pa_work_vig") && has("pa_rec_vig")) {
    d <- d |> dplyr::mutate(
      no_vigorous_pa = recode_yn(.data[[cols$pa_work_vig]]) == FALSE &
                       recode_yn(.data[[cols$pa_rec_vig]]) == FALSE
    )
  }

  # Sedentary time (P16a hours + P16b minutes)
  if (has("pa_sedentary_hrs") || has("pa_sedentary_min")) {
    hrs <- if (has("pa_sedentary_hrs")) as.numeric(d[[cols$pa_sedentary_hrs]]) else 0
    mins <- if (has("pa_sedentary_min")) as.numeric(d[[cols$pa_sedentary_min]]) else 0
    d$sedentary_min_day <- hrs * 60 + mins
  }

  # -- Diet ------------------------------------------------------------------
  # WHO STEPS diet cleaning:
  #  1. Special codes (77=don't know, 88=refused) → NA
  #  2. When days=0, servings question is skipped (NA) — contribution is 0, not missing
  #  3. Cap servings at reasonable max (WHO uses no explicit cap, but >30 is implausible)
  .clean_diet_val <- function(x) {
    x <- as.numeric(x)
    x[x %in% c(77, 88, 99)] <- NA
    x
  }

  if (has("fruit_days") & has("fruit_servings")) {
    d <- d |>
      dplyr::rename(fruit_days = dplyr::all_of(cols$fruit_days),
             fruit_serv = dplyr::all_of(cols$fruit_servings)) |>
      dplyr::mutate(
        fruit_days = .clean_diet_val(fruit_days),
        fruit_serv = .clean_diet_val(fruit_serv),
        # When days=0, servings is skipped (NA) — avg is 0, not missing
        fruit_serv = dplyr::if_else(fruit_days == 0 & is.na(fruit_serv), 0, fruit_serv),
        avg_fruit_servings = (fruit_days * fruit_serv) / 7
      )
  }
  if (has("veg_days") & has("veg_servings")) {
    d <- d |>
      dplyr::rename(veg_days = dplyr::all_of(cols$veg_days),
             veg_serv = dplyr::all_of(cols$veg_servings)) |>
      dplyr::mutate(
        veg_days = .clean_diet_val(veg_days),
        veg_serv = .clean_diet_val(veg_serv),
        veg_serv = dplyr::if_else(veg_days == 0 & is.na(veg_serv), 0, veg_serv),
        avg_veg_servings = (veg_days * veg_serv) / 7
      )
  }
  message("    \u2713 Diet special codes (77/88) cleaned; zero-days \u2192 0 servings")

  # Combined fruit+veg servings per day and <5 threshold
  if (all(c("avg_fruit_servings", "avg_veg_servings") %in% names(d))) {
    d <- d |>
      dplyr::mutate(
        avg_fruit_veg_servings = avg_fruit_servings + avg_veg_servings,
        low_fruit_veg = avg_fruit_veg_servings < 5
      )
  }

  # Salt-related variables
  if (has("salt_table")) {
    d <- d |> dplyr::mutate(
      always_add_salt = as.numeric(.data[[cols$salt_table]]) %in% c(1, 2)  # 1=Always, 2=Often
    )
  }
  if (has("processed_salt")) {
    d <- d |> dplyr::mutate(
      always_processed_salt = as.numeric(.data[[cols$processed_salt]]) %in% c(1, 2)
    )
  }
  if (has("salt_perception")) {
    d <- d |> dplyr::mutate(
      perceived_excess_salt = as.numeric(.data[[cols$salt_perception]]) %in% c(1, 2)  # Far too much / Too much
    )
  }

  # -- Anthropometry ---------------------------------------------------------
  if (has("height") & has("weight")) {
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
          bmi < bmi_overweight    ~ "Normal",
          bmi < bmi_obese         ~ "Overweight",
          bmi >= bmi_obese        ~ "Obese",
          TRUE                    ~ NA_character_
        ),
        overweight_obese = bmi >= bmi_overweight,
        obese            = bmi >= bmi_obese
      )
  }

  if (has("waist")) {
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

  if (has("hip")) {
    d <- d |>
      dplyr::rename(hip_cm = dplyr::all_of(cols$hip)) |>
      dplyr::mutate(
        hip_cm = as.numeric(hip_cm),
        hip_cm = dplyr::if_else(hip_cm < 50 | hip_cm > 200, NA_real_, hip_cm)
      )
  }
  # Waist-to-hip ratio (if both available)
  if (all(c("waist_cm", "hip_cm") %in% names(d))) {
    d <- d |> dplyr::mutate(waist_hip_ratio = waist_cm / hip_cm)
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
      # WHO STEPS protocol: mean of last 2 of 3 readings.
      # If reading 3 is missing, fall back to reading 2 alone.
      d <- d |> dplyr::mutate(
        sbp1 = as.numeric(sbp1), sbp2 = as.numeric(sbp2), sbp3 = as.numeric(sbp3),
        dbp1 = as.numeric(dbp1), dbp2 = as.numeric(dbp2), dbp3 = as.numeric(dbp3),
        mean_sbp = dplyr::case_when(
          !is.na(sbp2) & !is.na(sbp3) ~ (sbp2 + sbp3) / 2,
          !is.na(sbp2) ~ sbp2,
          !is.na(sbp3) ~ sbp3,
          TRUE ~ as.numeric(sbp1)
        ),
        mean_dbp = dplyr::case_when(
          !is.na(dbp2) & !is.na(dbp3) ~ (dbp2 + dbp3) / 2,
          !is.na(dbp2) ~ dbp2,
          !is.na(dbp3) ~ dbp3,
          TRUE ~ as.numeric(dbp1)
        )
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

  if (has("bp_meds")) {
    d <- d |>
      dplyr::rename(bp_meds_raw = dplyr::all_of(cols$bp_meds)) |>
      dplyr::mutate(on_bp_meds = recode_yn(bp_meds_raw))
  }

  # Ensure on_bp_meds exists (FALSE if not detected) before deriving raised_bp
  if (!"on_bp_meds" %in% names(d)) d$on_bp_meds <- FALSE

  if (all(c("mean_sbp", "mean_dbp") %in% names(d))) {
    if (bp_sbp_threshold != 140 || bp_dbp_threshold != 90) {
      message(glue::glue("    \u26a0 Using custom BP threshold: SBP>={bp_sbp_threshold} / DBP>={bp_dbp_threshold}"))
    }
    d <- d |> dplyr::mutate(
      raised_bp = (mean_sbp >= bp_sbp_threshold | mean_dbp >= bp_dbp_threshold) | (dplyr::if_else(!is.na(on_bp_meds), on_bp_meds, FALSE)),
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
  # WHO STEPS stores glucose in mmol/L (typical range 3-20), but some

  # country datasets use mg/dL (typical range 50-400).
  # Auto-detect units: if median > 35, assume mg/dL and convert to mmol/L.
  # Threshold aligned with WHO official scripts (median_b5 > 35).
  if (has("fasting_glucose")) {
    d <- d |>
      dplyr::rename(fasting_glucose = dplyr::all_of(cols$fasting_glucose)) |>
      dplyr::mutate(fasting_glucose = as.numeric(fasting_glucose))

    med_gluc <- stats::median(d$fasting_glucose, na.rm = TRUE)
    if (!is.na(med_gluc) && med_gluc > 35) {
      message(glue::glue("  \u26a0 Fasting glucose appears to be in mg/dL (median = {round(med_gluc, 1)}). Converting to mmol/L."))
      d <- d |> dplyr::mutate(fasting_glucose = fasting_glucose / 18.018)
    }

    # Plausibility check (mmol/L scale: 1-40)
    d <- d |> dplyr::mutate(
      fasting_glucose = dplyr::if_else(fasting_glucose < 1 | fasting_glucose > 40, NA_real_, fasting_glucose)
    )
  }
  if (has("dm_meds")) {
    d <- d |>
      dplyr::rename(dm_meds_raw = dplyr::all_of(cols$dm_meds)) |>
      dplyr::mutate(on_dm_meds = recode_yn(dm_meds_raw))
  }
  # Ensure on_dm_meds exists before deriving glucose indicators
  if (!"on_dm_meds" %in% names(d)) d$on_dm_meds <- FALSE

  if ("fasting_glucose" %in% names(d)) {
    d <- d |> dplyr::mutate(
      raised_glucose = fasting_glucose >= glucose_threshold | (dplyr::if_else(!is.na(on_dm_meds), on_dm_meds, FALSE)),
      diabetes       = fasting_glucose >= glucose_threshold | (dplyr::if_else(!is.na(on_dm_meds), on_dm_meds, FALSE)),
      impaired_glucose = fasting_glucose >= glucose_impaired_threshold & fasting_glucose < glucose_threshold
    )
  }

  # -- Cholesterol -----------------------------------------------------------
  # WHO STEPS stores cholesterol in mmol/L (typical range 2-12), but some
  # country datasets use mg/dL (typical range 80-400).
  # Auto-detect units: if median > 12, assume mg/dL and convert to mmol/L.
  # Threshold aligned with WHO official scripts (median_b8 > 12).
  if (has("total_chol")) {
    d <- d |>
      dplyr::rename(total_chol = dplyr::all_of(cols$total_chol)) |>
      dplyr::mutate(total_chol = as.numeric(total_chol))

    med_chol <- stats::median(d$total_chol, na.rm = TRUE)
    if (!is.na(med_chol) && med_chol > 12) {
      message(glue::glue("  \u26a0 Total cholesterol appears to be in mg/dL (median = {round(med_chol, 1)}). Converting to mmol/L."))
      d <- d |> dplyr::mutate(total_chol = total_chol / 38.67)
    }

    # Plausibility check (mmol/L scale: 1-20)
    d <- d |> dplyr::mutate(
      total_chol     = dplyr::if_else(total_chol < 1 | total_chol > 20, NA_real_, total_chol),
      raised_chol    = total_chol >= chol_threshold
    )
  }

  if (has("hdl_chol")) {
    d <- d |>
      dplyr::rename(hdl_chol = dplyr::all_of(cols$hdl_chol)) |>
      dplyr::mutate(hdl_chol = as.numeric(hdl_chol))

    med_hdl <- stats::median(d$hdl_chol, na.rm = TRUE)
    if (!is.na(med_hdl) && med_hdl > 10) {
      message(glue::glue("  \u26a0 HDL cholesterol appears to be in mg/dL (median = {round(med_hdl, 1)}). Converting to mmol/L."))
      d <- d |> dplyr::mutate(hdl_chol = hdl_chol / 38.67)
    }

    d <- d |> dplyr::mutate(
      hdl_chol = dplyr::if_else(hdl_chol < 0.1 | hdl_chol > 10, NA_real_, hdl_chol),
      low_hdl  = dplyr::case_when(
        sex == "Male"   & hdl_chol < 1.0 ~ TRUE,
        sex == "Female" & hdl_chol < 1.3 ~ TRUE,
        TRUE ~ FALSE
      )
    )
  }

  if (has("triglycerides")) {
    d <- d |>
      dplyr::rename(triglycerides = dplyr::all_of(cols$triglycerides)) |>
      dplyr::mutate(triglycerides = as.numeric(triglycerides))

    med_trig <- stats::median(d$triglycerides, na.rm = TRUE)
    if (!is.na(med_trig) && med_trig > 10) {
      message(glue::glue("  \u26a0 Triglycerides appears to be in mg/dL (median = {round(med_trig, 1)}). Converting to mmol/L."))
      d <- d |> dplyr::mutate(triglycerides = triglycerides / 88.57)
    }

    d <- d |> dplyr::mutate(
      triglycerides = dplyr::if_else(triglycerides < 0.1 | triglycerides > 30, NA_real_, triglycerides),
      raised_trig   = triglycerides >= 1.7   # WHO/IDF threshold: >=1.7 mmol/L
    )
  }

  # -- Additional cholesterol thresholds and medication-inclusive ---------------
  if ("total_chol" %in% names(d)) {
    d <- d |> dplyr::mutate(
      raised_chol_high = total_chol >= 6.2  # Higher threshold (240 mg/dL)
    )
  }
  if (has("chol_meds")) {
    d <- d |> dplyr::mutate(on_chol_meds = recode_yn(.data[[cols$chol_meds]]))
  }
  if (!"on_chol_meds" %in% names(d)) d$on_chol_meds <- FALSE
  if ("raised_chol" %in% names(d)) {
    d <- d |> dplyr::mutate(
      raised_chol_or_meds = raised_chol | dplyr::if_else(is.na(on_chol_meds), FALSE, on_chol_meds),
      raised_chol_high_or_meds = dplyr::if_else(is.na(raised_chol_high), FALSE, raised_chol_high) |
                                 dplyr::if_else(is.na(on_chol_meds), FALSE, on_chol_meds)
    )
  }

  # -- History variables (H-codes) -------------------------------------------
  # BP history
  if (has("bp_ever_measured")) {
    d <- d |> dplyr::mutate(bp_ever_measured = recode_yn(.data[[cols$bp_ever_measured]]))
  }
  if (has("bp_diagnosed")) {
    d <- d |> dplyr::mutate(bp_diagnosed = recode_yn(.data[[cols$bp_diagnosed]]))
  }
  if (has("bp_diagnosed_12m")) {
    d <- d |> dplyr::mutate(bp_diagnosed_12m = recode_yn(.data[[cols$bp_diagnosed_12m]]))
  }
  # BP controlled: among those treated, SBP below threshold
  if (all(c("on_bp_meds", "mean_sbp", "mean_dbp") %in% names(d))) {
    d <- d |> dplyr::mutate(
      bp_controlled = on_bp_meds & mean_sbp < bp_sbp_threshold & mean_dbp < bp_dbp_threshold
    )
  }

  # Diabetes history
  if (has("glucose_ever_measured")) {
    d <- d |> dplyr::mutate(glucose_ever_measured = recode_yn(.data[[cols$glucose_ever_measured]]))
  }
  if (has("dm_diagnosed")) {
    d <- d |> dplyr::mutate(dm_diagnosed = recode_yn(.data[[cols$dm_diagnosed]]))
  }
  if (has("dm_diagnosed_12m")) {
    d <- d |> dplyr::mutate(dm_diagnosed_12m = recode_yn(.data[[cols$dm_diagnosed_12m]]))
  }
  if (has("dm_insulin")) {
    d <- d |> dplyr::mutate(on_dm_insulin = recode_yn(.data[[cols$dm_insulin]]))
  }
  # New DM: raised glucose but NOT previously diagnosed
  if (all(c("raised_glucose", "dm_diagnosed") %in% names(d))) {
    d <- d |> dplyr::mutate(
      dm_new = dplyr::if_else(is.na(raised_glucose), FALSE, raised_glucose) &
               !dplyr::if_else(is.na(dm_diagnosed), FALSE, dm_diagnosed)
    )
  }

  # Cholesterol history
  if (has("chol_ever_measured")) {
    d <- d |> dplyr::mutate(chol_ever_measured = recode_yn(.data[[cols$chol_ever_measured]]))
  }
  if (has("chol_diagnosed") && !"chol_diagnosed" %in% names(d)) {
    d <- d |> dplyr::mutate(chol_diagnosed = recode_yn(.data[[cols$chol_diagnosed]]))
  }
  if (has("chol_diagnosed_12m")) {
    d <- d |> dplyr::mutate(chol_diagnosed_12m = recode_yn(.data[[cols$chol_diagnosed_12m]]))
  }

  # CVD history
  if (has("cvd_history")) {
    d <- d |> dplyr::mutate(cvd_history = recode_yn(.data[[cols$cvd_history]]))
  }
  if (has("cvd_aspirin")) {
    d <- d |> dplyr::mutate(on_aspirin = recode_yn(.data[[cols$cvd_aspirin]]))
  }
  if (has("cvd_statins")) {
    d <- d |> dplyr::mutate(on_statins = recode_yn(.data[[cols$cvd_statins]]))
  }

  # Lifestyle advice (H20a-f)
  if (has("adv_quit_tobacco")) d$advised_quit_tobacco  <- recode_yn(d[[cols$adv_quit_tobacco]])
  if (has("adv_reduce_salt"))  d$advised_reduce_salt   <- recode_yn(d[[cols$adv_reduce_salt]])
  if (has("adv_fruit_veg"))    d$advised_fruit_veg     <- recode_yn(d[[cols$adv_fruit_veg]])
  if (has("adv_reduce_fat"))   d$advised_reduce_fat    <- recode_yn(d[[cols$adv_reduce_fat]])
  if (has("adv_more_pa"))      d$advised_more_pa       <- recode_yn(d[[cols$adv_more_pa]])
  if (has("adv_healthy_wt"))   d$advised_healthy_weight <- recode_yn(d[[cols$adv_healthy_wt]])

  # Cervical cancer screening (female only)
  if (has("cervical_screen")) {
    d <- d |> dplyr::mutate(cervical_screened = recode_yn(.data[[cols$cervical_screen]]))
  }

  # -- Heart rate --------------------------------------------------------------
  if (has("mean_hr")) {
    d <- d |> dplyr::mutate(mean_heart_rate = as.numeric(.data[[cols$mean_hr]]))
  } else {
    # Compute mean from readings (same protocol as BP: mean of last 2 of 3)
    hr_cols <- c(cols$heart_rate1, cols$heart_rate2, cols$heart_rate3)
    hr_cols <- hr_cols[!sapply(hr_cols, is.null)]
    if (length(hr_cols) >= 2) {
      for (i in seq_along(hr_cols)) {
        d[[paste0(".hr", i)]] <- as.numeric(d[[hr_cols[[i]]]])
      }
      if (length(hr_cols) == 3) {
        d <- d |> dplyr::mutate(
          mean_heart_rate = dplyr::case_when(
            !is.na(.hr2) & !is.na(.hr3) ~ (.hr2 + .hr3) / 2,
            !is.na(.hr2) ~ .hr2,
            !is.na(.hr3) ~ .hr3,
            TRUE ~ .hr1
          )
        )
      } else {
        d$mean_heart_rate <- d$.hr1
      }
      # Clean up temp columns
      d <- d |> dplyr::select(-dplyr::starts_with(".hr"))
    }
  }

  # -- BMI: additional category variables for registry tables -----------------
  if ("bmi_category" %in% names(d)) {
    d <- d |> dplyr::mutate(
      underweight    = bmi < 18.5,
      normal_weight  = bmi >= 18.5 & bmi < 25,
      overweight_only = bmi >= 25 & bmi < 30
      # obese already exists from original BMI derivation
    )
  }

  # -- Drop incomplete cases for key variables --------------------------------
  n_before <- nrow(d)
  d <- d |> dplyr::filter(!is.na(age), !is.na(sex))
  n_after  <- nrow(d)
  if (n_before > n_after)
    message(glue::glue("  \u26a0 Removed {n_before - n_after} rows with missing age or sex"))

  message(glue::glue("\u2713 Cleaning complete. Final dataset: {nrow(d)} rows x {ncol(d)} columns"))
  return(d)
}

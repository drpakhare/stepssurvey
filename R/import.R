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
    "dta"  = tryCatch(
      haven::read_dta(path),
      error = function(e) {
        # Some STEPS .dta files have encoding issues — retry with latin1
        if (grepl("encoding|convert", e$message, ignore.case = TRUE)) {
          message("  \u26a0 Encoding issue detected, retrying with latin1 encoding...")
          haven::read_dta(path, encoding = "latin1")
        } else {
          stop(e)
        }
      }
    ),
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
  # Build a lookup from lowercase data-name -> original data-name
  data_names_lc <- tolower(names(data))
  # Walk candidates in PRIORITY ORDER and return the first that exists in data
  for (cand in tolower(candidates)) {
    idx <- which(data_names_lc == cand)
    if (length(idx) > 0) {
      match <- names(data)[idx[1]]
      if (!is.null(label)) message(glue::glue("  \u2713 {label}: '{match}'"))
      return(match)
    }
  }
  if (!is.null(label)) message(glue::glue("  \u26a0 Could not auto-detect column for: {label}"))
  return(NULL)
}


#' Get the SPSS/haven label for a column (if any)
#' @param col A vector (typically from a data frame column).
#' @return Character string of the label, or "" if none.
#' @keywords internal
.col_label <- function(col) {
  lbl <- attr(col, "label")
  if (is.null(lbl)) return("")
  tolower(as.character(lbl))
}


#' Strip haven-labelled classes from all columns in a data frame
#'
#' Converts `haven_labelled` vectors to their base R types.
#' This prevents downstream errors (e.g., `dim<-.haven_labelled()
#' not supported`) when passing the data to DT, dplyr, or survey.
#'
#' @param data A data frame (typically from [import_steps_data()]).
#' @return The same data frame with all haven labels removed.
#' @keywords internal
.strip_haven_labels <- function(data) {
  for (j in seq_along(data)) {
    col <- data[[j]]
    if (inherits(col, "haven_labelled")) {
      data[[j]] <- as.vector(col)       # drops class + labels, keeps type
    }
    # Also remove residual "label" attributes that can confuse some packages
    attr(data[[j]], "label")  <- NULL
    attr(data[[j]], "format.spss") <- NULL
  }
  data
}


#' Label-aware column detection for ambiguous STEPS codes
#'
#' Some STEPS column codes (e.g., A1, A5) mean different things across
#' instrument versions.
#' This function first tries unambiguous candidate names, then checks
#' ambiguous codes only if their column label matches expected keywords.
#'
#' @param data A data frame.
#' @param safe_candidates Character vector of unambiguous column names.
#' @param ambiguous Named list: each element is a character vector of
#'   keywords that must appear in the column label for the code to match.
#'   Names are the ambiguous column codes.
#'   Example: `list(a1 = c("current", "30 day"), a5 = c("current", "30 day"))`
#' @param label Optional label for progress messages.
#' @return The matched column name or NULL.
#' @keywords internal
.detect_col_with_label <- function(data, safe_candidates, ambiguous = list(), label = NULL) {
  # First try safe (unambiguous) candidates via standard detect_col (no message yet)
  data_names_lc <- tolower(names(data))
  for (cand in tolower(safe_candidates)) {
    idx <- which(data_names_lc == cand)
    if (length(idx) > 0) {
      match <- names(data)[idx[1]]
      if (!is.null(label)) message(glue::glue("  \u2713 {label}: '{match}'"))
      return(match)
    }
  }

  # Then try ambiguous codes with label validation
  for (code in names(ambiguous)) {
    idx <- which(data_names_lc == tolower(code))
    if (length(idx) > 0) {
      col_name <- names(data)[idx[1]]
      col_lbl  <- .col_label(data[[col_name]])
      keywords <- ambiguous[[code]]
      # Check if ANY keyword appears in the column label
      if (any(vapply(keywords, function(kw) grepl(kw, col_lbl, ignore.case = TRUE), logical(1)))) {
        if (!is.null(label)) message(glue::glue("  \u2713 {label}: '{col_name}' (label: '{col_lbl}')"))
        return(col_name)
      } else {
        if (!is.null(label)) message(glue::glue("  \u2192 Skipped '{col_name}' for {label} (label '{col_lbl}' does not match)"))
      }
    }
  }

  if (!is.null(label)) message(glue::glue("  \u26a0 Could not auto-detect column for: {label}"))
  return(NULL)
}

#' Auto-detect all standard STEPS columns
#'
#' Scans a data frame for standard WHO STEPS variable names across
#' versions 3.1 and 3.2.  Aliases are listed in priority order: the
#' first match wins, so put the most specific / unambiguous name first.
#'
#' @param data A data frame (typically from [import_steps_data()]).
#' @return A named list of detected column names (or `NULL` for missing).
#'
#' @details
#' WHO STEPS reorganised variable codes between v3.1 and v3.2:
#'
#' **v3.1 / Epi Info codes** (still common in many country datasets):
#' B1-B6 = blood-pressure readings, B7 = BP meds,
#' C1 = fasting glucose, C5 = DM meds, C6 = total cholesterol, C10 = chol meds,
#' M1 = height, M2 = weight, M3 = waist.
#'
#' **v3.2 instrument codes**:
#' M4a/M5a/M6a = SBP readings, M4b/M5b/M6b = DBP readings, M7 = BP meds,
#' M11 = height, M12 = weight, M14 = waist, M15 = hip,
#' B5 = fasting glucose, B6 = DM meds, B8 = total cholesterol, B9 = chol meds,
#' B16 = triglycerides, B17 = HDL cholesterol,
#' C1 = sex, C3 = age.
#'
#' The function includes aliases for both versions so datasets from
#' either instrument version are detected automatically.
#'
#' @export
detect_steps_columns <- function(data) {
  message("  Detecting STEPS columns...")

  cols <- list(
    # -- Demographics -----------------------------------------------------------
    # v3.1: age/c1   v3.2: C3 = age, C1 = sex
    age       = detect_col(data, c("age", "age_years", "c3"), "Age"),
    # v3.1: sex/c2   v3.2: C1 = sex
    sex       = detect_col(data, c("sex", "gender", "c1", "c2"), "Sex"),
    # Pre-computed age range groups (standard in WHO-processed datasets)
    agerange  = detect_col(data, c("agerange", "age_range", "agegrp",
                                    "age_group"), "Age range group"),
    # Validity flag (WHO scripts filter on valid==1; not always present)
    valid     = detect_col(data, c("valid", "consent", "completed"), "Valid/consent flag"),
    # Urban/rural setting (common stratifier)
    urban_rural = detect_col(data, c("urbanrural", "urban_rural", "urban",
                                      "residence", "setting"), "Urban/rural"),
    # -- Survey design variables ------------------------------------------------
    # WHO STEPS toolkit standard: WStep1, WStep2, WStep3 (one weight per Step)
    # See Part 4, Section 1 of WHO STEPS Manual.
    # Also detect common country-specific variants (W1Dist, finalwt, etc.)
    weight_step1 = detect_col(data, c("wstep1", "w1dist", "w1_dist",
                                       "wt_final", "wt", "finalwt",
                                       "sampleweight", "samplingweight",
                                       "weight"), "Weight Step 1 (behavioural)"),
    weight_step2 = detect_col(data, c("wstep2", "w2dist", "w2_dist"), "Weight Step 2 (physical)"),
    weight_step3 = detect_col(data, c("wstep3", "w3dist", "w3_dist"), "Weight Step 3 (biochemical)"),
    strata    = detect_col(data, c("stratum", "strata", "stratum_id", "district",
                                    "region", "province"), "Stratum"),
    psu       = detect_col(data, c("psu", "cluster", "psu_id", "clusterid",
                                    "i1", "cluster_id", "ea_id"), "PSU"),

    # -- Step 1: Tobacco -------------------------------------------------------
    # T1 = currently smoke? (same in v3.1 & v3.2)
    tobacco_current = detect_col(data, c("t1", "smk_current", "current_smoker",
                                         "tobacco_current", "current_smoke"), "Current tobacco use"),
    # T2 = smoke daily? (same in v3.1 & v3.2)
    tobacco_daily   = detect_col(data, c("t2", "smk_daily", "daily_smoker",
                                         "tobacco_daily"), "Daily tobacco use"),
    # T3 = age started smoking (daily smokers)
    tobacco_start_age = detect_col(data, c("t3", "smoking_start_age",
                                            "age_start_smoking"), "Age started smoking"),
    # T4 = duration of smoking
    tobacco_duration = detect_col(data, c("t4", "smoking_duration", "t4type",
                                           "smoke_years"), "Smoking duration"),
    # T5a-f = tobacco products used (daily amounts)
    tobacco_cig_day  = detect_col(data, c("t5a", "manufactured_cigarettes_day",
                                           "cig_per_day"), "Manufactured cigarettes/day"),
    tobacco_cig_week = detect_col(data, c("t5aw", "manufactured_cigarettes_week"), "Manufactured cigarettes/week"),
    tobacco_hand_day = detect_col(data, c("t5b", "handrolled_day"), "Hand-rolled cigarettes/day"),
    tobacco_pipe_day = detect_col(data, c("t5c", "pipes_day"), "Pipes/day"),
    tobacco_cigar_day = detect_col(data, c("t5d", "cigars_day"), "Cigars/day"),
    tobacco_shisha_day = detect_col(data, c("t5e", "shisha_day", "waterpipe_day"), "Shisha/day"),
    # T6 = quit attempt in past 12 months
    tobacco_quit_attempt = detect_col(data, c("t6", "quit_attempt", "tried_quit"), "Quit attempt past 12m"),
    # T7 = advised to quit by doctor
    tobacco_quit_advice = detect_col(data, c("t7", "quit_advice", "advised_quit"), "Advised to quit by HCP"),
    # T8 = past smoking, T9 = past daily smoking
    tobacco_past     = detect_col(data, c("t8", "past_smoking", "ever_smoked"), "Past smoking"),
    tobacco_past_daily = detect_col(data, c("t9", "past_daily_smoking"), "Past daily smoking"),
    # T10/T11 = quitting details
    tobacco_quit_age = detect_col(data, c("t10", "quitting_age"), "Quitting age"),
    tobacco_quit_duration = detect_col(data, c("t11", "duration_since_quitting"), "Duration since quitting"),
    # T12/T13 = smokeless tobacco
    smokeless_current = detect_col(data, c("t12", "t15", "smokeless_current",
                                           "current_smokeless", "slt_current"), "Current smokeless tobacco"),
    smokeless_daily  = detect_col(data, c("t13", "smokeless_daily",
                                           "daily_smokeless"), "Daily smokeless tobacco"),
    # T14a-e = smokeless products
    slt_snuff_mouth  = detect_col(data, c("t14a", "snuff_daily_mouth"), "Snuff mouth/day"),
    slt_chewing      = detect_col(data, c("t14c", "chewing_tobacco_daily"), "Chewing tobacco/day"),
    slt_betel        = detect_col(data, c("t14d", "betel_daily", "betel_quid"), "Betel quid/day"),
    # T15/T16 = past smokeless
    smokeless_past   = detect_col(data, c("t15", "past_smokeless"), "Past smokeless tobacco"),
    smokeless_past_daily = detect_col(data, c("t16", "past_daily_smokeless"), "Past daily smokeless"),
    # T17/T18 = second-hand smoke
    secondhand_home  = detect_col(data, c("t17", "passive_home", "secondhand_home",
                                           "shs_home"), "Second-hand smoke at home"),
    secondhand_work  = detect_col(data, c("t18", "passive_workplace", "secondhand_work",
                                           "shs_work"), "Second-hand smoke at workplace"),
    # Derived: any tobacco product
    any_tobacco      = detect_col(data, c("tobacco_product_any",
                                           "daily_tobacco_product_any"), "Any tobacco product"),

    # -- Step 1: Alcohol -------------------------------------------------------
    # A1 = ever consumed alcohol (v3.2) -or- current drinker (v3.1) — AMBIGUOUS
    alcohol_ever    = .detect_col_with_label(data,
      safe_candidates = c("alc_ever", "ever_drinker", "alcohol_ever"),
      ambiguous = list(a1 = c("ever", "lifetime")),
      label = "Ever consumed alcohol"),

    # A2/A4 = consumed past 12 months (v3.2 CORE)
    alcohol_12m     = detect_col(data, c("a2", "a4", "alc_12m",
                                         "alcohol_12months"), "Alcohol past 12 months"),

    # Current alcohol use (past 30 days):
    # Unambiguous names tried first, then a1/a5 with label validation.
    # a1 = current drinker in v3.1 (label contains "current")
    # a5 = past 30 days in v3.2 (label contains "30 day" or "current")
    # a13 is safe (always AUDIT-C, not current use in v3.2) — removed
    alcohol_current = .detect_col_with_label(data,
      safe_candidates = c("alc_current", "current_drinker", "alcohol_current",
                          "alcohol_30days"),
      ambiguous = list(
        a5 = c("30 day", "current"),
        a1 = c("current")
      ),
      label = "Current alcohol use (past 30 days)"),

    # Heavy episodic drinking:
    # a9 = count of times consumed 6+ drinks on a single occasion (v3.2)
    heavy_episode   = .detect_col_with_label(data,
      safe_candidates = c("alcohol_6drin_occasions", "heavy_drinking", "binge",
                          "heavy_episodic", "hed", "hed_total"),
      ambiguous = list(
        a9 = c("6 or more", "6+", "heavy", "episodic", "binge")
      ),
      label = "Heavy episodic drinking"),

    # A3 = stopped drinking?
    alcohol_stopped = detect_col(data, c("a3", "alcohol_stopped", "stopped_drinking"), "Stopped drinking"),
    # A4 = frequency past 12 months
    alcohol_freq_12m = detect_col(data, c("a4", "alcohol_frequency_12m",
                                           "alcohol_frequency_past_12_months"), "Alcohol frequency (past 12m)"),
    # A6 = number of drinking occasions past 30 days
    alcohol_occasions = detect_col(data, c("a6", "drinking_occasions",
                                            "alcohol_number_of_drinking_occasions"), "Drinking occasions (past 30d)"),
    # A7 = number of drinks per occasion
    alcohol_drinks_occasion = detect_col(data, c("a7", "drinks_per_occasion",
                                                  "alcohol_number_of_drinks_on_one_occasion"), "Drinks per occasion"),
    # A8 = largest number of drinks
    alcohol_largest_drinks = detect_col(data, c("a8", "largest_drinks",
                                                 "alcohol_largest_number_of_drinks_on_one_occasion"), "Largest drinks on one occasion"),
    # A9 = number of times with 6+ drinks (count variable)
    alcohol_6plus_count = detect_col(data, c("a9", "alcohol_6_or_more_drinks"), "Times with 6+ drinks"),
    # A10a-g = drinking past 7 days by day of week
    alcohol_7day_freq = detect_col(data, c("alcohol_7_day_frequency",
                                            "alcohol_7day_freq"), "Alcohol 7-day frequency"),
    # A11 = homebrew consumption
    alcohol_homebrew = detect_col(data, c("a11", "homebrew"), "Homebrew consumption"),
    # Derived: drinking level (High_End_Drinking)
    drinking_level   = detect_col(data, c("high_end_drinking", "drinking_level",
                                           "alcohol_level"), "Drinking level category"),

    # -- Step 1: Physical activity (GPAQ) --------------------------------------
    # P1-P16 GPAQ items (v3.2 CORE); met_total is computed
    met_total       = detect_col(data, c("met_total", "total_met", "pa_met",
                                         "gpaq_met", "totalmet"), "Total MET minutes"),
    pa_work_vig     = detect_col(data, c("p1", "pa_work_vigorous"), "Vigorous work activity"),
    pa_work_mod     = detect_col(data, c("p4", "pa_work_moderate"), "Moderate work activity"),
    pa_transport    = detect_col(data, c("p7", "pa_transport", "walk_cycle"), "Walk/bicycle transport"),
    pa_rec_vig      = detect_col(data, c("p10", "pa_rec_vigorous"), "Vigorous recreational activity"),
    pa_rec_mod      = detect_col(data, c("p13", "pa_rec_moderate"), "Moderate recreational activity"),
    pa_sedentary    = detect_col(data, c("p16", "sedentary", "sitting_time"), "Sedentary behaviour"),

    # -- Step 1: Diet ----------------------------------------------------------
    # D1-D4 same in v3.1 & v3.2
    fruit_days      = detect_col(data, c("d1", "fruit_days", "days_fruit"), "Fruit days/week"),
    fruit_servings  = detect_col(data, c("d2", "fruit_serv", "servings_fruit"), "Fruit servings/day"),
    veg_days        = detect_col(data, c("d3", "veg_days", "days_veg"), "Vegetable days/week"),
    veg_servings    = detect_col(data, c("d4", "veg_serv", "servings_veg"), "Vegetable servings/day"),
    # D5 = salt added to food at table
    salt_table      = detect_col(data, c("d5", "d5_binary", "salt_addition_frequency",
                                          "salt_table"), "Salt added at table"),
    # D6 = salt in cooking (v3.2 EXPANDED)
    salt_cooking    = detect_col(data, c("d6", "salt_cooking", "salt_added"), "Salt added in cooking"),
    # D7 = processed food high in salt
    processed_salt  = detect_col(data, c("d7", "d7_binary", "processed_food_consumption_frequency",
                                          "processed_salt"), "Processed food high in salt"),
    # D8 = perceived salt intake
    salt_perception = detect_col(data, c("d8", "salt_perception",
                                          "perceived_salt"), "Perceived salt intake"),
    # D9 = importance of lowering salt
    salt_importance = detect_col(data, c("d9", "salt_importance"), "Importance of lowering salt"),
    # D10 = knowledge of salt health effects
    salt_knowledge  = detect_col(data, c("d10", "salt_knowledge"), "Knowledge of salt effects"),
    # D11a-f = actions to control salt
    salt_limit_processed = detect_col(data, c("d11a", "limit_processed_foods"), "Limit processed food for salt"),
    salt_check_labels    = detect_col(data, c("d11b", "look_at_salt_content"), "Check salt labels"),
    salt_buy_low         = detect_col(data, c("d11c", "buy_low_salt"), "Buy low-salt alternatives"),
    salt_use_spices      = detect_col(data, c("d11d", "use_spices"), "Use spices instead of salt"),
    salt_avoid_outside   = detect_col(data, c("d11e", "avoid_eating_outside"), "Avoid food outside home"),
    salt_other_action    = detect_col(data, c("d11f", "other_salt_action"), "Other salt control action"),
    # D12 = type of oil/fat, D13 = meals outside home
    oil_type         = detect_col(data, c("d12", "oil_type", "cooking_oil"), "Type of cooking oil/fat"),
    meals_outside    = detect_col(data, c("d13", "meals_outside"), "Meals outside home per week"),

    # -- Step 1: Physical activity GPAQ individual items ----------------------
    # P1-P16 individual items for domain-specific calculations
    pa_work_vig_days  = detect_col(data, c("p2", "pa_work_vig_days"), "Vigorous work days/week"),
    pa_work_vig_hrs   = detect_col(data, c("p3a", "pa_work_vig_hours"), "Vigorous work hours"),
    pa_work_vig_min   = detect_col(data, c("p3b", "pa_work_vig_minutes"), "Vigorous work minutes"),
    pa_work_mod_days  = detect_col(data, c("p5", "pa_work_mod_days"), "Moderate work days/week"),
    pa_work_mod_hrs   = detect_col(data, c("p6a", "pa_work_mod_hours"), "Moderate work hours"),
    pa_work_mod_min   = detect_col(data, c("p6b", "pa_work_mod_minutes"), "Moderate work minutes"),
    pa_transport_days = detect_col(data, c("p8", "pa_transport_days"), "Transport days/week"),
    pa_transport_hrs  = detect_col(data, c("p9a", "pa_transport_hours"), "Transport hours"),
    pa_transport_min  = detect_col(data, c("p9b", "pa_transport_minutes"), "Transport minutes"),
    pa_rec_vig_days   = detect_col(data, c("p11", "pa_rec_vig_days"), "Vigorous recreation days/week"),
    pa_rec_vig_hrs    = detect_col(data, c("p12a", "pa_rec_vig_hours"), "Vigorous recreation hours"),
    pa_rec_vig_min    = detect_col(data, c("p12b", "pa_rec_vig_minutes"), "Vigorous recreation minutes"),
    pa_rec_mod_days   = detect_col(data, c("p14", "pa_rec_mod_days"), "Moderate recreation days/week"),
    pa_rec_mod_hrs    = detect_col(data, c("p15a", "pa_rec_mod_hours"), "Moderate recreation hours"),
    pa_rec_mod_min    = detect_col(data, c("p15b", "pa_rec_mod_minutes"), "Moderate recreation minutes"),
    pa_sedentary_hrs  = detect_col(data, c("p16a", "sedentary_hours"), "Sedentary hours"),
    pa_sedentary_min  = detect_col(data, c("p16b", "sedentary_minutes"), "Sedentary minutes"),

    # -- Step 1: History of NCD diagnosis & treatment (H-codes) ---------------
    # Blood pressure history
    bp_ever_measured = detect_col(data, c("h1", "bp_ever_measured",
                                           "bp_measured"), "BP ever measured"),
    bp_diagnosed    = detect_col(data, c("h2a", "h2", "bp_diagnosed",
                                         "ever_told_bp", "diagnosed_htn"), "Ever diagnosed raised BP"),
    bp_diagnosed_12m = detect_col(data, c("h2b", "bp_diagnosed_12m"), "BP diagnosed past 12m"),
    bp_trad_healer  = detect_col(data, c("h4", "bp_traditional_healer"), "Traditional healer for BP"),
    bp_herbal       = detect_col(data, c("h5", "bp_herbal_remedy"), "Herbal remedy for BP"),

    # Diabetes history
    glucose_ever_measured = detect_col(data, c("h6", "glucose_ever_measured",
                                                "blood_sugar_measured"), "Blood sugar ever measured"),
    dm_diagnosed    = detect_col(data, c("h7a", "h7", "dm_diagnosed",
                                         "ever_told_dm", "diagnosed_dm"), "Ever diagnosed diabetes"),
    dm_diagnosed_12m = detect_col(data, c("h7b", "dm_diagnosed_12m"), "DM diagnosed past 12m"),
    dm_insulin      = detect_col(data, c("h9", "dm_insulin", "insulin_current"), "Currently on insulin"),
    dm_trad_healer  = detect_col(data, c("h10", "dm_traditional_healer"), "Traditional healer for DM"),
    dm_herbal       = detect_col(data, c("h11", "dm_herbal_remedy"), "Herbal remedy for DM"),

    # Cholesterol history
    chol_ever_measured = detect_col(data, c("h12", "chol_ever_measured",
                                             "cholesterol_measured"), "Cholesterol ever measured"),
    chol_diagnosed  = detect_col(data, c("h13a", "h13", "chol_diagnosed",
                                         "ever_told_chol"), "Ever diagnosed raised cholesterol"),
    chol_diagnosed_12m = detect_col(data, c("h13b", "chol_diagnosed_12m"), "Chol diagnosed past 12m"),
    chol_trad_healer = detect_col(data, c("h15", "chol_traditional_healer"), "Traditional healer for chol"),
    chol_herbal     = detect_col(data, c("h16", "chol_herbal_remedy"), "Herbal remedy for chol"),

    # CVD history
    cvd_history     = detect_col(data, c("h17", "cvd_history", "heart_attack_stroke",
                                          "ever_heart_attack"), "CVD history (heart attack/stroke)"),
    cvd_aspirin     = detect_col(data, c("h18", "aspirin_regular", "on_aspirin"), "Currently taking aspirin"),
    cvd_statins     = detect_col(data, c("h19", "statins_regular", "on_statins"), "Currently taking statins"),

    # Lifestyle advice (H20a-f)
    adv_quit_tobacco = detect_col(data, c("h20a", "advised_quit_tobacco"), "Advised: quit tobacco"),
    adv_reduce_salt  = detect_col(data, c("h20b", "advised_reduce_salt"), "Advised: reduce salt"),
    adv_fruit_veg    = detect_col(data, c("h20c", "advised_fruit_veg"), "Advised: eat fruit/veg"),
    adv_reduce_fat   = detect_col(data, c("h20d", "advised_reduce_fat"), "Advised: reduce fat"),
    adv_more_pa      = detect_col(data, c("h20e", "advised_more_pa"), "Advised: more PA"),
    adv_healthy_wt   = detect_col(data, c("h20f", "advised_healthy_weight"), "Advised: healthy weight"),

    # Cervical cancer screening
    cervical_screen  = detect_col(data, c("cx1", "cervical_screening",
                                           "cervical_cancer_screening"), "Cervical cancer screening"),

    # -- Demographics (extended) -----------------------------------------------
    education_years  = detect_col(data, c("c4", "education_years", "edu_years"), "Education years"),
    education_level  = detect_col(data, c("c5", "education_group", "highest_education",
                                           "education_level"), "Highest education level"),
    ethnicity        = detect_col(data, c("c6", "caste_group", "ethnicity", "race"), "Ethnicity/Caste"),
    marital_status   = detect_col(data, c("c7", "marital_status", "marital"), "Marital status"),
    employment       = detect_col(data, c("c8", "employment_status", "employment",
                                           "work_status"), "Employment status"),
    income           = detect_col(data, c("c11", "income", "household_income",
                                           "estimate_hh_income_groups"), "Household income"),

    # -- Step 2: Anthropometry -------------------------------------------------
    # v3.1: M1=height, M2=weight, M3=waist
    # v3.2: M11=height, M12=weight, M14=waist, M15=hip
    height   = detect_col(data, c("m11", "m1", "height", "height_cm"), "Height (cm)"),
    weight   = detect_col(data, c("m12", "m2", "weight_kg", "bodyweight"), "Weight (kg)"),
    waist    = detect_col(data, c("m14", "m3", "waist", "waist_cm",
                                  "waist_circumference"), "Waist circumference (cm)"),
    hip      = detect_col(data, c("m15", "hip", "hip_cm",
                                  "hip_circumference"), "Hip circumference (cm)"),

    # -- Step 2: Blood pressure ------------------------------------------------
    # v3.1: B1/B3/B5 = SBP, B2/B4/B6 = DBP, B7 = meds
    # v3.2: M4a/M5a/M6a = SBP, M4b/M5b/M6b = DBP, M7 = meds
    sbp1 = detect_col(data, c("m4a", "b1", "sbp1", "sys1", "systolic1",
                               "sbp_1", "m4_a"), "SBP reading 1"),
    sbp2 = detect_col(data, c("m5a", "b3", "sbp2", "sys2", "systolic2",
                               "sbp_2", "m5_a"), "SBP reading 2"),
    sbp3 = detect_col(data, c("m6a", "b5", "sbp3", "sys3", "systolic3",
                               "sbp_3", "m6_a"), "SBP reading 3"),
    dbp1 = detect_col(data, c("m4b", "b2", "dbp1", "dia1", "diastolic1",
                               "dbp_1", "m4_b"), "DBP reading 1"),
    dbp2 = detect_col(data, c("m5b", "b4", "dbp2", "dia2", "diastolic2",
                               "dbp_2", "m5_b"), "DBP reading 2"),
    dbp3 = detect_col(data, c("m6b", "b6", "dbp3", "dia3", "diastolic3",
                               "dbp_3", "m6_b"), "DBP reading 3"),
    bp_meds = detect_col(data, c("m7", "h3", "b7", "bp_meds",
                                  "antihypertensive", "htn_meds",
                                  "bp_medication"), "BP medications"),
    # M8 = pregnancy status (for excluding pregnant women from BMI)
    pregnant     = detect_col(data, c("m8", "pregnant", "pregnancy"), "Pregnancy status"),
    # M16a-c = heart rate readings
    heart_rate1  = detect_col(data, c("m16a", "hr1", "heart_rate_1"), "Heart rate reading 1"),
    heart_rate2  = detect_col(data, c("m16b", "hr2", "heart_rate_2"), "Heart rate reading 2"),
    heart_rate3  = detect_col(data, c("m16c", "hr3", "heart_rate_3"), "Heart rate reading 3"),
    # Derived mean heart rate (some datasets pre-compute this)
    mean_hr      = detect_col(data, c("m_hr", "mean_hr", "mean_heart_rate"), "Mean heart rate"),

    # -- Step 3: Biochemical ---------------------------------------------------
    # v3.1: C1=glucose, C5=DM meds, C6=chol, C10=chol meds
    # v3.2: B5=glucose, B6=DM meds, B8=chol, B9=chol meds, B16=trig, B17=HDL
    fasting_glucose = detect_col(data, c("b5", "b5_mmol", "c1_mmol",
                                          "fasting_glucose", "glucose_fasting",
                                          "fbg", "fpg"), "Fasting blood glucose"),
    random_glucose  = detect_col(data, c("c1_rand", "glucose_random", "rbg",
                                          "random_glucose"), "Random blood glucose"),
    fasting_status  = detect_col(data, c("b1", "fasting", "fasting_status",
                                          "c1_fast"), "Fasting status"),
    dm_meds         = detect_col(data, c("b6", "h8", "c5", "dm_meds",
                                          "diabetes_meds", "insulin",
                                          "dm_medication"), "Diabetes medications"),
    total_chol      = detect_col(data, c("b8", "b8_mmol", "c6", "cholesterol",
                                          "total_chol", "tc_mmol",
                                          "total_cholesterol"), "Total cholesterol"),
    hdl_chol        = detect_col(data, c("b17", "b17_mmol", "hdl", "hdl_chol",
                                          "hdl_cholesterol"), "HDL cholesterol"),
    triglycerides   = detect_col(data, c("b16", "b16_mmol", "trig",
                                          "triglycerides", "tg_mmol"), "Triglycerides"),
    chol_meds       = detect_col(data, c("b9", "h14", "c10", "chol_meds",
                                          "statin", "chol_medication"), "Cholesterol medications")
  )

  n_found <- sum(!sapply(cols, is.null))
  message(glue::glue("  \u2192 {n_found}/{length(cols)} columns detected automatically"))
  return(cols)
}

#' Read a column mapping file
#'
#' Reads a filled-in column mapping template (Excel or CSV) and returns a
#' named list suitable for passing to [clean_steps_data()].  The mapping file
#' should have at least two columns: one with the standard variable name
#' (column A) and one with the user's column name (column C in the template,
#' or the third column).
#'
#' This function is the manual alternative to [detect_steps_columns()].
#' Use it when your dataset has non-standard variable names that
#' auto-detection cannot resolve.
#'
#' @param path Path to the filled mapping file (.xlsx or .csv).
#' @param data Optional data frame. If provided, the function validates that
#'   every mapped column actually exists in the data.
#'
#' @return A named list where names are standard variable identifiers
#'   (e.g. \code{"age"}, \code{"sbp1"}) and values are the corresponding
#'   column names in the user's dataset.
#'   Unmapped variables are set to \code{NULL}.
#'
#' @details
#' A blank template can be obtained from
#' \code{system.file("templates", "column_mapping_template.xlsx",
#'                    package = "stepssurvey")}
#' or downloaded from the Shiny app.
#'
#' The function ignores domain-header rows (rows where column A is all-caps
#' with no entry in column C) and skips any row where the user's column name
#' is blank.
#'
#' @examples
#' \dontrun{
#'   cols <- read_column_mapping("my_mapping.xlsx")
#'   raw  <- import_steps_data("survey.dta")
#'   clean <- clean_steps_data(raw, cols)
#' }
#'
#' @export
read_column_mapping <- function(path, data = NULL) {
  if (!file.exists(path)) {
    stop(glue::glue("Mapping file not found: {path}"), call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  mapping_df <- switch(ext,
    "xlsx" = readxl::read_excel(path, sheet = "Column Mapping",
                                col_types = "text"),
    "xls"  = readxl::read_excel(path, sheet = "Column Mapping",
                                col_types = "text"),
    "csv"  = utils::read.csv(path, stringsAsFactors = FALSE,
                              colClasses = "character"),
    stop(glue::glue("Unsupported mapping file format: .{ext}"), call. = FALSE)
  )

  # Expect columns: Standard Variable (1), Description (2), Your Column Name (3)
  if (ncol(mapping_df) < 3) {
    stop("Mapping file must have at least 3 columns: ",
         "Standard Variable, Description, Your Column Name", call. = FALSE)
  }

  # Use positional columns (robust to header renaming)
  std_col  <- mapping_df[[1]]
  user_col <- mapping_df[[3]]

  # Build the cols list
  cols <- list()
  n_mapped <- 0
  n_missing <- 0
  missing_vars <- character(0)

  for (i in seq_along(std_col)) {
    std_name <- trimws(as.character(std_col[i]))
    usr_name <- trimws(as.character(user_col[i]))

    # Skip blank rows, domain headers (all-caps with no mapping), NA values
    if (is.na(std_name) || nchar(std_name) == 0) next
    if (std_name == toupper(std_name) && grepl("[A-Z]", std_name)) next
    if (is.na(usr_name) || nchar(usr_name) == 0) {
      cols[[std_name]] <- NULL
      next
    }

    # Validate against actual data if provided
    if (!is.null(data) && !(usr_name %in% names(data))) {
      missing_vars <- c(missing_vars, usr_name)
      n_missing <- n_missing + 1
    }

    cols[[std_name]] <- usr_name
    n_mapped <- n_mapped + 1
  }

  message(glue::glue("  \u2713 Column mapping loaded: {n_mapped} variables mapped"))


  if (n_missing > 0) {
    warning(glue::glue(
      "{n_missing} mapped column(s) not found in data: ",
      "{paste(missing_vars, collapse = ', ')}"
    ), call. = FALSE)
  }

  # Ensure all standard names that exist in detect_steps_columns are present
  # (as NULL if not mapped), so downstream code can safely check cols$xyz
  all_standard <- c(
    "age", "sex", "agerange", "valid", "urban_rural",
    "weight_step1", "weight_step2", "weight_step3", "strata", "psu",
    "tobacco_current", "tobacco_daily", "tobacco_start_age", "tobacco_duration",
    "tobacco_cig_day", "tobacco_cig_week", "tobacco_hand_day",
    "tobacco_pipe_day", "tobacco_cigar_day", "tobacco_shisha_day",
    "tobacco_quit_attempt", "tobacco_quit_advice", "tobacco_past",
    "tobacco_past_daily", "tobacco_quit_age", "tobacco_quit_duration",
    "smokeless_current", "smokeless_daily", "slt_snuff_mouth",
    "slt_chewing", "slt_betel", "smokeless_past", "smokeless_past_daily",
    "secondhand_home", "secondhand_work", "any_tobacco",
    "alcohol_ever", "alcohol_12m", "alcohol_current", "heavy_episode",
    "alcohol_stopped", "alcohol_freq_12m", "alcohol_occasions",
    "alcohol_drinks_occasion", "alcohol_largest_drinks",
    "alcohol_6plus_count", "alcohol_7day_freq", "alcohol_homebrew",
    "drinking_level",
    "met_total", "pa_work_vig", "pa_work_mod", "pa_transport",
    "pa_rec_vig", "pa_rec_mod", "pa_sedentary",
    "fruit_days", "fruit_servings", "veg_days", "veg_servings",
    "salt_table", "salt_cooking", "processed_salt", "salt_perception",
    "salt_importance", "salt_knowledge",
    "salt_limit_processed", "salt_check_labels", "salt_buy_low",
    "salt_use_spices", "salt_avoid_outside", "salt_other_action",
    "oil_type", "meals_outside",
    "pa_work_vig_days", "pa_work_vig_hrs", "pa_work_vig_min",
    "pa_work_mod_days", "pa_work_mod_hrs", "pa_work_mod_min",
    "pa_transport_days", "pa_transport_hrs", "pa_transport_min",
    "pa_rec_vig_days", "pa_rec_vig_hrs", "pa_rec_vig_min",
    "pa_rec_mod_days", "pa_rec_mod_hrs", "pa_rec_mod_min",
    "pa_sedentary_hrs", "pa_sedentary_min",
    "bp_ever_measured", "bp_diagnosed", "bp_diagnosed_12m",
    "bp_trad_healer", "bp_herbal",
    "glucose_ever_measured", "dm_diagnosed", "dm_diagnosed_12m",
    "dm_insulin", "dm_trad_healer", "dm_herbal",
    "chol_ever_measured", "chol_diagnosed", "chol_diagnosed_12m",
    "chol_trad_healer", "chol_herbal",
    "cvd_history", "cvd_aspirin", "cvd_statins",
    "adv_quit_tobacco", "adv_reduce_salt", "adv_fruit_veg",
    "adv_reduce_fat", "adv_more_pa", "adv_healthy_wt",
    "cervical_screen",
    "education_years", "education_level", "ethnicity",
    "marital_status", "employment", "income",
    "height", "weight", "waist", "hip",
    "sbp1", "sbp2", "sbp3", "dbp1", "dbp2", "dbp3",
    "bp_meds", "pregnant", "heart_rate1", "heart_rate2", "heart_rate3",
    "mean_hr",
    "fasting_glucose", "random_glucose", "fasting_status",
    "dm_meds", "total_chol", "hdl_chol", "triglycerides", "chol_meds"
  )

  for (nm in all_standard) {
    if (!(nm %in% names(cols))) {
      cols[[nm]] <- NULL
    }
  }

  return(cols)
}

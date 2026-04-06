#' WHO STEPS Data Book Table Registry
#'
#' Defines all standard tables from the WHO STEPS Epi Info report template.
#' Each entry specifies the table metadata; generic compute and formatting
#' functions use this registry to produce the full data book automatically.
#'
#' @section Table types:
#' \describe{
#'   \item{proportion}{Single binary indicator: % (95% CI) by age × sex.
#'     Most common type. Example: "Current smokers among all respondents."}
#'   \item{mean}{Continuous variable: mean (95% CI) by age × sex.
#'     Example: "Mean BMI (kg/m²)."}
#'   \item{category}{Multi-level factor: % per level (95% CI) by age × sex.
#'     Example: "BMI classifications (Underweight / Normal / Overweight / Obese)."}
#'   \item{cascade}{Diagnosis → treatment → control chain: multiple proportions
#'     with nested denominators. Example: "Raised BP diagnosis, treatment and control."}
#'   \item{combined}{Summary of combined risk factors: 0, 1-2, 3-5 risk factors.}
#' }
#'
#' @section Registry fields:
#' \describe{
#'   \item{id}{Unique short identifier (e.g., "T_smoking_current").}
#'   \item{section}{Data book section (e.g., "Tobacco Use", "Blood Pressure").}
#'   \item{step}{STEPS step number (1, 2, or 3).}
#'   \item{title}{Table title as shown in the data book.}
#'   \item{description}{One-line description from the WHO template.}
#'   \item{type}{One of: "proportion", "mean", "category", "cascade", "combined".}
#'   \item{variable}{Column name(s) in the cleaned data frame to analyse.
#'     For proportion: single logical variable.
#'     For mean: single numeric variable.
#'     For category: single factor variable.
#'     For cascade: named list of logical variables.}
#'   \item{denominator}{NULL (= all respondents) or column name for subsetting
#'     (e.g., "current_alcohol" to restrict to drinkers).}
#'   \item{levels}{For category type: named character vector of level labels.}
#'   \item{epi_info}{Epi Info program name(s) for reference.}
#'   \item{unit}{Display unit (e.g., "%", "mmHg", "cm", "kg/m²", "mmol/L").}
#'   \item{questions}{STEPS instrument question codes used.}
#'   \item{sex_panels}{Logical. TRUE = 3 panels (Men/Women/Both);
#'     FALSE = 2 panels (Men/Women only, e.g., height/weight means).
#'     Default TRUE.}
#' }
#'
#' @return A list of table specification lists.
#' @export
steps_table_registry <- function() {
  list(

    # =========================================================================
    # SECTION: TOBACCO USE (Step 1)
    # =========================================================================

    list(
      id          = "T_smoking_current",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Current smokers",
      description = "Current smokers among all respondents.",
      type        = "proportion",
      variable    = "current_smoker",
      denominator = NULL,
      epi_info    = "TsmokestatusWT",
      unit        = "%",
      questions   = "T1, T2, T8"
    ),

    list(
      id          = "T_smoking_status",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Smoking status",
      description = "Smoking status of all respondents.",
      type        = "category",
      variable    = "smoking_status",
      denominator = NULL,
      levels      = c("Daily" = "daily_smoker",
                       "Non-daily" = "nondaily_smoker",
                       "Former smoker" = "former_smoker",
                       "Never smoker" = "never_smoker"),
      epi_info    = "TsmokestatusWT",
      unit        = "%",
      questions   = "T1, T2, T8"
    ),

    list(
      id          = "T_smoking_daily_among_smokers",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Current daily smokers among smokers",
      description = "Percentage of current daily smokers among smokers.",
      type        = "proportion",
      variable    = "daily_tobacco",
      denominator = "current_tobacco",
      epi_info    = "TsmokestatusWT",
      unit        = "%",
      questions   = "T1, T2"
    ),

    list(
      id          = "T_smoking_age_duration",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Mean age of initiation and duration of smoking",
      description = "Mean age started smoking and mean duration among daily smokers.",
      type        = "mean",
      variable    = c("smoking_start_age", "smoking_duration"),
      denominator = "daily_tobacco",
      epi_info    = "TagedurationWT",
      unit        = "years",
      questions   = "T3, T4",
      sex_panels  = FALSE
    ),

    list(
      id          = "T_smoking_manufactured_cig",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Type of tobacco smoked",
      description = "Percentage of smokers who use manufactured cigarettes.",
      type        = "proportion",
      variable    = "smokes_manufactured_cig",
      denominator = "current_smoker",
      epi_info    = "TtypeWT",
      unit        = "%",
      questions   = "T5a-T5f"
    ),

    list(
      id          = "T_smoking_mean_amount",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Mean amount of tobacco used by daily smokers by type",
      description = "Mean amount of tobacco used by daily smokers per day, by type.",
      type        = "mean",
      variable    = c("cig_per_day_manufactured", "cig_per_day_handrolled"),
      denominator = "daily_tobacco",
      epi_info    = "TmeanamountWT",
      unit        = "per day",
      questions   = "T5a-T5f"
    ),

    list(
      id          = "T_smoking_quantity_cat",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Daily cigarette smoking quantities",
      description = "Percentage of daily cigarette smokers smoking given quantities.",
      type        = "category",
      variable    = "cigarettes_per_day_cat",
      denominator = "daily_tobacco",
      levels      = c("<5" = "cig_lt5", "5-9" = "cig_5_9",
                       "10-14" = "cig_10_14", "15-24" = "cig_15_24",
                       "25+" = "cig_25plus"),
      epi_info    = "TamountWT",
      unit        = "%",
      questions   = "T5a, T5b"
    ),

    list(
      id          = "T_smoking_former",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Former daily smokers",
      description = "Percentage of former daily smokers among all respondents and among ever daily smokers.",
      type        = "proportion",
      variable    = "former_daily_smoker",
      denominator = NULL,
      epi_info    = "TformerWT",
      unit        = "%",
      questions   = "T8, T9, T10, T11"
    ),

    list(
      id          = "T_smoking_quit_attempt",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Quit attempt",
      description = "Percentage of current smokers who tried to stop smoking during past 12 months.",
      type        = "proportion",
      variable    = "quit_attempt",
      denominator = "current_smoker",
      epi_info    = "TquittingWT",
      unit        = "%",
      questions   = "T6"
    ),

    list(
      id          = "T_smoking_advice",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Advice to quit from doctor/health worker",
      description = "Percentage of current smokers advised to quit by doctor or health worker.",
      type        = "proportion",
      variable    = "quit_advice",
      denominator = "current_smoker",
      epi_info    = "TadviceWT",
      unit        = "%",
      questions   = "T7"
    ),

    list(
      id          = "T_smokeless_current",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Current smokeless tobacco users",
      description = "Current users of smokeless tobacco among all respondents.",
      type        = "proportion",
      variable    = "current_smokeless",
      denominator = NULL,
      epi_info    = "TsmokelessstatusWT",
      unit        = "%",
      questions   = "T12"
    ),

    list(
      id          = "T_tobacco_current_any",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Current tobacco users (smoking and/or smokeless)",
      description = "Percentage of daily and current tobacco users (smoking and/or smokeless).",
      type        = "proportion",
      variable    = "current_tobacco_any",
      denominator = NULL,
      epi_info    = "TanytobaccoWT",
      unit        = "%",
      questions   = "T1, T2, T12"
    ),

    list(
      id          = "T_secondhand_home",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Exposure to second-hand smoke at home",
      description = "Percentage of respondents exposed to second-hand smoke in the home.",
      type        = "proportion",
      variable    = "secondhand_home",
      denominator = NULL,
      epi_info    = "TsecondhandWT",
      unit        = "%",
      questions   = "T17"
    ),

    list(
      id          = "T_secondhand_work",
      section     = "Tobacco Use",
      step        = 1,
      title       = "Exposure to second-hand smoke at workplace",
      description = "Percentage of respondents exposed to second-hand smoke at workplace.",
      type        = "proportion",
      variable    = "secondhand_work",
      denominator = NULL,
      epi_info    = "TsecondhandWT",
      unit        = "%",
      questions   = "T18"
    ),

    # =========================================================================
    # SECTION: ALCOHOL CONSUMPTION (Step 1)
    # =========================================================================

    list(
      id          = "A_consumption_status",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Alcohol consumption status",
      description = "Alcohol consumption status of all respondents.",
      type        = "category",
      variable    = "alcohol_status",
      denominator = NULL,
      levels      = c("Lifetime abstainer" = "lifetime_abstainer",
                       "Past 12 months abstainer" = "past_12m_abstainer",
                       "Current (past 30 days)" = "current_alcohol",
                       "Drank past 12m not current" = "past_12m_not_current"),
      epi_info    = "AstatusWT",
      unit        = "%",
      questions   = "A1, A2, A5"
    ),

    list(
      id          = "A_frequency_12m",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Frequency of alcohol consumption in past 12 months",
      description = "Frequency of alcohol consumption in past 12 months among current drinkers.",
      type        = "category",
      variable    = "alcohol_freq_12m",
      denominator = "alcohol_12m",
      levels      = c("Daily" = "daily_drinker",
                       "5-6 per week" = "freq_5_6",
                       "3-4 per week" = "freq_3_4",
                       "1-2 per week" = "freq_1_2",
                       "1-3 per month" = "freq_1_3m",
                       "Less than monthly" = "freq_lt_monthly"),
      epi_info    = "Afrequency12WT",
      unit        = "%",
      questions   = "A4"
    ),

    list(
      id          = "A_mean_occasions_30d",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Mean drinking occasions in past 30 days",
      description = "Mean number of occasions with at least one drink in past 30 days among current drinkers.",
      type        = "mean",
      variable    = "drinking_occasions_30d",
      denominator = "current_alcohol",
      epi_info    = "AmeanoccasionsWT",
      unit        = "occasions",
      questions   = "A6"
    ),

    list(
      id          = "A_mean_drinks_occasion",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Mean drinks per drinking occasion",
      description = "Mean number of standard drinks consumed on a drinking occasion among current drinkers.",
      type        = "mean",
      variable    = "drinks_per_occasion",
      denominator = "current_alcohol",
      epi_info    = "AmeandrinkWT",
      unit        = "drinks",
      questions   = "A7"
    ),

    list(
      id          = "A_drinking_level",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Drinking levels among all respondents",
      description = "Percentage of respondents with different drinking levels.",
      type        = "category",
      variable    = "drinking_level",
      denominator = NULL,
      levels      = c("High-end" = "drinking_high",
                       "Intermediate" = "drinking_intermediate",
                       "Lower-end" = "drinking_lower",
                       "Non-drinker" = "non_drinker_30d"),
      epi_info    = "AdrinkinglevelWT",
      unit        = "%",
      questions   = "A5, A7"
    ),

    list(
      id          = "A_heavy_episodic",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Heavy episodic drinking (6+ drinks)",
      description = "Percentage of respondents who had 6+ drinks on any occasion in past 30 days.",
      type        = "proportion",
      variable    = "heavy_episodic",
      denominator = NULL,
      epi_info    = "AheavyepisodicWT",
      unit        = "%",
      questions   = "A9"
    ),

    list(
      id          = "A_7day_frequency",
      section     = "Alcohol Consumption",
      step        = 1,
      title       = "Frequency of alcohol consumption in past 7 days",
      description = "Frequency of alcohol consumption in past 7 days by current drinkers.",
      type        = "category",
      variable    = "alcohol_7day_freq_cat",
      denominator = "current_alcohol",
      levels      = c("0 days" = "drink_0d", "1-2 days" = "drink_1_2d",
                       "3-4 days" = "drink_3_4d", "5-6 days" = "drink_5_6d",
                       "Daily" = "drink_7d"),
      epi_info    = "A7dayfreqWT",
      unit        = "%",
      questions   = "A10a-A10g"
    ),

    # =========================================================================
    # SECTION: DIET (Step 1)
    # =========================================================================

    list(
      id          = "D_mean_fruit_veg_days",
      section     = "Diet",
      step        = 1,
      title       = "Mean days of fruit and vegetable consumption",
      description = "Mean number of days fruit and vegetables consumed in a typical week.",
      type        = "mean",
      variable    = c("fruit_days", "veg_days"),
      denominator = NULL,
      epi_info    = "DmeanfruitvegWT",
      unit        = "days",
      questions   = "D1, D3"
    ),

    list(
      id          = "D_mean_servings",
      section     = "Diet",
      step        = 1,
      title       = "Mean servings of fruit and vegetables per day",
      description = "Mean number of fruit, vegetable, and combined servings per day.",
      type        = "mean",
      variable    = c("avg_fruit_servings", "avg_veg_servings", "avg_fruit_veg_servings"),
      denominator = NULL,
      epi_info    = "DmeanservingsWT",
      unit        = "servings/day",
      questions   = "D1-D4"
    ),

    list(
      id          = "D_low_fruit_veg",
      section     = "Diet",
      step        = 1,
      title       = "Low fruit and vegetable consumption",
      description = "Percentage eating less than 5 servings of fruit and/or vegetables per day.",
      type        = "proportion",
      variable    = "low_fruit_veg",
      denominator = NULL,
      epi_info    = "DfruitvegWT",
      unit        = "%",
      questions   = "D1-D4"
    ),

    list(
      id          = "D_salt_add",
      section     = "Diet",
      step        = 1,
      title       = "Salt added to food",
      description = "Percentage who always or often add salt or salty sauce to food.",
      type        = "proportion",
      variable    = "always_add_salt",
      denominator = NULL,
      epi_info    = "DsaltWT",
      unit        = "%",
      questions   = "D5"
    ),

    list(
      id          = "D_processed_food",
      section     = "Diet",
      step        = 1,
      title       = "Processed food high in salt",
      description = "Percentage who always or often eat processed food high in salt.",
      type        = "proportion",
      variable    = "always_processed_salt",
      denominator = NULL,
      epi_info    = "DsaltWT",
      unit        = "%",
      questions   = "D7"
    ),

    list(
      id          = "D_salt_perception",
      section     = "Diet",
      step        = 1,
      title       = "Salt intake perception",
      description = "Percentage who think they consume far too much or too much salt.",
      type        = "proportion",
      variable    = "perceived_excess_salt",
      denominator = NULL,
      epi_info    = "DsaltperceptionWT",
      unit        = "%",
      questions   = "D8"
    ),

    # =========================================================================
    # SECTION: PHYSICAL ACTIVITY (Step 1)
    # =========================================================================

    list(
      id          = "P_insufficient_pa",
      section     = "Physical Activity",
      step        = 1,
      title       = "Not meeting WHO recommendations on physical activity",
      description = "Percentage not meeting WHO recommendations on physical activity for health.",
      type        = "proportion",
      variable    = "insufficient_pa",
      denominator = NULL,
      epi_info    = "PrecommendationsWT",
      unit        = "%",
      questions   = "P1-P15b"
    ),

    list(
      id          = "P_activity_level",
      section     = "Physical Activity",
      step        = 1,
      title       = "Level of total physical activity",
      description = "Percentage classified as Low, Moderate, or High physical activity.",
      type        = "category",
      variable    = "pa_category",
      denominator = NULL,
      levels      = c("Low" = "pa_low", "Moderate" = "pa_moderate", "High" = "pa_high"),
      epi_info    = "PlevelWT",
      unit        = "%",
      questions   = "P1-P15b"
    ),

    list(
      id          = "P_mean_minutes",
      section     = "Physical Activity",
      step        = 1,
      title       = "Mean minutes of total physical activity per day",
      description = "Mean minutes of total physical activity on average per day.",
      type        = "mean",
      variable    = "pa_minutes_per_day",
      denominator = NULL,
      epi_info    = "PtotalminutesWT",
      unit        = "minutes/day",
      questions   = "P1-P15b"
    ),

    list(
      id          = "P_mean_by_domain",
      section     = "Physical Activity",
      step        = 1,
      title       = "Mean minutes of physical activity by domain",
      description = "Mean minutes spent in work-, transport-, and recreation-related PA per day.",
      type        = "mean",
      variable    = c("pa_work_min_day", "pa_transport_min_day", "pa_recreation_min_day"),
      denominator = NULL,
      epi_info    = "PsetspecificWT",
      unit        = "minutes/day",
      questions   = "P1-P15b"
    ),

    list(
      id          = "P_sedentary",
      section     = "Physical Activity",
      step        = 1,
      title       = "Sedentary behaviour",
      description = "Mean minutes spent in sedentary activities on a typical day.",
      type        = "mean",
      variable    = "sedentary_min_day",
      denominator = NULL,
      epi_info    = "PsedentaryWT",
      unit        = "minutes/day",
      questions   = "P16a, P16b"
    ),

    list(
      id          = "P_no_vigorous",
      section     = "Physical Activity",
      step        = 1,
      title       = "No vigorous physical activity",
      description = "Percentage of respondents not engaging in vigorous physical activity.",
      type        = "proportion",
      variable    = "no_vigorous_pa",
      denominator = NULL,
      epi_info    = "PnovigorousWT",
      unit        = "%",
      questions   = "P1, P10"
    ),

    # =========================================================================
    # SECTION: HISTORY OF RAISED BP / DIABETES / CHOLESTEROL / CVD (Step 1)
    # =========================================================================

    list(
      id          = "H_bp_measurement",
      section     = "History of Raised Blood Pressure",
      step        = 1,
      title       = "Blood pressure measurement and diagnosis",
      description = "BP measurement and diagnosis among all respondents.",
      type        = "cascade",
      variable    = list(
        "Ever measured"       = "bp_ever_measured",
        "Diagnosed"           = "bp_diagnosed",
        "Diagnosed past 12m"  = "bp_diagnosed_12m"
      ),
      denominator = NULL,
      epi_info    = "HbloodpressureWT",
      unit        = "%",
      questions   = "H1, H2a, H2b"
    ),

    list(
      id          = "H_bp_treatment",
      section     = "History of Raised Blood Pressure",
      step        = 1,
      title       = "Raised BP treatment among those diagnosed",
      description = "Treatment results among those previously diagnosed with raised BP.",
      type        = "proportion",
      variable    = "on_bp_meds",
      denominator = "bp_diagnosed",
      epi_info    = "HbloodpressureWT",
      unit        = "%",
      questions   = "H2a, H3"
    ),

    list(
      id          = "H_dm_measurement",
      section     = "History of Diabetes",
      step        = 1,
      title       = "Blood sugar measurement and diagnosis",
      description = "Blood sugar measurement and diagnosis among all respondents.",
      type        = "cascade",
      variable    = list(
        "Ever measured"       = "glucose_ever_measured",
        "Diagnosed"           = "dm_diagnosed",
        "Diagnosed past 12m"  = "dm_diagnosed_12m"
      ),
      denominator = NULL,
      epi_info    = "HdiabetesWT",
      unit        = "%",
      questions   = "H6, H7a, H7b"
    ),

    list(
      id          = "H_dm_treatment",
      section     = "History of Diabetes",
      step        = 1,
      title       = "Diabetes treatment among those diagnosed",
      description = "Treatment results among those previously diagnosed with diabetes.",
      type        = "cascade",
      variable    = list(
        "On medication" = "on_dm_meds",
        "On insulin"    = "on_dm_insulin"
      ),
      denominator = "dm_diagnosed",
      epi_info    = "HdiabetesWT",
      unit        = "%",
      questions   = "H7a, H8, H9"
    ),

    list(
      id          = "H_chol_measurement",
      section     = "History of Raised Total Cholesterol",
      step        = 1,
      title       = "Total cholesterol measurement and diagnosis",
      description = "Total cholesterol measurement and diagnosis among all respondents.",
      type        = "cascade",
      variable    = list(
        "Ever measured"       = "chol_ever_measured",
        "Diagnosed"           = "chol_diagnosed",
        "Diagnosed past 12m"  = "chol_diagnosed_12m"
      ),
      denominator = NULL,
      epi_info    = "HcholWT",
      unit        = "%",
      questions   = "H12, H13a, H13b"
    ),

    list(
      id          = "H_chol_treatment",
      section     = "History of Raised Total Cholesterol",
      step        = 1,
      title       = "Cholesterol treatment among those diagnosed",
      description = "Treatment results among those previously diagnosed with raised cholesterol.",
      type        = "proportion",
      variable    = "on_chol_meds",
      denominator = "chol_diagnosed",
      epi_info    = "HcholWT",
      unit        = "%",
      questions   = "H13a, H14"
    ),

    list(
      id          = "H_cvd_history",
      section     = "History of Cardiovascular Diseases",
      step        = 1,
      title       = "History of heart attack, angina, or stroke",
      description = "Percentage who have ever had a heart attack, chest pain from heart disease, or stroke.",
      type        = "proportion",
      variable    = "cvd_history",
      denominator = NULL,
      epi_info    = "HcvdWT",
      unit        = "%",
      questions   = "H17"
    ),

    list(
      id          = "H_cvd_meds",
      section     = "History of Cardiovascular Diseases",
      step        = 1,
      title       = "CVD medication (aspirin and/or statins)",
      description = "Currently taking aspirin or statins regularly among those with CVD history.",
      type        = "cascade",
      variable    = list(
        "Taking aspirin" = "on_aspirin",
        "Taking statins" = "on_statins"
      ),
      denominator = "cvd_history",
      epi_info    = "HcvdmedsWT",
      unit        = "%",
      questions   = "H17, H18, H19"
    ),

    list(
      id          = "H_lifestyle_advice",
      section     = "History of Cardiovascular Diseases",
      step        = 1,
      title       = "Lifestyle advice from doctor or health worker",
      description = "Percentage advised to quit tobacco, reduce salt, eat fruit/veg, reduce fat, be active, maintain healthy weight.",
      type        = "cascade",
      variable    = list(
        "Quit tobacco"         = "advised_quit_tobacco",
        "Reduce salt"          = "advised_reduce_salt",
        "Eat fruit/veg"        = "advised_fruit_veg",
        "Reduce fat"           = "advised_reduce_fat",
        "Be more active"       = "advised_more_pa",
        "Maintain healthy wt"  = "advised_healthy_weight"
      ),
      denominator = NULL,
      epi_info    = "HlifestyleWT",
      unit        = "%",
      questions   = "H20a-H20f"
    ),

    list(
      id          = "H_cervical_screening",
      section     = "Cervical Cancer Screening",
      step        = 1,
      title       = "Cervical cancer screening",
      description = "Percentage of female respondents who have ever had cervical cancer screening.",
      type        = "proportion",
      variable    = "cervical_screened",
      denominator = NULL,
      sex_filter  = "female",
      epi_info    = "HcervcancerWT",
      unit        = "%",
      questions   = "CX1"
    ),

    # =========================================================================
    # SECTION: PHYSICAL MEASUREMENTS — BLOOD PRESSURE (Step 2)
    # =========================================================================

    list(
      id          = "M_bp_mean",
      section     = "Blood Pressure",
      step        = 2,
      title       = "Mean blood pressure",
      description = "Mean systolic and diastolic blood pressure including those on medication.",
      type        = "mean",
      variable    = c("mean_sbp", "mean_dbp"),
      denominator = NULL,
      epi_info    = "MbloodpressureWT",
      unit        = "mmHg",
      questions   = "M4a-M6b"
    ),

    list(
      id          = "M_bp_raised",
      section     = "Blood Pressure",
      step        = 2,
      title       = "Raised blood pressure",
      description = "Percentage with raised blood pressure (SBP>=140 and/or DBP>=90 or on medication).",
      type        = "proportion",
      variable    = "raised_bp",
      denominator = NULL,
      epi_info    = "MraisedbpWT",
      unit        = "%",
      questions   = "M4a-M6b, M7"
    ),

    list(
      id          = "M_bp_cascade",
      section     = "Blood Pressure",
      step        = 2,
      title       = "Raised BP diagnosis, treatment and control",
      description = "Diagnosis, treatment, and control among those with raised BP or on medication.",
      type        = "cascade",
      variable    = list(
        "Previously diagnosed"           = "bp_diagnosed",
        "On medication (among diagnosed)" = "on_bp_meds",
        "BP controlled (SBP<140 & DBP<90 among treated)" = "bp_controlled"
      ),
      denominator = "raised_bp",
      epi_info    = "MraisedbpWT",
      unit        = "%",
      questions   = "H1, H2a, H3, M4a-M6b, M7"
    ),

    list(
      id          = "M_heart_rate",
      section     = "Blood Pressure",
      step        = 2,
      title       = "Mean heart rate",
      description = "Mean heart rate (beats per minute).",
      type        = "mean",
      variable    = "mean_heart_rate",
      denominator = NULL,
      epi_info    = "MheartrateWT",
      unit        = "bpm",
      questions   = "M16a-M16c"
    ),

    # =========================================================================
    # SECTION: PHYSICAL MEASUREMENTS — ANTHROPOMETRY (Step 2)
    # =========================================================================

    list(
      id          = "M_height_weight_bmi",
      section     = "Anthropometry",
      step        = 2,
      title       = "Mean height, weight, and BMI",
      description = "Mean height, weight, and BMI excluding pregnant women.",
      type        = "mean",
      variable    = c("height_cm", "weight_kg", "bmi"),
      denominator = NULL,
      epi_info    = "MbmiWT",
      unit        = c("cm", "kg", "kg/m\u00B2"),
      questions   = "M8, M11, M12"
    ),

    list(
      id          = "M_bmi_categories",
      section     = "Anthropometry",
      step        = 2,
      title       = "BMI classifications",
      description = "Percentage in each BMI category excluding pregnant women.",
      type        = "category",
      variable    = "bmi_category",
      denominator = NULL,
      levels      = c("Underweight (<18.5)" = "underweight",
                       "Normal (18.5-24.9)"  = "normal_weight",
                       "Overweight (25-29.9)" = "overweight_only",
                       "Obese (>=30)"         = "obese"),
      epi_info    = "MbmiclassWT",
      unit        = "%",
      questions   = "M8, M11, M12"
    ),

    list(
      id          = "M_bmi_overweight",
      section     = "Anthropometry",
      step        = 2,
      title       = "Overweight (BMI >= 25)",
      description = "Percentage classified as overweight (BMI >= 25) excluding pregnant women.",
      type        = "proportion",
      variable    = "overweight_obese",
      denominator = NULL,
      epi_info    = "MbmiclassWT",
      unit        = "%",
      questions   = "M8, M11, M12"
    ),

    list(
      id          = "M_waist",
      section     = "Anthropometry",
      step        = 2,
      title       = "Mean waist circumference",
      description = "Mean waist circumference excluding pregnant women.",
      type        = "mean",
      variable    = "waist_cm",
      denominator = NULL,
      epi_info    = "MwaistWT",
      unit        = "cm",
      questions   = "M14"
    ),

    list(
      id          = "M_hip",
      section     = "Anthropometry",
      step        = 2,
      title       = "Mean hip circumference",
      description = "Mean hip circumference excluding pregnant women.",
      type        = "mean",
      variable    = "hip_cm",
      denominator = NULL,
      epi_info    = "MhipWT",
      unit        = "cm",
      questions   = "M15"
    ),

    list(
      id          = "M_waist_hip_ratio",
      section     = "Anthropometry",
      step        = 2,
      title       = "Mean waist-to-hip ratio",
      description = "Mean waist-to-hip ratio excluding pregnant women.",
      type        = "mean",
      variable    = "waist_hip_ratio",
      denominator = NULL,
      epi_info    = "MwaisthipratioWT",
      unit        = "",
      questions   = "M14, M15"
    ),

    # =========================================================================
    # SECTION: BIOCHEMICAL MEASUREMENTS (Step 3)
    # =========================================================================

    list(
      id          = "B_glucose_mean",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Mean fasting blood glucose",
      description = "Mean fasting blood glucose including those on medication.",
      type        = "mean",
      variable    = "fasting_glucose",
      denominator = NULL,
      epi_info    = "BglucoseWT",
      unit        = "mmol/L",
      questions   = "B5, B6"
    ),

    list(
      id          = "B_glucose_raised",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Raised blood glucose",
      description = "Categorization of blood glucose levels and percentage on medication.",
      type        = "cascade",
      variable    = list(
        "Impaired (6.1-6.9 mmol/L)"       = "impaired_glucose",
        "Raised (>=7.0 mmol/L) or on meds" = "raised_glucose",
        "On medication"                     = "on_dm_meds"
      ),
      denominator = NULL,
      epi_info    = "BglucoseWT",
      unit        = "%",
      questions   = "B5, B6, H8, H9"
    ),

    list(
      id          = "B_glucose_cascade",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Raised blood glucose diagnosis and treatment",
      description = "Raised blood glucose diagnosis and treatment among all respondents.",
      type        = "cascade",
      variable    = list(
        "Raised glucose or on meds"          = "raised_glucose",
        "Previously diagnosed"               = "dm_diagnosed",
        "Diagnosed & on medication"          = "on_dm_meds",
        "Diagnosed & on insulin"             = "on_dm_insulin",
        "Not previously diagnosed (new)"     = "dm_new"
      ),
      denominator = NULL,
      epi_info    = "BglucoseWT",
      unit        = "%",
      questions   = "B5, B6, H6-H9"
    ),

    list(
      id          = "B_chol_mean",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Mean total cholesterol",
      description = "Mean total cholesterol including those on medication.",
      type        = "mean",
      variable    = "total_chol",
      denominator = NULL,
      epi_info    = "BtotallipidsWT",
      unit        = "mmol/L",
      questions   = "B8"
    ),

    list(
      id          = "B_chol_raised",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Raised total cholesterol",
      description = "Percentage with raised total cholesterol at two thresholds.",
      type        = "cascade",
      variable    = list(
        ">= 5.0 mmol/L (190 mg/dL)"            = "raised_chol",
        ">= 6.2 mmol/L (240 mg/dL)"            = "raised_chol_high",
        ">= 5.0 or on medication"               = "raised_chol_or_meds",
        ">= 6.2 or on medication"               = "raised_chol_high_or_meds"
      ),
      denominator = NULL,
      epi_info    = "BraisedcholWT",
      unit        = "%",
      questions   = "B8, B9, H14"
    ),

    list(
      id          = "B_hdl",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Mean HDL cholesterol",
      description = "Mean HDL cholesterol and percentage with low HDL.",
      type        = "mean",
      variable    = "hdl_chol",
      denominator = NULL,
      epi_info    = "BhdlipidsWT",
      unit        = "mmol/L",
      questions   = "B17"
    ),

    list(
      id          = "B_triglycerides",
      section     = "Biochemical Measurements",
      step        = 3,
      title       = "Mean fasting triglycerides",
      description = "Mean fasting triglycerides and percentage with raised triglycerides.",
      type        = "mean",
      variable    = "triglycerides",
      denominator = NULL,
      epi_info    = "BtriglycerideWT",
      unit        = "mmol/L",
      questions   = "B16"
    ),

    # =========================================================================
    # SECTION: CARDIOVASCULAR DISEASE RISK (Steps 1-3)
    # =========================================================================

    list(
      id          = "R_cvd_risk_10yr",
      section     = "Cardiovascular Disease Risk",
      step        = 3,
      title       = "10-year cardiovascular disease risk",
      description = "10-year CVD risk distribution among respondents aged 40-69.",
      type        = "category",
      variable    = "cvd_risk_category",
      denominator = NULL,
      levels      = c("<10%" = "cvd_low", "10-<20%" = "cvd_medium",
                       "20-<30%" = "cvd_high", ">=30%" = "cvd_very_high"),
      epi_info    = "RcvdriskWT",
      unit        = "%",
      questions   = "Combined"
    ),

    # =========================================================================
    # SECTION: COMBINED RISK FACTORS (Steps 1+2)
    # =========================================================================

    list(
      id          = "RF_combined",
      section     = "Summary of Combined Risk Factors",
      step        = 2,
      title       = "Summary of combined risk factors",
      description = "Percentage with 0, 1-2, or 3-5 of 5 key risk factors.",
      type        = "combined",
      variable    = list(
        "Current daily smoking"        = "daily_tobacco",
        "Less than 5 servings fruit/veg" = "low_fruit_veg",
        "Insufficient PA"              = "insufficient_pa",
        "Overweight/obese (BMI>=25)"   = "overweight_obese",
        "Raised BP"                    = "raised_bp"
      ),
      denominator = NULL,
      epi_info    = "RaisedriskWT",
      unit        = "%",
      questions   = "T1, T2, D1-D4, P1-P15b, M4a-M6b, M7, M8, M11, M12"
    )
  )
}


#' Get table registry entries by section
#'
#' @param section Section name (e.g., "Tobacco Use", "Blood Pressure").
#'   If NULL, returns all entries.
#' @return A filtered list of registry entries.
#' @export
get_registry_by_section <- function(section = NULL) {
  reg <- steps_table_registry()
  if (is.null(section)) return(reg)
  Filter(function(x) x$section == section, reg)
}


#' Get table registry entries by step
#'
#' @param step STEPS step number (1, 2, or 3).
#' @return A filtered list of registry entries.
#' @export
get_registry_by_step <- function(step) {
  reg <- steps_table_registry()
  Filter(function(x) x$step == step, reg)
}


#' List all available sections in the registry
#'
#' @return Character vector of unique section names.
#' @export
list_registry_sections <- function() {
  unique(vapply(steps_table_registry(), function(x) x$section, character(1)))
}

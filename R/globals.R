#' stepssurvey: Analyse WHO STEPS Survey Data
#'
#' @description
#' A complete analysis pipeline for the WHO STEPwise Approach to NCD Risk
#' Factor Surveillance (STEPS).
#'
#' @docType package
#' @name stepssurvey-package
"_PACKAGE"

# Silence R CMD check notes about tidyverse NSE variables
utils::globalVariables(c(
  # Core demographics & survey design
  "age", "sex", "age_group", "estimate", "lower", "upper", "se",
  "ci_l", "ci_u", "domain", "indicator",
  "wt_final", "wt_step1", "wt_step2", "wt_step3", ".wt",
  "stratum", "psu",

  # Tobacco
  "tobacco_current_raw", "current_tobacco", "tobacco_daily_raw",
  "daily_tobacco", "smokeless_current_raw", "current_smokeless",
  "current_smoker", "current_tobacco_any", "smoking_status", "daily_smoker", "nondaily_smoker",
  "former_smoker", "never_smoker", "former_daily_smoker",
  "smoking_start_age", "smoking_duration",
  "smokes_manufactured_cig", "cig_per_day_manufactured", "cig_per_day_handrolled",
  "cigarettes_per_day_cat", "quit_attempt", "quit_advice",
  "secondhand_home", "secondhand_work",

  # Alcohol
  "alcohol_current_raw", "current_alcohol", "alcohol_12m",
  "heavy_episode_raw", "heavy_episode_raw_n", "heavy_episodic",
  "alcohol_status", "lifetime_abstainer", "past_12m_abstainer",
  "past_12m_not_current", "alcohol_freq_12m",
  "drinking_occasions_30d", "drinks_per_occasion", "drinking_level",
  "drinking_high", "drinking_intermediate", "drinking_lower", "non_drinker_30d",
  "alcohol_7day_freq_cat",

  # Diet
  "fruit_days", "fruit_serv", "veg_days", "veg_serv",
  "avg_fruit_servings", "avg_veg_servings", "avg_fruit_veg_servings",
  "low_fruit_veg",
  "always_add_salt", "always_processed_salt", "perceived_excess_salt",

  # Physical activity
  "met_total", "insufficient_pa", "low_pa", "pa_category",
  "pa_minutes_per_day", "pa_work_min_day", "pa_transport_min_day",
  "pa_recreation_min_day", "sedentary_min_day", "no_vigorous_pa",

  # Anthropometry
  "height_cm", "weight_kg", "bmi", "bmi_category", "overweight_obese",
  "obese", "underweight", "normal_weight", "overweight_only",
  "waist_cm", "central_obesity", "hip_cm", "waist_hip_ratio",

  # Blood pressure
  "mean_sbp", "mean_dbp", "mean_heart_rate",
  "sbp1", "sbp2", "sbp3", "dbp1", "dbp2", "dbp3",
  "bp_meds_raw", "on_bp_meds", "raised_bp", "bp_stage", "bp_controlled",
  "bp_ever_measured", "bp_diagnosed", "bp_diagnosed_12m",

  # Biochemical
  "fasting_glucose", "dm_meds_raw", "on_dm_meds", "on_dm_insulin",
  "raised_glucose", "diabetes", "impaired_glucose",
  "glucose_ever_measured", "dm_diagnosed", "dm_diagnosed_12m", "dm_new",
  "total_chol", "raised_chol", "raised_chol_high",
  "raised_chol_or_meds", "raised_chol_high_or_meds", "on_chol_meds",
  "chol_ever_measured", "chol_diagnosed", "chol_diagnosed_12m",
  "hdl_chol", "low_hdl", "triglycerides", "raised_trig",

  # CVD history & lifestyle
  "cvd_history", "on_aspirin", "on_statins",
  "advised_quit_tobacco", "advised_reduce_salt", "advised_fruit_veg",
  "advised_reduce_fat", "advised_more_pa", "advised_healthy_weight",
  "cervical_screened",

  # Combined risk & CVD risk
  "cvd_risk_category", ".rf_count", ".rf_0", ".rf_1_2", ".rf_3_5",

  # Table formatting
  "both", "men", "women", "Prevalence",

  # Cleaning internals
  "smokeless_daily_raw", "tobacco_past_raw", "tobacco_past_daily_raw",
  "daily_smokeless", "alcohol_ever", ".data",

  # Diagnostics / plotting
  "digit", "pct", "pct_missing", "stat", "value", "variable",
  "short_label", "label_x", "label_y",

  # Shiny modules
  "zip", "req"
))

#' @importFrom stats as.formula confint rbinom rnorm runif
#' @importFrom utils head
#' @importFrom ggplot2 %+replace%
NULL

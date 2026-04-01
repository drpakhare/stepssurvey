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
  "age", "sex", "age_group", "estimate", "lower", "upper", "se",
  "ci_l", "ci_u", "domain", "indicator", "wt_final", "stratum", "psu",
  "tobacco_current_raw", "current_tobacco", "tobacco_daily_raw",
  "daily_tobacco", "alcohol_current_raw", "current_alcohol",
  "heavy_episode_raw", "heavy_episodic", "met_total", "insufficient_pa",
  "low_pa", "pa_category", "fruit_days", "fruit_serv", "veg_days",
  "veg_serv", "avg_fruit_servings", "avg_veg_servings", "low_fruit_veg",
  "height_cm", "weight_kg", "bmi", "bmi_category", "overweight_obese",
  "obese", "waist_cm", "central_obesity", "mean_sbp", "mean_dbp",
  "bp_meds_raw", "on_bp_meds", "raised_bp", "bp_stage",
  "fasting_glucose", "dm_meds_raw", "on_dm_meds", "raised_glucose",
  "diabetes", "impaired_glucose", "total_chol", "raised_chol",
  "both", "Prevalence",
  "sbp1", "sbp2", "sbp3", "dbp1", "dbp2", "dbp3"
))

#' @importFrom stats as.formula confint rbinom rnorm runif
#' @importFrom utils head
#' @importFrom ggplot2 %+replace%
NULL

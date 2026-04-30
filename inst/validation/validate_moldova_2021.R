#!/usr/bin/env Rscript
# =============================================================================
# Validation Script: Moldova STEPS 2021
# Compares stepssurvey package outputs against official WHO fact sheet values
#
# Data source: WHO NCD Microdata Repository (licensed access)
# Reference: Moldova 2021 STEPS Fact Sheet (WHO European Region, 2022)
# =============================================================================

library(stepssurvey)
library(dplyr)
library(survey)

# -- Configuration -----------------------------------------------------------
# Resolve paths: work from project root or from inst/validation/
.pkg_root <- tryCatch(rprojroot::find_package_root_file(), error = function(e) getwd())
DATA_PATH <- file.path(.pkg_root, "../STEPS Licensed Datasets/Republic of Moldova 2021/MDA_2021_STEPS.dta")
REF_PATH  <- file.path(.pkg_root, "inst/validation/moldova_2021_reference.csv")
# Fallback: try relative paths (when sourced from inst/validation/)
if (!file.exists(DATA_PATH)) DATA_PATH <- "../../../STEPS Licensed Datasets/Republic of Moldova 2021/MDA_2021_STEPS.dta"
if (!file.exists(REF_PATH))  REF_PATH  <- "moldova_2021_reference.csv"

cat("=== Moldova STEPS 2021 Validation ===\n\n")

# -- Step 1: Import ----------------------------------------------------------
cat("--- Step 1: Import ---\n")
raw <- import_steps_data(DATA_PATH)
cat(sprintf("  Imported: %d rows x %d cols\n\n", nrow(raw), ncol(raw)))

# -- Step 2: Detect columns --------------------------------------------------
cat("--- Step 2: Detect columns ---\n")
cols <- detect_steps_columns(raw)

# Show key detections
key_cols <- c("age", "sex", "weight_step1", "weight_step2", "weight_step3",
              "strata", "psu", "agerange", "tobacco_current", "alcohol_current",
              "fasting_glucose", "total_chol", "sbp1", "height", "weight")
for (k in key_cols) {
  val <- if (is.null(cols[[k]])) "NOT FOUND" else cols[[k]]
  cat(sprintf("  %-20s -> %s\n", k, val))
}

# -- Step 3: Clean -----------------------------------------------------------
cat("\n--- Step 3: Clean ---\n")
cleaned <- clean_steps_data(.strip_haven_labels(raw), cols)
cat(sprintf("  Cleaned: %d rows\n", nrow(cleaned)))

# Verify sex coding
cat("  Sex distribution:\n")
print(table(cleaned$sex, useNA = "ifany"))

# Verify age groups
if ("agerange" %in% names(cleaned)) {
  cat("  Age range distribution:\n")
  print(table(cleaned$agerange, useNA = "ifany"))
}

# -- Step 4: Survey design ---------------------------------------------------
cat("\n--- Step 4: Survey design ---\n")
designs <- setup_survey_design(cleaned)
cat("  Design class:", class(designs), "\n")

# -- Step 5: Compute key indicators and compare ------------------------------
cat("\n--- Step 5: Computing indicators and comparing ---\n\n")

# Load reference values
ref <- read.csv(REF_PATH, stringsAsFactors = FALSE)

# Helper: compute a proportion estimate (both sexes, 18-69)
compute_prop <- function(var_name, design, label = var_name) {
  if (!var_name %in% names(design$variables)) {
    return(data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA))
  }

  tryCatch({
    formula <- as.formula(paste0("~", var_name))
    est <- svymean(formula, design, na.rm = TRUE)
    ci <- confint(est)
    # svymean on logical variables returns 2 rows (FALSE, TRUE) — pick TRUE
    vals <- as.numeric(est)
    if (length(vals) == 2) {
      data.frame(
        indicator = label,
        our_est = round(vals[2] * 100, 1),
        our_ci_low = round(ci[2, 1] * 100, 1),
        our_ci_high = round(ci[2, 2] * 100, 1)
      )
    } else {
      data.frame(
        indicator = label,
        our_est = round(vals[1] * 100, 1),
        our_ci_low = round(ci[1, 1] * 100, 1),
        our_ci_high = round(ci[1, 2] * 100, 1)
      )
    }
  }, error = function(e) {
    data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA)
  })
}

# Helper: compute a mean estimate
compute_mean <- function(var_name, design, label = var_name) {
  if (!var_name %in% names(design$variables)) {
    return(data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA))
  }

  tryCatch({
    formula <- as.formula(paste0("~", var_name))
    est <- svymean(formula, design, na.rm = TRUE)
    ci <- confint(est)
    data.frame(
      indicator = label,
      our_est = round(as.numeric(est), 1),
      our_ci_low = round(ci[1, 1], 1),
      our_ci_high = round(ci[1, 2], 1)
    )
  }, error = function(e) {
    data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA)
  })
}

# -- Compute indicators using step-specific designs --------------------------
# Step 1 indicators (behavioural)
d1 <- designs$step1

# Step 2 indicators (physical measurements)
d2 <- designs$step2

# Step 3 indicators (biochemical)
d3 <- designs$step3

# List of proportion indicators to check
# NOTE: Variable names must match what clean_steps_data() produces:
#   current_tobacco_any = smoking OR smokeless (fact sheet "current tobacco use")
#   current_tobacco     = current smoking only (fact sheet "current smoking")
#   current_alcohol     = drank in past 30 days
#   overweight_obese    = BMI >= 25 (fact sheet "overweight" = includes obese)
#   raised_bp           = SBP>=140 OR DBP>=90 OR on meds
#   raised_glucose      = FBG>=7.0 OR on meds
#   raised_chol_or_meds = TC>=5.0 OR on meds

results <- bind_rows(
  # Tobacco
  compute_prop("current_tobacco_any", d1, "current_tobacco_use"),
  compute_prop("current_tobacco", d1, "current_smoking"),
  compute_prop("secondhand_home", d1, "secondhand_smoke_home"),
  compute_prop("secondhand_work", d1, "secondhand_smoke_work"),

  # Alcohol
  compute_prop("current_alcohol", d1, "current_alcohol_30days"),
  compute_prop("heavy_episodic", d1, "heavy_episodic_drinking"),

  # Diet
  compute_prop("low_fruit_veg", d1, "insufficient_fruit_veg"),

  # Physical Activity
  compute_prop("insufficient_pa", d1, "insufficient_pa"),

  # Physical Measurements
  compute_prop("overweight_obese", d2, "overweight_bmi25"),
  compute_prop("obese", d2, "obese_bmi30"),
  compute_prop("raised_bp", d2, "raised_bp_or_meds"),

  # Biochemical
  compute_prop("raised_glucose", d3, "raised_glucose_or_meds"),
  compute_prop("raised_chol_or_meds", d3, "raised_cholesterol_or_meds"),
  compute_prop("impaired_glucose", d3, "impaired_fasting_glucose"),

  # Means
  compute_mean("bmi", d2, "mean_bmi"),
  compute_mean("mean_sbp", d2, "mean_sbp"),
  compute_mean("total_chol", d3, "mean_total_cholesterol")
)

# Merge with reference values
comparison <- results %>%
  left_join(
    ref %>% select(indicator, ref_est = both, ref_ci_low = both_ci_low, ref_ci_high = both_ci_high),
    by = "indicator"
  ) %>%
  mutate(
    diff = our_est - ref_est,
    match = abs(diff) < 1.0  # Within 1 percentage point
  )

# -- Display results ---------------------------------------------------------
cat("VALIDATION RESULTS: Moldova STEPS 2021\n")
cat("Our package estimates vs. WHO official fact sheet (Both Sexes, 18-69)\n")
cat(sprintf("%-35s %8s %8s %8s %8s %8s\n",
            "Indicator", "Ours", "WHO", "Diff", "Match?", "CI overlap?"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (i in seq_len(nrow(comparison))) {
  r <- comparison[i, ]
  ci_overlap <- ""
  if (!is.na(r$our_ci_low) && !is.na(r$ref_ci_low)) {
    # Check if our CI overlaps with WHO CI
    overlaps <- r$our_ci_high >= r$ref_ci_low && r$our_ci_low <= r$ref_ci_high
    ci_overlap <- ifelse(overlaps, "YES", "NO")
  }

  cat(sprintf("%-35s %8s %8s %8s %8s %8s\n",
              r$indicator,
              ifelse(is.na(r$our_est), "N/A", sprintf("%.1f", r$our_est)),
              ifelse(is.na(r$ref_est), "N/A", sprintf("%.1f", r$ref_est)),
              ifelse(is.na(r$diff), "N/A", sprintf("%+.1f", r$diff)),
              ifelse(is.na(r$match), "N/A", ifelse(r$match, "YES", "NO")),
              ci_overlap))
}

# Summary
n_compared <- sum(!is.na(comparison$diff))
n_match <- sum(comparison$match, na.rm = TRUE)
n_ci_overlap <- sum(
  comparison$our_ci_high >= comparison$ref_ci_low &
  comparison$our_ci_low <= comparison$ref_ci_high,
  na.rm = TRUE
)

cat(paste(rep("-", 85), collapse = ""), "\n")
cat(sprintf("SUMMARY: %d/%d indicators within 1pp | %d/%d CIs overlap\n",
            n_match, n_compared, n_ci_overlap, n_compared))
cat(sprintf("Indicators not computed: %d\n", sum(is.na(comparison$our_est))))

# -- Save results ------------------------------------------------------------
write.csv(comparison, "moldova_2021_validation_results.csv", row.names = FALSE)
cat("\nResults saved to moldova_2021_validation_results.csv\n")

# -- Print available derived variables for debugging -------------------------
cat("\n--- Available derived variables in cleaned data ---\n")
derived_vars <- c("current_tobacco", "current_smoker", "tobacco_daily",
                  "smokeless_current", "secondhand_home", "secondhand_work",
                  "current_drinker", "heavy_episodic",
                  "low_fruit_veg", "insufficient_pa", "met_total",
                  "bmi", "overweight", "obese", "mean_sbp", "raised_bp",
                  "fasting_glucose", "raised_glucose", "impaired_glucose",
                  "total_chol", "raised_chol",
                  "age_group", "agerange", "sex")

for (v in derived_vars) {
  if (v %in% names(cleaned)) {
    if (is.logical(cleaned[[v]])) {
      cat(sprintf("  %-25s logical  TRUE=%d FALSE=%d NA=%d\n", v,
                  sum(cleaned[[v]], na.rm = TRUE),
                  sum(!cleaned[[v]], na.rm = TRUE),
                  sum(is.na(cleaned[[v]]))))
    } else if (is.numeric(cleaned[[v]])) {
      cat(sprintf("  %-25s numeric  mean=%.2f  NA=%d\n", v,
                  mean(cleaned[[v]], na.rm = TRUE),
                  sum(is.na(cleaned[[v]]))))
    } else {
      cat(sprintf("  %-25s %s  levels=%d  NA=%d\n", v, class(cleaned[[v]])[1],
                  length(unique(cleaned[[v]])),
                  sum(is.na(cleaned[[v]]))))
    }
  } else {
    cat(sprintf("  %-25s *** NOT FOUND ***\n", v))
  }
}

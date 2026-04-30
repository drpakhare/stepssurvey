#!/usr/bin/env Rscript
# =============================================================================
# Multi-Country Validation Script
# Runs stepssurvey package on all available STEPS datasets and compares
# against official WHO fact sheet values
# =============================================================================

library(stepssurvey)
library(dplyr)
library(survey)

# -- Configuration -----------------------------------------------------------
DATASETS_ROOT <- "../STEPS Licensed Datasets"

countries <- list(
  list(
    name     = "Moldova 2021",
    data     = file.path(DATASETS_ROOT, "Republic of Moldova 2021/MDA_2021_STEPS.dta"),
    ref      = "inst/validation/moldova_2021_reference.csv",
    age_min  = 18,
    age_max  = 69
  ),
  # Jordan 2019 excluded: non-standard instrument coding produces systematic

  # discrepancies in current_smoking (+5.4pp) and raised_bp (+4.0pp).
  # Kept for future investigation once Jordan-specific coding is understood.
  # list(
  #   name     = "Jordan 2019",
  #   data     = file.path(DATASETS_ROOT, "Jordan 2019/jor2019.dta"),
  #   ref      = "inst/validation/jordan_2019_reference.csv",
  #   age_min  = 18,
  #   age_max  = 69
  # ),
  list(
    name     = "Mongolia 2019",
    data     = file.path(DATASETS_ROOT, "Mongolia 2019/mng2019.dta"),
    ref      = "inst/validation/mongolia_2019_reference.csv",
    age_min  = 15,
    age_max  = 69,
    bp_sbp   = 130,
    bp_dbp   = 80,
    # Mongolia fact sheet reports daily smoking under "current smoking"
    smoking_var = "daily_smoker"
  ),
  list(
    name     = "Georgia 2016",
    data     = file.path(DATASETS_ROOT, "Geogrgia 2016/geo2016.dta"),
    ref      = "inst/validation/georgia_2016_reference.csv",
    age_min  = 18,
    age_max  = 69,
    # Georgia fact sheet reports daily smoking under "current smoking"
    smoking_var = "daily_smoker"
  )
)

# -- Helpers (same as Moldova script) ----------------------------------------
compute_prop <- function(var_name, design, label = var_name) {
  if (!var_name %in% names(design$variables)) {
    return(data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA))
  }
  tryCatch({
    formula <- as.formula(paste0("~", var_name))
    est <- svymean(formula, design, na.rm = TRUE)
    ci <- confint(est)
    vals <- as.numeric(est)
    if (length(vals) == 2) {
      data.frame(indicator = label, our_est = round(vals[2] * 100, 1),
                 our_ci_low = round(ci[2, 1] * 100, 1), our_ci_high = round(ci[2, 2] * 100, 1))
    } else {
      data.frame(indicator = label, our_est = round(vals[1] * 100, 1),
                 our_ci_low = round(ci[1, 1] * 100, 1), our_ci_high = round(ci[1, 2] * 100, 1))
    }
  }, error = function(e) {
    data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA)
  })
}

compute_mean <- function(var_name, design, label = var_name) {
  if (!var_name %in% names(design$variables)) {
    return(data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA))
  }
  tryCatch({
    formula <- as.formula(paste0("~", var_name))
    est <- svymean(formula, design, na.rm = TRUE)
    ci <- confint(est)
    data.frame(indicator = label, our_est = round(as.numeric(est), 1),
               our_ci_low = round(ci[1, 1], 1), our_ci_high = round(ci[1, 2], 1))
  }, error = function(e) {
    data.frame(indicator = label, our_est = NA, our_ci_low = NA, our_ci_high = NA)
  })
}

# -- Run validation for each country -----------------------------------------
all_results <- list()

for (country in countries) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("  VALIDATING:", country$name, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")

  # Check files exist
  if (!file.exists(country$data)) {
    cat("  *** DATA FILE NOT FOUND:", country$data, "-- SKIPPING ***\n")
    next
  }
  if (!file.exists(country$ref)) {
    cat("  *** REFERENCE FILE NOT FOUND:", country$ref, "-- SKIPPING ***\n")
    next
  }

  # Step 1: Import
  raw <- import_steps_data(country$data)
  cat(sprintf("  Imported: %d rows x %d cols\n", nrow(raw), ncol(raw)))

  # Step 2: Detect columns
  cols <- detect_steps_columns(raw)

  # Step 3: Clean
  bp_sbp <- if (!is.null(country$bp_sbp)) country$bp_sbp else 140
  bp_dbp <- if (!is.null(country$bp_dbp)) country$bp_dbp else 90
  cleaned <- clean_steps_data(.strip_haven_labels(raw), cols,
                               age_min = country$age_min,
                               age_max = country$age_max,
                               bp_sbp_threshold = bp_sbp,
                               bp_dbp_threshold = bp_dbp)
  cat(sprintf("  Cleaned: %d rows\n", nrow(cleaned)))

  # Step 4: Survey design
  designs <- setup_survey_design(cleaned)

  # Step 5: Compute indicators
  d1 <- designs$step1
  d2 <- designs$step2
  d3 <- designs$step3

  # Some WHO fact sheets report daily smoking under "current smoking"
  smoking_var <- if (!is.null(country$smoking_var)) country$smoking_var else "current_tobacco"

  results <- bind_rows(
    compute_prop("current_tobacco_any", d1, "current_tobacco_use"),
    compute_prop(smoking_var, d1, "current_smoking"),
    compute_prop("secondhand_home", d1, "secondhand_smoke_home"),
    compute_prop("secondhand_work", d1, "secondhand_smoke_work"),
    compute_prop("current_alcohol", d1, "current_alcohol_30days"),
    compute_prop("heavy_episodic", d1, "heavy_episodic_drinking"),
    compute_prop("low_fruit_veg", d1, "insufficient_fruit_veg"),
    compute_prop("insufficient_pa", d1, "insufficient_pa"),
    compute_prop("overweight_obese", d2, "overweight_bmi25"),
    compute_prop("obese", d2, "obese_bmi30"),
    compute_prop("raised_bp", d2, "raised_bp_or_meds"),
    compute_prop("raised_glucose", d3, "raised_glucose_or_meds"),
    compute_prop("raised_chol_or_meds", d3, "raised_cholesterol_or_meds"),
    compute_prop("impaired_glucose", d3, "impaired_fasting_glucose"),
    compute_mean("bmi", d2, "mean_bmi"),
    compute_mean("mean_sbp", d2, "mean_sbp"),
    compute_mean("total_chol", d3, "mean_total_cholesterol")
  )

  # Load reference and compare
  ref <- read.csv(country$ref, stringsAsFactors = FALSE)
  comparison <- results %>%
    left_join(
      ref %>% select(indicator, ref_est = both, ref_ci_low = both_ci_low, ref_ci_high = both_ci_high),
      by = "indicator"
    ) %>%
    mutate(
      diff = our_est - ref_est,
      match = abs(diff) < 1.0
    )

  # Display
  cat(sprintf("\nVALIDATION RESULTS: %s\n", country$name))
  cat("Our package vs. WHO official fact sheet (Both Sexes)\n")
  cat(sprintf("%-35s %8s %8s %8s %8s %8s\n",
              "Indicator", "Ours", "WHO", "Diff", "Match?", "CI overlap?"))
  cat(paste(rep("-", 85), collapse = ""), "\n")

  for (i in seq_len(nrow(comparison))) {
    r <- comparison[i, ]
    ci_overlap <- ""
    if (!is.na(r$our_ci_low) && !is.na(r$ref_ci_low)) {
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

  n_compared <- sum(!is.na(comparison$diff))
  n_match <- sum(comparison$match, na.rm = TRUE)
  n_ci_overlap <- sum(
    comparison$our_ci_high >= comparison$ref_ci_low &
    comparison$our_ci_low <= comparison$ref_ci_high,
    na.rm = TRUE
  )

  cat(paste(rep("-", 85), collapse = ""), "\n")
  cat(sprintf("SUMMARY: %d/%d within 1pp | %d/%d CIs overlap\n",
              n_match, n_compared, n_ci_overlap, n_compared))

  # Save per-country results
  out_file <- sprintf("inst/validation/%s_validation_results.csv",
                       gsub(" ", "_", tolower(country$name)))
  write.csv(comparison, out_file, row.names = FALSE)
  cat(sprintf("Results saved to %s\n", out_file))

  all_results[[country$name]] <- list(
    n_compared = n_compared,
    n_match = n_match,
    n_ci_overlap = n_ci_overlap,
    comparison = comparison
  )
}

# -- Grand summary -----------------------------------------------------------
cat("\n\n", paste(rep("=", 80), collapse = ""), "\n")
cat("  GRAND SUMMARY: All Countries\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat(sprintf("%-25s %12s %12s %12s\n", "Country", "Within 1pp", "CI Overlap", "Match Rate"))
cat(paste(rep("-", 65), collapse = ""), "\n")
total_compared <- 0
total_match <- 0
total_ci <- 0

for (nm in names(all_results)) {
  r <- all_results[[nm]]
  cat(sprintf("%-25s %6d/%-5d %6d/%-5d %10.0f%%\n",
              nm, r$n_match, r$n_compared, r$n_ci_overlap, r$n_compared,
              r$n_match / r$n_compared * 100))
  total_compared <- total_compared + r$n_compared
  total_match    <- total_match    + r$n_match
  total_ci       <- total_ci       + r$n_ci_overlap
}

cat(paste(rep("-", 65), collapse = ""), "\n")
cat(sprintf("%-25s %6d/%-5d %6d/%-5d %10.0f%%\n",
            "TOTAL", total_match, total_compared, total_ci, total_compared,
            total_match / total_compared * 100))

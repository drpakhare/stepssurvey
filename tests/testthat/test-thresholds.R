test_that("default thresholds produce expected raised_bp definition", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  # Default: SBP >= 140 or DBP >= 90 or on meds
  raised <- clean$raised_bp
  mean_sbp <- clean$mean_sbp
  mean_dbp <- clean$mean_dbp
  # clean_steps_data() renames bp_meds to bp_meds_raw and derives on_bp_meds
  on_meds <- if ("on_bp_meds" %in% names(clean)) clean$on_bp_meds else rep(FALSE, nrow(clean))

  # Every raised_bp == TRUE should have SBP >= 140 or DBP >= 90 or on meds
  for (i in which(!is.na(raised) & raised == TRUE)) {
    high_sbp <- !is.na(mean_sbp[i]) && mean_sbp[i] >= 140
    high_dbp <- !is.na(mean_dbp[i]) && mean_dbp[i] >= 90
    has_meds <- !is.na(on_meds[i]) && on_meds[i] == TRUE
    expect_true(high_sbp || high_dbp || has_meds,
      info = paste("Row", i, "raised_bp=TRUE but SBP=", mean_sbp[i],
                   "DBP=", mean_dbp[i], "on_bp_meds=", on_meds[i]))
  }
})

test_that("custom BP thresholds change raised_bp prevalence", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)

  # Default 140/90
  clean_default <- clean_steps_data(td, cols,
    bp_sbp_threshold = 140, bp_dbp_threshold = 90)

  # Stricter 130/80
  clean_strict <- clean_steps_data(td, cols,
    bp_sbp_threshold = 130, bp_dbp_threshold = 80)

  # Looser 160/100
  clean_loose <- clean_steps_data(td, cols,
    bp_sbp_threshold = 160, bp_dbp_threshold = 100)

  prev_default <- mean(clean_default$raised_bp, na.rm = TRUE)
  prev_strict <- mean(clean_strict$raised_bp, na.rm = TRUE)
  prev_loose <- mean(clean_loose$raised_bp, na.rm = TRUE)

  # Stricter threshold should give higher prevalence
 expect_true(prev_strict >= prev_default,
    info = paste("strict:", prev_strict, "default:", prev_default))

  # Looser threshold should give lower prevalence
  expect_true(prev_loose <= prev_default,
    info = paste("loose:", prev_loose, "default:", prev_default))
})

test_that("custom BMI thresholds change overweight/obese prevalence", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)

  # Default 25/30
  clean_default <- clean_steps_data(td, cols,
    bmi_overweight = 25, bmi_obese = 30)

  # Stricter Asian thresholds 23/27.5
  clean_asian <- clean_steps_data(td, cols,
    bmi_overweight = 23, bmi_obese = 27.5)

  prev_ow_default <- mean(clean_default$overweight_obese, na.rm = TRUE)
  prev_ow_asian <- mean(clean_asian$overweight_obese, na.rm = TRUE)

  # Lower threshold should capture more people
  expect_true(prev_ow_asian >= prev_ow_default,
    info = paste("asian:", prev_ow_asian, "default:", prev_ow_default))
})

test_that("custom glucose threshold changes raised_glucose prevalence", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)

  # Default 7.0
  clean_default <- clean_steps_data(td, cols, glucose_threshold = 7.0)

  # Stricter 6.5
  clean_strict <- clean_steps_data(td, cols, glucose_threshold = 6.5)

  if ("raised_glucose" %in% names(clean_default) &&
      "raised_glucose" %in% names(clean_strict)) {
    prev_default <- mean(clean_default$raised_glucose, na.rm = TRUE)
    prev_strict <- mean(clean_strict$raised_glucose, na.rm = TRUE)

    expect_true(prev_strict >= prev_default,
      info = paste("strict:", prev_strict, "default:", prev_default))
  }
})

test_that("custom cholesterol threshold changes raised_chol prevalence", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)

  clean_default <- clean_steps_data(td, cols, chol_threshold = 5.0)
  clean_strict <- clean_steps_data(td, cols, chol_threshold = 4.5)

  if ("raised_chol" %in% names(clean_default) &&
      "raised_chol" %in% names(clean_strict)) {
    prev_default <- mean(clean_default$raised_chol, na.rm = TRUE)
    prev_strict <- mean(clean_strict$raised_chol, na.rm = TRUE)

    expect_true(prev_strict >= prev_default,
      info = paste("strict:", prev_strict, "default:", prev_default))
  }
})

test_that("steps_config stores all threshold parameters", {
  cfg <- steps_config(
    data_path = "dummy.csv",
    bp_sbp_threshold = 130,
    bp_dbp_threshold = 80,
    bmi_overweight = 23,
    bmi_obese = 27.5,
    glucose_threshold = 6.5,
    glucose_impaired_threshold = 5.6,
    chol_threshold = 4.5
  )

  expect_equal(cfg$bp_sbp_threshold, 130)
  expect_equal(cfg$bp_dbp_threshold, 80)
  expect_equal(cfg$bmi_overweight, 23)
  expect_equal(cfg$bmi_obese, 27.5)
  expect_equal(cfg$glucose_threshold, 6.5)
  expect_equal(cfg$glucose_impaired_threshold, 5.6)
  expect_equal(cfg$chol_threshold, 4.5)
})

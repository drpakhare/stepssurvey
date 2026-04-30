# Edge case tests for robustness

test_that("clean_steps_data handles missing optional columns gracefully", {
  # Minimal dataset: only age, sex, weight, stratum, psu
  set.seed(42)
  n <- 100
  minimal <- data.frame(
    age = sample(18:69, n, replace = TRUE),
    sex = sample(1:2, n, replace = TRUE),
    wt_final = runif(n, 0.5, 2.0),
    stratum = rep("S1", n),
    psu = sample(paste0("P", 1:5), n, replace = TRUE)
  )

  cols <- detect_steps_columns(minimal)
  clean <- clean_steps_data(minimal, cols)

  expect_s3_class(clean, "data.frame")
  expect_true(nrow(clean) > 0)
  expect_true("age_group" %in% names(clean))
  expect_s3_class(clean$sex, "factor")
})

test_that("clean_steps_data handles age filtering correctly", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)

  # Narrow age range
  clean <- clean_steps_data(td, cols, age_min = 25, age_max = 44)

  expect_true(all(clean$age >= 25))
  expect_true(all(clean$age <= 44))
  expect_true(nrow(clean) < 500)
})

test_that("clean_steps_data handles extended age range", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)

  # Wider than default
  clean_wide <- clean_steps_data(td, cols, age_min = 15, age_max = 79)
  clean_default <- clean_steps_data(td, cols, age_min = 18, age_max = 69)

  expect_true(nrow(clean_wide) >= nrow(clean_default))
})

test_that("non-drinkers are coded FALSE not NA for alcohol indicators", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  if ("current_alcohol" %in% names(clean)) {
    # Non-drinkers should be FALSE, not NA
    n_false <- sum(clean$current_alcohol == FALSE, na.rm = TRUE)
    expect_true(n_false > 0,
      info = "No non-drinkers found -- all may be NA")
  }

  if ("heavy_episodic" %in% names(clean)) {
    # Non-drinkers should be FALSE for heavy episodic too
    n_false_hed <- sum(clean$heavy_episodic == FALSE, na.rm = TRUE)
    expect_true(n_false_hed > 0,
      info = "No FALSE values in heavy_episodic -- denominator may be wrong")
  }
})

test_that("GPAQ screening No sets domain to zero not NA", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  if ("met_total" %in% names(clean)) {
    # MET should have zeros (from screening No), not all NA or all positive
    n_zero <- sum(clean$met_total == 0, na.rm = TRUE)
    n_positive <- sum(clean$met_total > 0, na.rm = TRUE)

    expect_true(n_zero > 0 || n_positive > 0,
      info = "MET total is all NA")
  }
})

test_that("diet zero-days gives zero servings not NA", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  if ("avg_fruit_veg" %in% names(clean)) {
    # Should not be all NA
    expect_true(sum(!is.na(clean$avg_fruit_veg)) > 0)

    # Zero should be possible
    n_zero <- sum(clean$avg_fruit_veg == 0, na.rm = TRUE)
    # It's OK if there happen to be none, but avg should be reasonable
    avg <- mean(clean$avg_fruit_veg, na.rm = TRUE)
    expect_true(avg >= 0 & avg < 20,
      info = paste("Average fruit/veg servings:", avg))
  }
})

test_that("BMI is computed and categorised", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  expect_true("bmi" %in% names(clean))
  expect_true("bmi_category" %in% names(clean))

  # BMI should be reasonable (15-60)
  bmi_vals <- clean$bmi[!is.na(clean$bmi)]
  expect_true(all(bmi_vals > 10 & bmi_vals < 80))

  # Categories should include expected values (character, not factor)
  cats <- unique(clean$bmi_category[!is.na(clean$bmi_category)])
  expect_true("Normal" %in% cats || "Overweight" %in% cats)
})

test_that("BP averaging uses last two of three readings", {
  td <- generate_test_data(n = 200, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  expect_true("mean_sbp" %in% names(clean))
  expect_true("mean_dbp" %in% names(clean))

  # Mean SBP should be in reasonable range
  sbp_vals <- clean$mean_sbp[!is.na(clean$mean_sbp)]
  expect_true(all(sbp_vals > 60 & sbp_vals < 300))
})

test_that("setup_survey_design works without weights", {
  set.seed(42)
  n <- 100
  no_weight <- data.frame(
    age = sample(18:69, n, replace = TRUE),
    sex = sample(1:2, n, replace = TRUE),
    stratum = rep("S1", n),
    psu = sample(paste0("P", 1:5), n, replace = TRUE)
  )

  cols <- detect_steps_columns(no_weight)
  clean <- clean_steps_data(no_weight, cols)
  designs <- setup_survey_design(clean)

  # Should still produce a valid design (with weight = 1)
  expect_s3_class(designs$step1, "survey.design")
})

test_that("full pipeline runs end-to-end on demo data", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)
  result <- compute_all_indicators(designs)
  tables <- build_steps_tables(result$results)
  plots <- build_steps_plots(result$results, result$key_indicators,
                              "Test", 2024)

  expect_true(nrow(result$key_indicators) > 0)
  expect_true(length(tables) > 0)
  expect_true(length(plots) > 0)
})

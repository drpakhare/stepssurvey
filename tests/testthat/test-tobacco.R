# Regression tests for the tobacco 100% prevalence bug.
# The smk_cln filter was using raw WHO 1/2 codes on demo data that uses
# 0/1 codes, causing all non-smokers to be set to NA and producing 100%
# tobacco prevalence.

test_that("demo data does NOT produce 100% tobacco prevalence", {
  td <- generate_test_data(n = 1000, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  tobacco_prev <- mean(clean$current_tobacco, na.rm = TRUE)

  # Demo data is generated with ~25% smokers, should never be 100%
  expect_true(tobacco_prev < 0.80,
    info = paste("Tobacco prevalence:", round(tobacco_prev * 100, 1), "%"))
  expect_true(tobacco_prev > 0.05,
    info = paste("Tobacco prevalence:", round(tobacco_prev * 100, 1), "%"))
})

test_that("smk_cln filter preserves non-smokers with 0/1 coding", {
  # Simulate 0/1 coding (R/demo convention)
  td <- generate_test_data(n = 500, seed = 99)
  cols <- detect_steps_columns(td)

  # Verify demo data uses 0/1 coding for t1
  t1_vals <- unique(td[[cols$tobacco_current]])
  expect_true(all(t1_vals %in% c(0, 1, NA)),
    info = paste("t1 values:", paste(sort(t1_vals), collapse = ", ")))

  clean <- clean_steps_data(td, cols)

  # Non-smokers should NOT be NA after cleaning
  n_smokers <- sum(clean$current_tobacco == TRUE, na.rm = TRUE)
  n_nonsmokers <- sum(clean$current_tobacco == FALSE, na.rm = TRUE)

  expect_true(n_nonsmokers > 0,
    info = paste("Non-smokers:", n_nonsmokers, "Smokers:", n_smokers))
})

test_that("smk_cln filter works with 1/2 WHO coding", {
  # Create data with WHO standard 1=Yes, 2=No coding
  set.seed(123)
  n <- 200
  td <- data.frame(
    age = sample(18:69, n, replace = TRUE),
    sex = sample(1:2, n, replace = TRUE),
    wt_final = runif(n, 0.5, 2.0),
    stratum = sample(paste0("S", 1:3), n, replace = TRUE),
    psu = sample(paste0("P", 1:10), n, replace = TRUE),
    t1 = sample(c(1, 2), n, replace = TRUE, prob = c(0.3, 0.7)),
    t2 = sample(c(1, 2), n, replace = TRUE),
    m4a = rnorm(n, 125, 15), m5a = rnorm(n, 125, 15),
    m4b = rnorm(n, 80, 10), m5b = rnorm(n, 80, 10),
    m11 = rnorm(n, 170, 10), m12 = rnorm(n, 75, 15)
  )

  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  tobacco_prev <- mean(clean$current_tobacco, na.rm = TRUE)

  # Should be around 30%, definitely not 100%
  expect_true(tobacco_prev < 0.80,
    info = paste("WHO 1/2 coding tobacco prevalence:",
                 round(tobacco_prev * 100, 1), "%"))
})

test_that("daily_tobacco is derived correctly", {
  td <- generate_test_data(n = 500, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  # Daily tobacco users should be a subset of current tobacco users
  if ("daily_tobacco" %in% names(clean)) {
    daily <- clean$daily_tobacco
    current <- clean$current_tobacco

    # Everyone who is daily should also be current
    daily_but_not_current <- sum(daily & !current, na.rm = TRUE)
    expect_equal(daily_but_not_current, 0,
      info = "Found daily users who are not current users")
  }
})

test_that("tobacco indicators compute without error", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)

  result <- compute_tobacco_indicators(designs$step1)
  expect_type(result, "list")
  expect_true("current_tobacco_total" %in% names(result))

  # Prevalence should be a reasonable number
  prev <- result$current_tobacco_total$estimate
  expect_true(prev >= 0 & prev <= 100,
    info = paste("Tobacco prevalence:", prev))
})

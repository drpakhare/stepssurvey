test_that("steps_data_quality returns expected structure", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  dq <- steps_data_quality(td, clean, cols)

  expect_s3_class(dq, "steps_quality")
  expect_true(all(c("digit_preference", "completeness",
                     "plausibility", "weights") %in% names(dq)))
})

test_that("digit_preference returns data for SBP", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  dq <- steps_data_quality(td, clean, cols)
  dp <- dq$digit_preference

  # Should have SBP entry
  expect_true("SBP" %in% names(dp))

  # SBP entry should have digit_pct and heaping_0_5
  sbp_dp <- dp$SBP
  expect_true("digit_pct" %in% names(sbp_dp))
  expect_true("heaping_0_5" %in% names(sbp_dp))
  expect_true(is.numeric(sbp_dp$heaping_0_5))
})

test_that("completeness returns a named list of data frames", {
  td <- generate_test_data(n = 200, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  dq <- steps_data_quality(td, clean, cols)
  comp <- dq$completeness

  # completeness is a named list of data frames (one per domain)
  expect_type(comp, "list")
  expect_true(length(comp) > 0)

  # Each domain entry should be a data frame with pct_missing
  first_domain <- comp[[1]]
  expect_s3_class(first_domain, "data.frame")
  expect_true(nrow(first_domain) > 0)
  expect_true("pct_missing" %in% names(first_domain))
})

test_that("weight diagnostics returns weight stats", {
  td <- generate_test_data(n = 200, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)

  dq <- steps_data_quality(td, clean, cols)
  wt <- dq$weights

  expect_type(wt, "list")
  # Should have at least Step 1 weight info
  expect_true(length(wt) > 0)
})

test_that("plot_digit_preference produces a ggplot", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  dq <- steps_data_quality(td, clean, cols)

  # plot_digit_preference requires a measure argument
  p <- plot_digit_preference(dq, "SBP")
  expect_s3_class(p, "ggplot")
})

test_that("plot_completeness produces a ggplot", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  dq <- steps_data_quality(td, clean, cols)

  p <- plot_completeness(dq)
  expect_s3_class(p, "ggplot")
})

test_that("plot_weights produces a ggplot", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  dq <- steps_data_quality(td, clean, cols)

  p <- plot_weights(dq)
  expect_s3_class(p, "ggplot")
})

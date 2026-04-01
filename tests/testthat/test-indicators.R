test_that("compute_all_indicators returns expected structure", {
  td <- generate_test_data(n = 300, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  design <- setup_survey_design(clean)
  result <- compute_all_indicators(design)

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("key_indicators" %in% names(result))

  # Check domains
  expect_true(all(c("tobacco", "alcohol", "diet_pa", "anthropometry",
                     "blood_pressure", "biochemical") %in% names(result$results)))

  # Check key_indicators is a tibble
  ki <- result$key_indicators
  expect_s3_class(ki, "data.frame")
  expect_true(nrow(ki) > 0)
  expect_true(all(c("domain", "indicator", "estimate", "lower", "upper") %in% names(ki)))

  # All estimates should be percentages between 0 and 100
  expect_true(all(ki$estimate >= 0 & ki$estimate <= 100))
})

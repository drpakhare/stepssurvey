test_that("setup_survey_design creates valid design object", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  design <- setup_survey_design(clean)

  expect_s3_class(design, "survey.design")
})

test_that("svyprop returns consistent columns", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  design <- setup_survey_design(clean)

  # Without by
  res_total <- svyprop(~as.numeric(current_tobacco), design)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res_total)))

  # With by
  res_sex <- svyprop(~as.numeric(current_tobacco), design, by = ~sex)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res_sex)))
})

test_that("svymn returns consistent columns", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  design <- setup_survey_design(clean)

  res <- svymn(~bmi, design)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res)))

  res_by <- svymn(~bmi, design, by = ~sex)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res_by)))
})

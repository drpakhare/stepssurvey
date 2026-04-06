test_that("setup_survey_design creates valid design object", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)

  expect_s3_class(designs, "steps_designs")
  expect_s3_class(designs$step1, "survey.design")
  expect_s3_class(designs$step2, "survey.design")
  expect_s3_class(designs$step3, "survey.design")
})

test_that("svyprop returns consistent columns", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)

  # Without by (tobacco is Step 1)
  res_total <- svyprop(~as.numeric(current_tobacco), designs$step1)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res_total)))

  # With by
  res_sex <- svyprop(~as.numeric(current_tobacco), designs$step1, by = ~sex)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res_sex)))
})

test_that("svymn returns consistent columns", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)

  # BMI is Step 2
  res <- svymn(~bmi, designs$step2)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res)))

  res_by <- svymn(~bmi, designs$step2, by = ~sex)
  expect_true(all(c("estimate", "lower", "upper") %in% names(res_by)))
})

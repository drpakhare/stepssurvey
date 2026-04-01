test_that("recode_yn handles various encodings", {
  expect_true(recode_yn(1))
  expect_true(recode_yn("Yes"))
  expect_true(recode_yn("yes"))
  expect_true(recode_yn("Y"))
  expect_false(recode_yn(2))
  expect_false(recode_yn("No"))
  expect_false(recode_yn("no"))
  expect_true(is.na(recode_yn(NA)))
  expect_true(is.na(recode_yn(99)))
})

test_that("clean_steps_data produces expected columns", {
  td <- generate_test_data(n = 200, seed = 1)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols, age_min = 18, age_max = 69)

  expect_s3_class(clean, "data.frame")
  expect_true("age_group" %in% names(clean))
  expect_true("sex" %in% names(clean))
  expect_s3_class(clean$sex, "factor")
  expect_equal(levels(clean$sex), c("Male", "Female"))

  # Check derived indicators exist
  expect_true("bmi" %in% names(clean))
  expect_true("raised_bp" %in% names(clean))
  expect_true("current_tobacco" %in% names(clean))
})

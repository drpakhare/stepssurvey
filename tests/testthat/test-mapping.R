test_that("read_column_mapping reads CSV mapping correctly", {
  # Create a minimal CSV mapping
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  mapping <- data.frame(
    Standard.Variable = c("DEMOGRAPHICS", "age", "sex", "sbp1", "height"),
    Description = c(NA, "Age", "Sex", "SBP 1", "Height cm"),
    Your.Column.Name = c(NA, "my_age", "my_sex", "bp_systolic_1", ""),
    stringsAsFactors = FALSE
  )
  write.csv(mapping, tmp, row.names = FALSE)

  cols <- read_column_mapping(tmp)

  # Mapped variables should have user's column names

  expect_equal(cols$age, "my_age")
  expect_equal(cols$sex, "my_sex")
  expect_equal(cols$sbp1, "bp_systolic_1")

  # Unmapped variable (blank "Your Column Name") should be NULL
  expect_null(cols$height)

  # Domain header row should be skipped (not appear as a key)
  expect_false("DEMOGRAPHICS" %in% names(cols))
})

test_that("read_column_mapping validates against data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  mapping <- data.frame(
    Standard.Variable = c("age", "sex"),
    Description = c("Age", "Sex"),
    Your.Column.Name = c("my_age", "nonexistent_col"),
    stringsAsFactors = FALSE
  )
  write.csv(mapping, tmp, row.names = FALSE)

  fake_data <- data.frame(my_age = 1:5, my_sex = c(1, 2, 1, 2, 1))

  # Should warn about nonexistent_col

  expect_warning(
    cols <- read_column_mapping(tmp, data = fake_data),
    "not found in data"
  )

  # But still map it (the user might fix the data later)
  expect_equal(cols$sex, "nonexistent_col")
})

test_that("read_column_mapping maps provided variables correctly", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  mapping <- data.frame(
    Standard.Variable = c("age", "sbp1", "fasting_glucose"),
    Description = c("Age", "SBP 1", "Glucose"),
    Your.Column.Name = c("my_age", "my_sbp", "my_glucose"),
    stringsAsFactors = FALSE
  )
  write.csv(mapping, tmp, row.names = FALSE)

  cols <- read_column_mapping(tmp)

  # Mapped variables should be present
  expect_equal(cols$age, "my_age")
  expect_equal(cols$sbp1, "my_sbp")
  expect_equal(cols$fasting_glucose, "my_glucose")

  # Unmapped variables should be NULL (absent from list in R)
  expect_null(cols$height)
  expect_null(cols$weight)
})

test_that("read_column_mapping errors on missing file", {
  expect_error(read_column_mapping("nonexistent.xlsx"), "not found")
})

test_that("read_column_mapping errors on unsupported format", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  writeLines("{}", tmp)
  expect_error(read_column_mapping(tmp), "Unsupported")
})

test_that("read_column_mapping integrates with clean_steps_data", {
  # Create mapping that maps demo data columns to themselves
  td <- generate_test_data(n = 100, seed = 42)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  # Map all key columns using their actual names from demo data
  auto_cols <- detect_steps_columns(td)
  mapping <- data.frame(
    Standard.Variable = c("age", "sex", "weight_step1", "strata", "psu",
                           "tobacco_current", "sbp1", "sbp2", "sbp3",
                           "dbp1", "dbp2", "dbp3", "height", "weight"),
    Description = rep("", 14),
    Your.Column.Name = c(auto_cols$age, auto_cols$sex, auto_cols$weight_step1,
                          auto_cols$strata, auto_cols$psu,
                          auto_cols$tobacco_current,
                          auto_cols$sbp1, auto_cols$sbp2, auto_cols$sbp3,
                          auto_cols$dbp1, auto_cols$dbp2, auto_cols$dbp3,
                          auto_cols$height, auto_cols$weight),
    stringsAsFactors = FALSE
  )
  write.csv(mapping, tmp, row.names = FALSE)

  cols <- read_column_mapping(tmp, data = td)
  clean <- clean_steps_data(td, cols)

  expect_s3_class(clean, "data.frame")
  expect_true(nrow(clean) > 0)
  expect_true("bmi" %in% names(clean))
})

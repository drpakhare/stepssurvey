test_that("detect_col finds columns case-insensitively", {
  df <- data.frame(Age = 1:5, SEX = c("M", "F", "M", "F", "M"))
  expect_equal(detect_col(df, c("age", "c1"), "Age"), "Age")
  expect_equal(detect_col(df, c("sex", "gender"), "Sex"), "SEX")
  expect_null(detect_col(df, c("missing_col"), "Missing"))
})

test_that("detect_steps_columns returns named list", {
  df <- data.frame(
    age = 18:22, sex = c(1, 2, 1, 2, 1),
    wt_final = rep(1, 5), stratum = rep("S1", 5), psu = rep("P1", 5),
    t1 = c(1, 0, 1, 0, 1), a1 = c(0, 1, 0, 1, 0)
  )
  cols <- detect_steps_columns(df)
  expect_type(cols, "list")
  expect_equal(cols$age, "age")
  expect_equal(cols$sex, "sex")
  expect_equal(cols$tobacco_current, "t1")
})

test_that("generate_test_data produces valid data frame", {
  td <- generate_test_data(n = 100, seed = 1)
  expect_s3_class(td, "data.frame")
  expect_equal(nrow(td), 100)
  expect_true(all(c("age", "sex", "wt_final", "stratum", "psu") %in% names(td)))
  expect_true(all(td$age >= 18 & td$age <= 69))
  expect_true(all(td$sex %in% c(1, 2)))
})

test_that("build_forest_plot returns a ggplot", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)
  result <- compute_all_indicators(designs)

  p <- build_forest_plot(result$key_indicators, "Testland", 2024)
  expect_s3_class(p, "ggplot")
})

test_that("build_radar_plot returns a ggplot", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)
  result <- compute_all_indicators(designs)

  p <- build_radar_plot(result$key_indicators, "Testland", 2024)
  expect_s3_class(p, "ggplot")
})

test_that("build_steps_plots includes forest and radar", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)
  result <- compute_all_indicators(designs)

  plots <- build_steps_plots(
    result$results, result$key_indicators, "Testland", 2024
  )

  expect_type(plots, "list")
  expect_true("forest" %in% names(plots))
  expect_true("radar" %in% names(plots))
  expect_s3_class(plots$forest, "ggplot")
  expect_s3_class(plots$radar, "ggplot")
})

test_that("build_steps_plots produces overview plot", {
  td <- generate_test_data(n = 300, seed = 42)
  cols <- detect_steps_columns(td)
  clean <- clean_steps_data(td, cols)
  designs <- setup_survey_design(clean)
  result <- compute_all_indicators(designs)

  plots <- build_steps_plots(
    result$results, result$key_indicators, "Testland", 2024
  )

  expect_true("overview" %in% names(plots))
  expect_s3_class(plots$overview, "ggplot")
})

test_that("theme_steps returns a ggplot theme", {
  th <- theme_steps()
  expect_s3_class(th, "theme")
})

test_that("steps_colors returns named color list", {
  pal <- steps_colors()
  expect_type(pal, "list")
  expect_true(all(c("blue", "male", "female") %in% names(pal)))
})

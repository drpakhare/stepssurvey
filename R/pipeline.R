#' Run the complete STEPS analysis pipeline
#'
#' Imports raw data, cleans it, sets up the survey design, computes
#' all indicators, generates publication-ready tables and plots, and
#' optionally renders Word reports.
#'
#' This is the main entry point for end-to-end STEPS analysis.
#'
#' @param data_path Path to raw STEPS data file (CSV, Excel, Stata, or SPSS).
#' @param country_name Country name for reports (default "Country Name").
#' @param survey_year Survey year (default 2024).
#' @param age_min Minimum age in years (default 18).
#' @param age_max Maximum age in years (default 69).
#' @param output_dir Directory for all outputs (default "outputs").
#' @param render_reports Logical; render Word documents? (default TRUE).
#' @param mapping_file Optional path to a filled column mapping template
#'   (Excel or CSV). If provided, uses [read_column_mapping()] instead of
#'   auto-detection. See the template at
#'   \code{system.file("templates", "column_mapping_template.xlsx",
#'   package = "stepssurvey")}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{raw_data}{Original imported data frame}
#'     \item{clean_data}{Cleaned and recoded data}
#'     \item{cols}{Detected column mapping from [detect_steps_columns()]}
#'     \item{design}{[survey::svydesign] object}
#'     \item{indicators}{List of all computed indicator results by domain}
#'     \item{key_indicators}{Summary tibble of headline estimates}
#'     \item{tables}{List of [flextable::flextable] objects}
#'     \item{plots}{List of [ggplot2::ggplot] objects}
#'     \item{config}{Configuration list from [steps_config()]}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-detect columns
#' result <- run_steps_pipeline("data/raw/steps_data.csv",
#'                              country_name = "Senegal",
#'                              survey_year = 2023)
#' result$key_indicators
#' result$plots$overview
#'
#' # Use a custom column mapping
#' result <- run_steps_pipeline("data/raw/steps_data.csv",
#'                              country_name = "Senegal",
#'                              survey_year = 2023,
#'                              mapping_file = "my_mapping.xlsx")
#' }
run_steps_pipeline <- function(data_path,
                               country_name = "Country Name",
                               survey_year = 2024,
                               age_min = 18,
                               age_max = 69,
                               output_dir = "outputs",
                               render_reports = TRUE,
                               mapping_file = NULL) {

  message("\n========================================")
  message(" WHO STEPS Analysis Pipeline")
  message("========================================\n")

  # -- 1. Import data ----------------------------------------------------------
  message("-- Step 1: Importing data ------------------------------------------")

  raw_data <- import_steps_data(data_path)

  if (!is.null(mapping_file)) {
    message("  Using column mapping from: ", mapping_file)
    cols <- read_column_mapping(mapping_file, data = raw_data)
  } else {
    cols <- detect_steps_columns(raw_data)
  }

  # -- 2. Clean data ----------------------------------------------------------
  message("-- Step 2: Cleaning data -----------------------------------------")
  clean_data <- clean_steps_data(raw_data, cols,
                                 age_min = age_min,
                                 age_max = age_max)

  # -- 3. Survey design -------------------------------------------------------
  message("-- Step 3: Survey design -----------------------------------------")
  design <- setup_survey_design(clean_data)

  # -- 4. Compute indicators --------------------------------------------------
  message("-- Step 4: Computing indicators --------------------------------")
  all_ind <- compute_all_indicators(design)
  indicators <- all_ind$results
  key_indicators <- all_ind$key_indicators

  # -- 5. Tables --------------------------------------------------------------
  message("-- Step 5: Generating tables -----------------------------------")
  tables <- build_steps_tables(indicators)

  # -- 6. Visualisations ------------------------------------------------------
  message("-- Step 6: Generating visualisations --------------------------")
  plots <- build_steps_plots(indicators, key_indicators,
                             country_name, survey_year)

  fig_dir <- file.path(output_dir, "figures")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  save_steps_plots(plots, fig_dir)

  # -- 7. Config for reports --------------------------------------------------
  config <- steps_config(
    data_path    = data_path,
    country_name = country_name,
    survey_year  = survey_year,
    age_min      = age_min,
    age_max      = age_max
  )

  # -- 8. Reports (optional) --------------------------------------------------
  if (render_reports) {
    message("-- Step 7: Rendering reports ----------------------------------")

    # Save intermediate results for Rmd templates
    proc_dir <- file.path(output_dir, "data", "processed")
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(indicators,     file.path(proc_dir, "indicators.rds"))
    saveRDS(key_indicators, file.path(proc_dir, "key_indicators.rds"))
    saveRDS(tables,         file.path(proc_dir, "tables.rds"))
    saveRDS(plots,          file.path(proc_dir, "plots.rds"))

    # Set data_dir so the Rmd template knows where to find the RDS files
    config$data_dir <- proc_dir

    tryCatch({
      render_fact_sheet(config, output_dir)
      render_data_book(config, output_dir)
      render_country_report(config, output_dir)
    }, error = function(e) {
      message(glue::glue("  \u26a0 Report rendering failed: {e$message}"))
    })
  }

  message(glue::glue("\n\u2713 Pipeline complete.\n"))

  list(
    raw_data       = raw_data,
    clean_data     = clean_data,
    cols           = cols,
    design         = design,
    indicators     = indicators,
    key_indicators = key_indicators,
    tables         = tables,
    plots          = plots,
    config         = config
  )
}

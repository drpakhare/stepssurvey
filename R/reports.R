#' Create STEPS analysis configuration
#'
#' Builds a configuration list that specifies data paths, design variables,
#' and report parameters for the STEPS pipeline.
#'
#' @param data_path Path to raw STEPS data file (CSV or Excel).
#' @param country_name Country name for reports (default "Country Name").
#' @param survey_year Survey year (default 2024).
#' @param age_min Minimum age (default 18).
#' @param age_max Maximum age (default 69).
#' @param weight_var Weight variable name (default "wt_final", set NULL if none).
#' @param strata_var Strata variable name (default "stratum", set NULL if none).
#' @param cluster_var Cluster variable name (default "psu", set NULL if none).
#' @param bp_sbp_threshold SBP threshold for raised BP (default 140).
#' @param bp_dbp_threshold DBP threshold for raised BP (default 90).
#' @param bmi_overweight BMI threshold for overweight (default 25.0).
#' @param bmi_obese BMI threshold for obesity (default 30.0).
#' @param glucose_threshold Fasting glucose threshold in mmol/L (default 7.0).
#' @param glucose_impaired_threshold Impaired fasting glucose threshold in mmol/L (default 6.1).
#' @param chol_threshold Total cholesterol threshold in mmol/L (default 5.0).
#'
#' @return A list with elements:
#'   - `data_path`: Input file path
#'   - `country_name`: Country name
#'   - `survey_year`: Survey year
#'   - `age_min`, `age_max`: Age range
#'   - `weight_var`, `strata_var`, `cluster_var`: Design variable names
#'   - Threshold parameters for BP, BMI, glucose, cholesterol
#'
#' @examples
#' \dontrun{
#'   cfg <- steps_config("data/steps_2023.csv", "Senegal", 2023)
#'   cfg <- steps_config("data/steps.csv", "Mongolia", 2019,
#'                       bp_sbp_threshold = 130, bp_dbp_threshold = 80)
#' }
#'
#' @export
steps_config <- function(data_path, country_name = "Country Name", survey_year = 2024,
                         age_min = 18, age_max = 69,
                         weight_var = "wt_final",
                         strata_var = "stratum",
                         cluster_var = "psu",
                         bp_sbp_threshold = 140, bp_dbp_threshold = 90,
                         bmi_overweight = 25.0, bmi_obese = 30.0,
                         glucose_threshold = 7.0,
                         glucose_impaired_threshold = 6.1,
                         chol_threshold = 5.0) {
  list(
    data_path                  = data_path,
    country_name               = country_name,
    survey_year                = survey_year,
    age_min                    = age_min,
    age_max                    = age_max,
    weight_var                 = weight_var,
    strata_var                 = strata_var,
    cluster_var                = cluster_var,
    bp_sbp_threshold           = bp_sbp_threshold,
    bp_dbp_threshold           = bp_dbp_threshold,
    bmi_overweight             = bmi_overweight,
    bmi_obese                  = bmi_obese,
    glucose_threshold          = glucose_threshold,
    glucose_impaired_threshold = glucose_impaired_threshold,
    chol_threshold             = chol_threshold
  )
}

#' Render STEPS Fact Sheet report
#'
#' Generates a Word document with an overview of key NCD risk factor
#' prevalence, including summary table and sex-stratified charts.
#'
#' @param config A list from [steps_config()] with survey metadata and paths.
#'   Expected to have `country_name`, `survey_year`, `age_min`, `age_max`.
#' @param output_dir Directory for output reports (default \code{tempdir()}).
#' @param format Output format: `"html"` for self-contained HTML (default)
#'   or `"word"` for Word (.docx).
#'
#' @return Path to generated output file (invisibly).
#'   Prints message with output location.
#'
#' @details
#' The fact sheet template uses pre-computed indicators, key_indicators,
#' and plots (via .rds files in data/processed/).
#' Requires rmarkdown, flextable, ggplot2, glue, patchwork packages.
#'
#' @export
render_fact_sheet <- function(config, output_dir = tempdir(),
                              format = c("html", "word")) {
  format <- match.arg(format)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  template_path <- system.file("rmd", "fact_sheet.Rmd", package = "stepssurvey")

  if (!file.exists(template_path)) {
    stop("Fact sheet template not found. Install stepssurvey package correctly.")
  }

  ext <- if (format == "html") "html" else "docx"
  output_file <- file.path(output_dir, paste0("fact_sheet.", ext))
  output_format <- if (format == "html") "html_document" else "word_document"

  tryCatch({
    rmarkdown::render(
      template_path,
      output_file = output_file,
      output_format = output_format,
      params = list(config = config, output_dir = output_dir),
      quiet = TRUE
    )
    message(glue::glue("\u2713 Fact sheet rendered: {output_file}"))
  },
  error = function(e) {
    message(glue::glue("\u2717 Failed to render fact sheet: {e$message}"))
  })

  invisible(output_file)
}

#' Render STEPS Data Book report
#'
#' Generates a Word document with detailed age-stratified prevalence tables
#' for all available indicators, organized by STEPS step.
#'
#' @param config A list from [steps_config()] with survey metadata.
#'   Expected to have `country_name`, `survey_year`, `age_min`, `age_max`.
#' @param output_dir Directory for output reports (default \code{tempdir()}).
#'
#' @return Path to generated Word document (invisibly).
#'   Prints message with output location.
#'
#' @details
#' Sections correspond to STEPS steps:
#'   - Step 1: Behavioural Risk Factors (tobacco, alcohol, diet, physical activity)
#'   - Step 2: Physical Measurements (overweight/obesity, blood pressure)
#'   - Step 3: Biochemical (glucose, cholesterol)
#'
#' Requires pre-computed tables and plots in data/processed/.
#'
#' @export
render_data_book <- function(config, output_dir = tempdir()) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  template_path <- system.file("rmd", "data_book.Rmd", package = "stepssurvey")

  if (!file.exists(template_path)) {
    stop("Data book template not found. Install stepssurvey package correctly.")
  }

  output_file <- file.path(output_dir, "data_book.docx")

  # Ensure config has data_dir pointing to where RDS files live
  if (is.null(config$data_dir)) {
    config$data_dir <- file.path(output_dir, "data")
  }

  tryCatch({
    rmarkdown::render(
      template_path,
      output_file = output_file,
      params = list(config = config),
      quiet = TRUE
    )
    message(glue::glue("\u2713 Data book rendered: {output_file}"))
  },
  error = function(e) {
    message(glue::glue("\u2717 Failed to render data book: {e$message}"))
  })

  invisible(output_file)
}

#' Render STEPS Country Report
#'
#' Generates a comprehensive Word document with executive summary,
#' indicator-by-indicator analysis, and recommendations for public health action.
#'
#' @param config A list from [steps_config()] with survey metadata.
#'   Expected to have `country_name`, `survey_year`, `age_min`, `age_max`.
#' @param output_dir Directory for output reports (default \code{tempdir()}).
#'
#' @return Path to generated Word document (invisibly).
#'   Prints message with output location.
#'
#' @details
#' Sections include:
#'   - Executive summary with key findings
#'   - Tobacco use
#'   - Physical activity
#'   - Overweight and obesity
#'   - Blood pressure
#'   - Blood glucose and cholesterol
#'   - Recommendations for public health action
#'   - Methodology
#'
#' Requires pre-computed indicators, tables, and plots in data/processed/.
#'
#' @export
render_country_report <- function(config, output_dir = tempdir()) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  template_path <- system.file("rmd", "country_report.Rmd", package = "stepssurvey")

  if (!file.exists(template_path)) {
    stop("Country report template not found. Install stepssurvey package correctly.")
  }

  output_file <- file.path(output_dir, "country_report.docx")

  # Ensure config has data_dir pointing to where RDS files live
  if (is.null(config$data_dir)) {
    # Default: look for a "data" subdirectory in output_dir
    config$data_dir <- file.path(output_dir, "data")
  }

  tryCatch({
    rmarkdown::render(
      template_path,
      output_file = output_file,
      params = list(config = config),
      quiet = TRUE
    )
    message(glue::glue("\u2713 Country report rendered: {output_file}"))
  },
  error = function(e) {
    message(glue::glue("\u2717 Failed to render country report: {e$message}"))
  })

  invisible(output_file)
}

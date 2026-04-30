#' Reports Module — UI
#'
#' @param id Module namespace id.
#' @keywords internal
mod_reports_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Report Generation",
      width = 280,
      shiny::actionButton(
        ns("btn_generate"),
        "Generate report",
        class = "btn-primary btn-lg",
        width = "100%",
        icon = shiny::icon("file-word")
      ),
      shiny::hr(),
      shiny::downloadButton(
        ns("dl_report"),
        "Download Summary Report",
        class = "btn-success btn-sm",
        style = "width:100%;",
        icon = shiny::icon("file-word")
      ),
      shiny::downloadButton(
        ns("dl_data_book"),
        "Download Data Book",
        class = "btn-success btn-sm",
        style = "width:100%;margin-top:5px;",
        icon = shiny::icon("book")
      ),
      shiny::hr(),
      shiny::h6("Fact Sheet"),
      shiny::downloadButton(
        ns("dl_fact_sheet_html"),
        "Fact Sheet (HTML)",
        class = "btn-info btn-sm",
        style = "width:100%;",
        icon = shiny::icon("file-lines")
      ),
      shiny::downloadButton(
        ns("dl_fact_sheet_word"),
        "Fact Sheet (Word)",
        class = "btn-info btn-sm",
        style = "width:100%;margin-top:5px;",
        icon = shiny::icon("file-word")
      ),
      shiny::hr(),
      shiny::downloadButton(
        ns("dl_tables_plots"),
        "Download tables & plots",
        class = "btn-outline-secondary btn-sm",
        style = "width:100%;margin-top:5px;",
        icon = shiny::icon("download")
      )
    ),

    # Progress and status
    bslib::card(
      bslib::card_header("Generation Status"),
      shiny::div(
        id = ns("status_container"),
        shiny::textOutput(ns("status_text")),
        shiny::br(),
        shiny::uiOutput(ns("progress_bar"))
      ),
      class = "mt-3"
    ),

    # Summary information
    bslib::card(
      bslib::card_header("Report Summary"),
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::value_box(
          "Report Status",
          shiny::textOutput(ns("report_status")),
          theme = "primary",
          showcase = shiny::icon("check-circle")
        ),
        bslib::value_box(
          "Output Files",
          shiny::textOutput(ns("n_files")),
          theme = "info",
          showcase = shiny::icon("folder")
        )
      ),
      class = "mt-3"
    )
  )
}


#' Reports Module — Server
#'
#' @param id Module namespace id.
#' @param upload_out Reactive list returned by [mod_upload_server()], containing
#'   `raw` (data frame), `cols` (column mapping), and `config` (survey configuration).
#'
#' @return A list with reactive elements:
#'   - `report_path`: Path to generated Word document
#'   - `output_dir`: Directory containing all outputs
#'   - `generation_status`: Current status string
#'
#' @keywords internal
#'
#' @details
#' The full pipeline is executed internally:
#' 1. Cleans data using [clean_steps_data()]
#' 2. Sets up survey design with [setup_survey_design()]
#' 3. Computes all indicators with [compute_all_indicators()]
#' 4. Builds tables with [build_steps_tables()]
#' 5. Builds plots with [build_steps_plots()]
#' 6. Saves all outputs to temp directory
#' 7. Renders report using [render_country_report()]
#'
#' Progress is shown via status text and progress bar during generation.
#' Errors are caught and displayed to the user via notifications.
#'
mod_reports_server <- function(id, upload_out) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Reactive values for tracking state
    report_state <- shiny::reactiveValues(
      status = "Ready",
      is_generating = FALSE,
      report_path = NULL,
      data_book_path = NULL,
      fact_sheet_html = NULL,
      fact_sheet_word = NULL,
      output_dir = NULL,
      output_files = list(),
      error_message = NULL
    )

    # Main report generation pipeline
    report_result <- shiny::eventReactive(input$btn_generate, {
      shiny::req(upload_out$raw(), upload_out$cols(), upload_out$config())

      report_state$is_generating <- TRUE
      report_state$status <- "Initializing..."
      report_state$error_message <- NULL

      tryCatch({
        # Step 0: Extract inputs
        report_state$status <- "Extracting data..."
        raw <- upload_out$raw()
        cols <- upload_out$cols()
        cfg <- upload_out$config()

        # Step 1: Clean data
        report_state$status <- "Cleaning data..."
        clean_data <- clean_steps_data(
          raw, cols,
          age_min = cfg$age_min,
          age_max = cfg$age_max,
          bp_sbp_threshold = cfg$bp_sbp_threshold,
          bp_dbp_threshold = cfg$bp_dbp_threshold,
          bmi_overweight = cfg$bmi_overweight,
          bmi_obese = cfg$bmi_obese,
          glucose_threshold = cfg$glucose_threshold,
          glucose_impaired_threshold = cfg$glucose_impaired_threshold,
          chol_threshold = cfg$chol_threshold
        )
        message(sprintf("  Cleaned data: %d rows, %d cols", nrow(clean_data), ncol(clean_data)))

        # Step 2: Setup survey design
        report_state$status <- "Setting up survey design..."
        design <- setup_survey_design(clean_data)
        message("  Survey design created")

        # Step 3: Compute all indicators
        report_state$status <- "Computing indicators..."
        indicators <- compute_all_indicators(design)
        message("  Indicators computed")

        # Step 4: Build summary tables (for country report)
        report_state$status <- "Building tables..."
        tables <- build_steps_tables(indicators$results)
        message("  Tables built")

        # Step 4b: Build detailed 3-panel tables (for data book)
        report_state$status <- "Building detailed tables..."
        computed_results <- tryCatch(
          compute_all_tables(design),
          error = function(e) {
            message(glue::glue("  Warning: Could not compute detailed tables: {e$message}"))
            NULL
          }
        )
        detailed_tables <- if (!is.null(computed_results)) {
          tryCatch(
            build_all_tables(computed_results),
            error = function(e) {
              message(glue::glue("  Warning: Could not build detailed tables: {e$message}"))
              NULL
            }
          )
        } else {
          NULL
        }
        message("  Detailed tables built")

        # Step 5: Build plots
        report_state$status <- "Building plots..."
        plots <- build_steps_plots(
          indicators$results,
          indicators$key_indicators,
          cfg$country_name,
          cfg$survey_year
        )
        message("  Plots built")

        # Step 6: Create temp output directory
        report_state$status <- "Saving outputs..."
        output_dir <- tempfile(pattern = "steps_report_")
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        data_dir <- file.path(output_dir, "data")
        dir.create(data_dir, showWarnings = FALSE)

        # Save indicators, tables, and plots as RDS
        saveRDS(indicators$results, file.path(data_dir, "indicators.rds"))
        saveRDS(indicators$key_indicators, file.path(data_dir, "key_indicators.rds"))
        saveRDS(tables, file.path(data_dir, "tables.rds"))
        saveRDS(plots, file.path(data_dir, "plots.rds"))

        # Save detailed 3-panel tables and computed results for data book
        if (!is.null(detailed_tables)) {
          saveRDS(detailed_tables, file.path(data_dir, "detailed_tables.rds"))
        }
        if (!is.null(computed_results)) {
          saveRDS(computed_results, file.path(data_dir, "computed_results.rds"))
        }

        # Save clean data for reference
        saveRDS(clean_data, file.path(data_dir, "clean_data.rds"))
        readr::write_csv(clean_data, file.path(data_dir, "clean_data.csv"))

        message(sprintf("  Outputs saved to %s", output_dir))

        # Step 7: Render country report (summary)
        report_state$status <- "Rendering summary report..."

        # Prepare config object for rendering
        # If cfg is not a steps_config object, ensure it has required fields
        if (!all(c("country_name", "survey_year", "age_min", "age_max") %in% names(cfg))) {
          cfg <- list(
            country_name = if (!is.null(cfg$country_name)) cfg$country_name else "Country Name",
            survey_year  = if (!is.null(cfg$survey_year))  cfg$survey_year  else 2024,
            age_min      = if (!is.null(cfg$age_min))      cfg$age_min      else 18,
            age_max      = if (!is.null(cfg$age_max))      cfg$age_max      else 69
          )
        }

        # Call render_country_report (summary)
        report_path <- render_country_report(cfg, output_dir)

        # If render_country_report doesn't return a path, construct it
        if (is.null(report_path)) {
          report_path <- file.path(output_dir, "country_report.docx")
        }

        # Render data book (detailed tables) if detailed tables were built
        data_book_path <- NULL
        if (!is.null(detailed_tables)) {
          report_state$status <- "Rendering data book..."
          data_book_path <- tryCatch({
            render_data_book(cfg, output_dir)
          }, error = function(e) {
            message(glue::glue("  Warning: Could not render data book: {e$message}"))
            NULL
          })
        }

        # Render fact sheets (HTML + Word)
        report_state$status <- "Rendering fact sheets..."
        fact_sheet_html <- tryCatch({
          render_fact_sheet(cfg, output_dir, format = "html")
        }, error = function(e) {
          message(glue::glue("  Warning: Could not render HTML fact sheet: {e$message}"))
          NULL
        })
        fact_sheet_word <- tryCatch({
          render_fact_sheet(cfg, output_dir, format = "word")
        }, error = function(e) {
          message(glue::glue("  Warning: Could not render Word fact sheet: {e$message}"))
          NULL
        })

        report_state$status <- "Complete"
        report_state$report_path <- report_path
        report_state$data_book_path <- data_book_path
        report_state$fact_sheet_html <- fact_sheet_html
        report_state$fact_sheet_word <- fact_sheet_word
        report_state$output_dir <- output_dir

        # List all output files
        all_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
        report_state$output_files <- all_files

        message(sprintf("Report generation complete: %s", report_path))

        list(
          success = TRUE,
          report_path = report_path,
          output_dir = output_dir,
          output_files = all_files
        )
      },
      error = function(e) {
        error_msg <- as.character(e$message)
        report_state$status <- "Error"
        report_state$error_message <- error_msg
        report_state$is_generating <- FALSE

        shiny::showNotification(
          sprintf("Report generation failed: %s", error_msg),
          type = "error",
          duration = 10
        )

        list(
          success = FALSE,
          error = error_msg
        )
      })
    })

    # Update is_generating flag when generation completes
    shiny::observe({
      if (!is.null(report_result())) {
        report_state$is_generating <- FALSE
      }
    })

    # Status text output
    output$status_text <- shiny::renderText({
      result <- report_result()
      if (is.null(result)) {
        return(report_state$status)
      }
      if (result$success) {
        "Report generated successfully!"
      } else {
        sprintf("Error: %s", result$error)
      }
    })

    # Progress bar
    output$progress_bar <- shiny::renderUI({
      if (report_state$is_generating) {
        shiny::div(
          class = "progress",
          style = "height: 20px;",
          shiny::div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            style = "width: 100%;",
            "Generating..."
          )
        )
      } else {
        NULL
      }
    })

    # Report status output
    output$report_status <- shiny::renderText({
      if (!is.null(report_state$report_path) &&
          file.exists(report_state$report_path)) {
        "Generated"
      } else if (!is.null(report_state$error_message)) {
        "Failed"
      } else {
        "Not generated"
      }
    })

    # Number of files output
    output$n_files <- shiny::renderText({
      if (length(report_state$output_files) > 0) {
        as.character(length(report_state$output_files))
      } else {
        "0"
      }
    })

    # Download handler for Word report
    output$dl_report <- shiny::downloadHandler(
      filename = function() {
        cfg <- upload_out$config()
        country_clean <- gsub("[^a-zA-Z0-9]", "_", cfg$country_name)
        sprintf("steps_country_report_%s_%d.docx",
                country_clean, cfg$survey_year)
      },
      content = function(file) {
        shiny::req(report_state$report_path)
        if (!file.exists(report_state$report_path)) {
          shiny::showNotification(
            "Report file not found. Please generate the report first.",
            type = "error"
          )
          return(NULL)
        }
        file.copy(report_state$report_path, file, overwrite = TRUE)
      }
    )

    # Download handler for Data Book
    output$dl_data_book <- shiny::downloadHandler(
      filename = function() {
        cfg <- upload_out$config()
        country_clean <- gsub("[^a-zA-Z0-9]", "_", cfg$country_name)
        sprintf("steps_data_book_%s_%d.docx",
                country_clean, cfg$survey_year)
      },
      content = function(file) {
        shiny::req(report_state$data_book_path)
        if (!file.exists(report_state$data_book_path)) {
          shiny::showNotification(
            "Data book not found. Please generate the report first.",
            type = "error"
          )
          return(NULL)
        }
        file.copy(report_state$data_book_path, file, overwrite = TRUE)
      }
    )

    # Download handler for tables and plots zip
    output$dl_tables_plots <- shiny::downloadHandler(
      filename = function() {
        cfg <- upload_out$config()
        country_clean <- gsub("[^a-zA-Z0-9]", "_", cfg$country_name)
        sprintf("steps_tables_plots_%s_%d.zip",
                country_clean, cfg$survey_year)
      },
      content = function(file) {
        shiny::req(report_state$output_dir)
        if (length(report_state$output_files) == 0) {
          shiny::showNotification(
            "No output files found. Please generate the report first.",
            type = "error"
          )
          return(NULL)
        }

        # Create temporary directory for zip contents
        temp_zip_dir <- tempfile()
        dir.create(temp_zip_dir, recursive = TRUE, showWarnings = FALSE)

        # Copy only data files (not including final report itself if we want)
        data_dir <- file.path(report_state$output_dir, "data")
        if (dir.exists(data_dir)) {
          file.copy(
            file.path(data_dir, c("tables.rds", "plots.rds", "clean_data.csv")),
            file.path(temp_zip_dir, c("tables.rds", "plots.rds", "clean_data.csv")),
            overwrite = TRUE
          )
        }

        # Create zip file
        zip(
          zipfile = file,
          files = list.files(temp_zip_dir, full.names = TRUE),
          flags = "-j"
        )

        # Clean up
        unlink(temp_zip_dir, recursive = TRUE)
      }
    )

    # Download handler for HTML fact sheet
    output$dl_fact_sheet_html <- shiny::downloadHandler(
      filename = function() {
        cfg <- upload_out$config()
        country_clean <- gsub("[^a-zA-Z0-9]", "_", cfg$country_name)
        sprintf("steps_fact_sheet_%s_%d.html",
                country_clean, cfg$survey_year)
      },
      content = function(file) {
        shiny::req(report_state$fact_sheet_html)
        if (!file.exists(report_state$fact_sheet_html)) {
          shiny::showNotification(
            "Fact sheet not found. Please generate the report first.",
            type = "error"
          )
          return(NULL)
        }
        file.copy(report_state$fact_sheet_html, file, overwrite = TRUE)
      }
    )

    # Download handler for Word fact sheet
    output$dl_fact_sheet_word <- shiny::downloadHandler(
      filename = function() {
        cfg <- upload_out$config()
        country_clean <- gsub("[^a-zA-Z0-9]", "_", cfg$country_name)
        sprintf("steps_fact_sheet_%s_%d.docx",
                country_clean, cfg$survey_year)
      },
      content = function(file) {
        shiny::req(report_state$fact_sheet_word)
        if (!file.exists(report_state$fact_sheet_word)) {
          shiny::showNotification(
            "Fact sheet not found. Please generate the report first.",
            type = "error"
          )
          return(NULL)
        }
        file.copy(report_state$fact_sheet_word, file, overwrite = TRUE)
      }
    )

    # Return reactive values for downstream use
    list(
      report_path = shiny::reactive({ report_state$report_path }),
      output_dir = shiny::reactive({ report_state$output_dir }),
      generation_status = shiny::reactive({ report_state$status }),
      output_files = shiny::reactive({ report_state$output_files })
    )
  })
}

#' Upload Module — UI
#' @param id Module namespace id.
#' @keywords internal
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Data & Settings",
      width = 320,

      shiny::fileInput(ns("file"), "Upload STEPS data",
                       accept = c(".csv", ".xlsx", ".xls", ".dta", ".sav")),

      shiny::actionButton(ns("use_demo"), "Use demo data",
                          class = "btn-outline-primary btn-sm mb-3",
                          width = "100%"),

      shiny::hr(),

      shiny::tags$details(
        shiny::tags$summary(shiny::tags$strong("Column mapping (optional)")),
        shiny::helpText(
          "Upload a filled mapping template if your variables have ",
          "non-standard names. Leave empty to use auto-detection."
        ),
        shiny::fileInput(ns("mapping_file"), NULL,
                         accept = c(".xlsx", ".xls", ".csv")),
        shiny::downloadButton(ns("dl_mapping_template"),
                              "Download blank template",
                              class = "btn-outline-secondary btn-sm mb-2",
                              style = "width:100%")
      ),

      shiny::hr(),

      shiny::textInput(ns("country"), "Country name", value = "Country"),
      shiny::numericInput(ns("year"), "Survey year", value = 2024,
                          min = 1990, max = 2030),
      shiny::sliderInput(ns("age_range"), "Age range",
                         min = 15, max = 100, value = c(18, 69)),

      shiny::hr(),
      shiny::h6("Indicator thresholds"),
      shiny::helpText("Customise thresholds for indicator definitions. WHO defaults shown."),

      # BP thresholds
      shiny::tags$details(
        shiny::tags$summary(shiny::tags$strong("Blood pressure")),
        shiny::helpText("Raised BP definition (default: 140/90 mmHg)."),
        shiny::fluidRow(
          shiny::column(6,
            shiny::numericInput(ns("bp_sbp"), "SBP >=", value = 140,
                                min = 100, max = 180, step = 5)
          ),
          shiny::column(6,
            shiny::numericInput(ns("bp_dbp"), "DBP >=", value = 90,
                                min = 60, max = 120, step = 5)
          )
        )
      ),

      # BMI thresholds
      shiny::tags$details(
        shiny::tags$summary(shiny::tags$strong("BMI")),
        shiny::helpText("WHO standard: overweight >=25, obese >=30."),
        shiny::fluidRow(
          shiny::column(6,
            shiny::numericInput(ns("bmi_ow"), "Overweight >=", value = 25.0,
                                min = 20, max = 30, step = 0.5)
          ),
          shiny::column(6,
            shiny::numericInput(ns("bmi_ob"), "Obese >=", value = 30.0,
                                min = 25, max = 40, step = 0.5)
          )
        )
      ),

      # Glucose thresholds
      shiny::tags$details(
        shiny::tags$summary(shiny::tags$strong("Fasting glucose")),
        shiny::helpText("mmol/L. WHO: raised >=7.0, impaired >=6.1."),
        shiny::fluidRow(
          shiny::column(6,
            shiny::numericInput(ns("gluc_raised"), "Raised >=", value = 7.0,
                                min = 5, max = 15, step = 0.1)
          ),
          shiny::column(6,
            shiny::numericInput(ns("gluc_impaired"), "Impaired >=", value = 6.1,
                                min = 4, max = 10, step = 0.1)
          )
        )
      ),

      # Cholesterol threshold
      shiny::tags$details(
        shiny::tags$summary(shiny::tags$strong("Total cholesterol")),
        shiny::helpText("mmol/L. WHO: raised >=5.0."),
        shiny::numericInput(ns("chol_raised"), "Raised >=", value = 5.0,
                            min = 3, max = 8, step = 0.1)
      ),

      shiny::hr(),
      shiny::h6("Column overrides"),
      shiny::helpText("Leave blank to use auto-detected columns."),
      shiny::uiOutput(ns("col_overrides"))
    ),

    # Main panel
    bslib::card(
      bslib::card_header("Raw data preview"),
      DT::DTOutput(ns("raw_table"))
    ),
    bslib::card(
      bslib::card_header("Detected STEPS columns"),
      DT::DTOutput(ns("detected_table"))
    )
  )
}


#' Upload Module — Server
#'
#' @param id Module namespace id.
#' @return A [shiny::reactiveValues] with elements `raw`, `cols`, `config`.
#' @keywords internal
mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(raw = NULL, cols = NULL)

    # -- Import from file upload ------------------------------------------------
    shiny::observeEvent(input$file, {
      req(input$file)
      imported <- import_steps_data(input$file$datapath)

      # Use mapping file if provided, otherwise auto-detect
      if (!is.null(input$mapping_file)) {
        rv$cols <- read_column_mapping(input$mapping_file$datapath, data = imported)
      } else {
        rv$cols <- detect_steps_columns(imported)
      }

      # Strip haven labels after detection (labels are used for
      # disambiguation in detect_steps_columns but cause issues
      # with DT::datatable and dim<- operations downstream)
      rv$raw   <- .strip_haven_labels(imported)
    })

    # -- Re-apply mapping if uploaded after data ---------------------------------
    shiny::observeEvent(input$mapping_file, {
      req(input$mapping_file, rv$raw)
      rv$cols <- read_column_mapping(input$mapping_file$datapath, data = rv$raw)
    })

    # -- Download blank mapping template -----------------------------------------
    output$dl_mapping_template <- shiny::downloadHandler(
      filename = function() "column_mapping_template.xlsx",
      content = function(file) {
        template_path <- system.file("templates",
                                      "column_mapping_template.xlsx",
                                      package = "stepssurvey")
        file.copy(template_path, file)
      }
    )

    # -- Import demo data -------------------------------------------------------
    shiny::observeEvent(input$use_demo, {
      rv$raw  <- generate_test_data(n = 3000, seed = 42)
      rv$cols <- detect_steps_columns(rv$raw)
    })

    # -- Column override dropdowns ----------------------------------------------
    output$col_overrides <- shiny::renderUI({
      shiny::req(rv$raw)
      choices <- c("(auto)" = "", sort(names(rv$raw)))

      key_cols <- c("age", "sex", "weight_step1", "weight_step2", "weight_step3",
                     "strata", "psu",
                     "tobacco_current", "alcohol_current",
                     "height", "weight", "sbp1", "fasting_glucose")

      purrr::map(key_cols, function(col_name) {
        detected <- rv$cols[[col_name]]
        label <- paste0(col_name, if (!is.null(detected)) paste0(" [", detected, "]") else " [not found]")
        shiny::selectInput(ns(paste0("ov_", col_name)), label,
                           choices = choices, selected = "",
                           width = "100%")
      })
    })

    # -- Apply overrides to cols ------------------------------------------------
    cols_final <- shiny::reactive({
      shiny::req(rv$cols)
      cols <- rv$cols
      key_cols <- c("age", "sex", "weight_step1", "weight_step2", "weight_step3",
                     "strata", "psu",
                     "tobacco_current", "alcohol_current",
                     "height", "weight", "sbp1", "fasting_glucose")
      for (col_name in key_cols) {
        ov <- input[[paste0("ov_", col_name)]]
        if (!is.null(ov) && nchar(ov) > 0) {
          cols[[col_name]] <- ov
        }
      }
      cols
    })

    # -- Raw data table ---------------------------------------------------------
    output$raw_table <- DT::renderDT({
      shiny::req(rv$raw)
      DT::datatable(utils::head(rv$raw, 100),
                    options = list(scrollX = TRUE, pageLength = 10),
                    rownames = FALSE)
    })

    # -- Detected columns table -------------------------------------------------
    output$detected_table <- DT::renderDT({
      shiny::req(rv$cols)
      detected <- rv$cols
      df <- data.frame(
        Variable   = names(detected),
        Detected   = sapply(detected, function(x) if (is.null(x)) "-- not found --" else x),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, options = list(pageLength = 50, dom = "t"),
                    rownames = FALSE)
    })

    # -- Return reactive config -------------------------------------------------
    config <- shiny::reactive({
      list(
        country_name              = input$country,
        survey_year               = input$year,
        age_min                   = input$age_range[1],
        age_max                   = input$age_range[2],
        bp_sbp_threshold          = input$bp_sbp,
        bp_dbp_threshold          = input$bp_dbp,
        bmi_overweight            = input$bmi_ow,
        bmi_obese                 = input$bmi_ob,
        glucose_threshold         = input$gluc_raised,
        glucose_impaired_threshold = input$gluc_impaired,
        chol_threshold            = input$chol_raised
      )
    })

    # Return values for downstream modules
    list(
      raw    = shiny::reactive(rv$raw),
      cols   = cols_final,
      config = config
    )
  })
}

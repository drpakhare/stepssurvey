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

      shiny::textInput(ns("country"), "Country name", value = "Country"),
      shiny::numericInput(ns("year"), "Survey year", value = 2024,
                          min = 1990, max = 2030),
      shiny::sliderInput(ns("age_range"), "Age range",
                         min = 15, max = 100, value = c(18, 69)),

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
      rv$cols  <- detect_steps_columns(imported)
      # Strip haven labels after detection (labels are used for
      # disambiguation in detect_steps_columns but cause issues
      # with DT::datatable and dim<- operations downstream)
      rv$raw   <- .strip_haven_labels(imported)
    })

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
        country_name = input$country,
        survey_year  = input$year,
        age_min      = input$age_range[1],
        age_max      = input$age_range[2]
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

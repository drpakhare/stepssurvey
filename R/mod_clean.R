#' Clean Module — UI
#' @param id Module namespace id.
#' @keywords internal
mod_clean_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Cleaning",
      width = 280,
      shiny::actionButton(ns("btn_clean"), "Clean data",
                          class = "btn-primary btn-lg", width = "100%",
                          icon = shiny::icon("broom")),
      shiny::hr(),
      shiny::downloadButton(ns("dl_clean"), "Download cleaned CSV",
                            class = "btn-outline-secondary btn-sm",
                            style = "width:100%;")
    ),

    # Summary cards
    bslib::layout_column_wrap(
      width = 1 / 4,
      bslib::value_box("Rows (raw)",    shiny::textOutput(ns("n_raw")),
                       theme = "primary", showcase = shiny::icon("database")),
      bslib::value_box("Rows (clean)",  shiny::textOutput(ns("n_clean")),
                       theme = "success", showcase = shiny::icon("check")),
      bslib::value_box("Rows dropped",  shiny::textOutput(ns("n_dropped")),
                       theme = "warning", showcase = shiny::icon("filter")),
      bslib::value_box("Variables",     shiny::textOutput(ns("n_vars")),
                       theme = "info",    showcase = shiny::icon("columns"))
    ),

    # Distribution checks
    bslib::navset_card_tab(
      title = "Derived variable distributions",
      bslib::nav_panel("Age & Sex",   DT::DTOutput(ns("tbl_demo"))),
      bslib::nav_panel("BMI",         DT::DTOutput(ns("tbl_bmi"))),
      bslib::nav_panel("BP staging",  DT::DTOutput(ns("tbl_bp"))),
      bslib::nav_panel("PA levels",   DT::DTOutput(ns("tbl_pa")))
    )
  )
}


#' Clean Module — Server
#'
#' @param id Module namespace id.
#' @param upload_out Reactive list returned by [mod_upload_server()].
#' @return A [shiny::reactive] returning the cleaned data frame.
#' @keywords internal
mod_clean_server <- function(id, upload_out) {
  shiny::moduleServer(id, function(input, output, session) {

    clean_data <- shiny::eventReactive(input$btn_clean, {
      shiny::req(upload_out$raw(), upload_out$cols())
      cfg <- upload_out$config()
      clean_steps_data(upload_out$raw(), upload_out$cols(),
                       age_min = cfg$age_min, age_max = cfg$age_max,
                       bp_sbp_threshold = cfg$bp_sbp_threshold,
                       bp_dbp_threshold = cfg$bp_dbp_threshold,
                       bmi_overweight = cfg$bmi_overweight,
                       bmi_obese = cfg$bmi_obese,
                       glucose_threshold = cfg$glucose_threshold,
                       glucose_impaired_threshold = cfg$glucose_impaired_threshold,
                       chol_threshold = cfg$chol_threshold)
    })

    # -- Summary value boxes ----------------------------------------------------
    output$n_raw     <- shiny::renderText({
      shiny::req(upload_out$raw()); format(nrow(upload_out$raw()), big.mark = ",")
    })
    output$n_clean   <- shiny::renderText({
      shiny::req(clean_data()); format(nrow(clean_data()), big.mark = ",")
    })
    output$n_dropped <- shiny::renderText({
      shiny::req(upload_out$raw(), clean_data())
      format(nrow(upload_out$raw()) - nrow(clean_data()), big.mark = ",")
    })
    output$n_vars    <- shiny::renderText({
      shiny::req(clean_data()); ncol(clean_data())
    })

    # -- Distribution tables ----------------------------------------------------
    output$tbl_demo <- DT::renderDT({
      shiny::req(clean_data())
      tbl <- as.data.frame(table(clean_data()$age_group, clean_data()$sex))
      names(tbl) <- c("Age group", "Sex", "n")
      DT::datatable(tbl, options = list(dom = "t", pageLength = 20),
                    rownames = FALSE)
    })

    output$tbl_bmi <- DT::renderDT({
      shiny::req(clean_data(), "bmi_category" %in% names(clean_data()))
      tbl <- as.data.frame(table(clean_data()$bmi_category, clean_data()$sex))
      names(tbl) <- c("BMI category", "Sex", "n")
      DT::datatable(tbl, options = list(dom = "t"), rownames = FALSE)
    })

    output$tbl_bp <- DT::renderDT({
      shiny::req(clean_data(), "bp_stage" %in% names(clean_data()))
      tbl <- as.data.frame(table(clean_data()$bp_stage, clean_data()$sex))
      names(tbl) <- c("BP stage", "Sex", "n")
      DT::datatable(tbl, options = list(dom = "t"), rownames = FALSE)
    })

    output$tbl_pa <- DT::renderDT({
      shiny::req(clean_data(), "pa_category" %in% names(clean_data()))
      tbl <- as.data.frame(table(clean_data()$pa_category, clean_data()$sex))
      names(tbl) <- c("PA level", "Sex", "n")
      DT::datatable(tbl, options = list(dom = "t"), rownames = FALSE)
    })

    # -- Download handler -------------------------------------------------------
    output$dl_clean <- shiny::downloadHandler(
      filename = function() paste0("steps_clean_", Sys.Date(), ".csv"),
      content  = function(file) {
        readr::write_csv(clean_data(), file)
      }
    )

    # Return
    clean_data
  })
}

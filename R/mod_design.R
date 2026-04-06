#' Design Module — UI
#' @param id Module namespace id.
#' @keywords internal
mod_design_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::actionButton(
        ns("btn_design"),
        "Set up design",
        class = "w-100",
        icon = shiny::icon("cogs")
      ),
      title = "Survey Design"
    ),

    bslib::card(
      bslib::card_header("Design Summary"),
      shiny::fluidRow(
        shiny::column(
          3,
          bslib::value_box(
            title = "Design Type",
            value = shiny::textOutput(ns("design_type")),
            theme = "primary"
          )
        ),
        shiny::column(
          3,
          bslib::value_box(
            title = "Unweighted N",
            value = shiny::textOutput(ns("unweighted_n")),
            theme = "info"
          )
        ),
        shiny::column(
          3,
          bslib::value_box(
            title = "Weighted N",
            value = shiny::textOutput(ns("weighted_n")),
            theme = "success"
          )
        ),
        shiny::column(
          3,
          bslib::value_box(
            title = "Strata",
            value = shiny::textOutput(ns("n_strata")),
            theme = "warning"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          3,
          bslib::value_box(
            title = "PSUs",
            value = shiny::textOutput(ns("n_psus")),
            theme = "danger"
          )
        )
      ),
      class = "mt-3"
    ),

    bslib::card(
      bslib::card_header("Design Elements"),
      DT::DTOutput(ns("design_table")),
      class = "mt-3"
    )
  )
}

#' Design Module — Server
#' @param id Module namespace id.
#' @param clean_out Reactive returning the cleaned data frame.
#' @return A [shiny::reactive] returning the survey design object.
#' @keywords internal
mod_design_server <- function(id, clean_out) {
  shiny::moduleServer(id, function(input, output, session) {

    # Create design when button clicked
    design <- shiny::eventReactive(
      input$btn_design,
      {
        shiny::req(clean_out())
        setup_survey_design(clean_out())
      },
      ignoreNULL = TRUE
    )

    # Extract design information
    design_info <- shiny::reactive({
      shiny::req(design())
      d <- design()

      # setup_survey_design now returns a steps_designs list
      d1 <- if (inherits(d, "steps_designs")) d$step1 else d

      # Get the variables data frame
      data <- d1$variables

      # Check for design elements
      has_weight <- "wt_step1" %in% names(data) || "wt_final" %in% names(data)
      has_strata <- "stratum" %in% names(data)
      has_cluster <- "psu" %in% names(data)

      # Determine design type
      if (has_strata && has_cluster && has_weight) {
        design_type <- "Stratified Clustered (Weighted)"
      } else if (has_strata && has_cluster) {
        design_type <- "Stratified Clustered"
      } else if (has_strata && has_weight) {
        design_type <- "Stratified (Weighted)"
      } else if (has_cluster && has_weight) {
        design_type <- "Clustered (Weighted)"
      } else if (has_strata) {
        design_type <- "Stratified"
      } else if (has_cluster) {
        design_type <- "Clustered"
      } else if (has_weight) {
        design_type <- "Weighted"
      } else {
        design_type <- "Simple"
      }

      # Calculate counts
      unweighted_n <- nrow(data)

      wt_col <- if ("wt_step1" %in% names(data)) "wt_step1" else if ("wt_final" %in% names(data)) "wt_final" else NULL
      weighted_n <- if (!is.null(wt_col)) {
        round(sum(data[[wt_col]], na.rm = TRUE), 0)
      } else {
        unweighted_n
      }

      n_strata <- if (has_strata) {
        length(unique(data$stratum[!is.na(data$stratum)]))
      } else {
        0
      }

      n_psus <- if (has_cluster) {
        length(unique(data$psu[!is.na(data$psu)]))
      } else {
        0
      }

      list(
        design_type = design_type,
        unweighted_n = unweighted_n,
        weighted_n = weighted_n,
        n_strata = n_strata,
        n_psus = n_psus,
        has_weight = has_weight,
        has_strata = has_strata,
        has_cluster = has_cluster
      )
    })

    # Render value boxes
    output$design_type <- shiny::renderText({
      shiny::req(design_info())
      design_info()$design_type
    })

    output$unweighted_n <- shiny::renderText({
      shiny::req(design_info())
      format(design_info()$unweighted_n, big.mark = ",")
    })

    output$weighted_n <- shiny::renderText({
      shiny::req(design_info())
      format(design_info()$weighted_n, big.mark = ",")
    })

    output$n_strata <- shiny::renderText({
      shiny::req(design_info())
      design_info()$n_strata
    })

    output$n_psus <- shiny::renderText({
      shiny::req(design_info())
      design_info()$n_psus
    })

    # Render design summary table
    output$design_table <- DT::renderDT({
      shiny::req(design_info())
      info <- design_info()

      summary_df <- data.frame(
        Element = c("Weight (Step 1)", "Weight (Step 2)", "Weight (Step 3)",
                     "Stratum Column", "PSU Column"),
        Detected = c(
          if (info$has_weight) "Yes (wt_step1)" else "No",
          if (info$has_weight) "Yes (wt_step2)" else "No",
          if (info$has_weight) "Yes (wt_step3)" else "No",
          if (info$has_strata) "Yes (stratum)" else "No",
          if (info$has_cluster) "Yes (psu)" else "No"
        ),
        Status = c(
          if (info$has_weight) "Active" else "Not used",
          if (info$has_weight) "Active" else "Not used",
          if (info$has_weight) "Active" else "Not used",
          if (info$has_strata) "Active" else "Not used",
          if (info$has_cluster) "Active" else "Not used"
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        summary_df,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(width = "150px", targets = 0)
          )
        ),
        rownames = FALSE
      )
    })

    # Return design reactive for downstream use
    design
  })
}

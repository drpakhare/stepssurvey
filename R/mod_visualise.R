#' Visualise Module — UI
#'
#' Creates the user interface for the visualisation module, including a "Generate plots"
#' action button and tabbed layout for displaying STEPS survey visualizations.
#'
#' @param id Module namespace id.
#'
#' @return A Shiny UI definition.
#'
#' @keywords internal
mod_visualise_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Visualisation",
      width = 280,
      shiny::actionButton(
        ns("btn_generate"),
        "Generate plots",
        class = "btn-primary btn-lg",
        width = "100%",
        icon = shiny::icon("chart-bar")
      ),
      shiny::hr(),
      shiny::downloadButton(
        ns("dl_plots"),
        "Download all plots (ZIP)",
        class = "btn-outline-secondary btn-sm",
        style = "width:100%;"
      )
    ),

    # Tabbed plot display
    bslib::navset_card_tab(
      title = "STEPS Visualizations",
      id = ns("plot_tabs"),
      bslib::nav_panel("Overview", shiny::plotOutput(ns("plot_overview"), height = "600px")),
      bslib::nav_panel("Forest plot", shiny::plotOutput(ns("plot_forest"), height = "600px")),
      bslib::nav_panel("Risk profile", shiny::plotOutput(ns("plot_radar"), height = "600px")),
      bslib::nav_panel("Tobacco by Sex", shiny::plotOutput(ns("plot_tobacco_sex"), height = "600px")),
      bslib::nav_panel("BP by Sex", shiny::plotOutput(ns("plot_bp_sex"), height = "600px")),
      bslib::nav_panel("Obesity by Sex", shiny::plotOutput(ns("plot_obesity_sex"), height = "600px")),
      bslib::nav_panel("Glucose by Sex", shiny::plotOutput(ns("plot_glucose_sex"), height = "600px")),
      bslib::nav_panel("BP by Age", shiny::plotOutput(ns("plot_bp_age"), height = "600px")),
      bslib::nav_panel("Obesity by Age", shiny::plotOutput(ns("plot_obesity_age"), height = "600px")),
      bslib::nav_panel("Sex Dashboard", shiny::plotOutput(ns("plot_sex_dashboard"), height = "700px"))
    )
  )
}


#' Visualise Module — Server
#'
#' Server logic for the visualisation module. Generates plots on button click,
#' renders them in tabs, and provides ZIP download functionality.
#'
#' @param id Module namespace id.
#' @param results_out Reactive list returned by [mod_indicators_server()], containing
#'   `results` (list of indicator results) and `key_indicators` (tibble of key indicators).
#' @param upload_out Reactive list returned by [mod_upload_server()], containing
#'   `config` (list with country_name and survey_year).
#'
#' @return NULL (invisible). This is a terminal module for visualisation.
#'
#' @details
#' The module calls [build_steps_plots()] with the reactive inputs to generate
#' a named list of ggplot2 objects. These are then rendered in individual tabs.
#' The download handler creates a ZIP archive of all plots as PNG files.
#'
#' @keywords internal
mod_visualise_server <- function(id, results_out, upload_out) {
  shiny::moduleServer(id, function(input, output, session) {

    # Generate plots on button click
    plots <- shiny::eventReactive(input$btn_generate, {
      shiny::req(results_out$results(), results_out$key_indicators(), upload_out$config())

      cfg <- upload_out$config()

      build_steps_plots(
        results_out$results(),
        results_out$key_indicators(),
        cfg$country_name,
        cfg$survey_year
      )
    })

    # -- Helper: safe plot renderer that catches margin errors -----------------
    safe_render_plot <- function(plot_name, label) {
      shiny::renderPlot({
        shiny::req(plots())
        p <- plots()[[plot_name]]
        shiny::validate(shiny::need(!is.null(p), paste(label, "not available")))
        p
      }, res = 96)
    }

    # -- Render individual plots ------------------------------------------------
    output$plot_overview      <- safe_render_plot("overview", "Overview plot")
    output$plot_forest        <- safe_render_plot("forest", "Forest plot")
    output$plot_radar         <- safe_render_plot("radar", "Risk profile radar")
    output$plot_tobacco_sex   <- safe_render_plot("tobacco_by_sex", "Tobacco by sex plot")
    output$plot_bp_sex        <- safe_render_plot("bp_by_sex", "BP by sex plot")
    output$plot_obesity_sex   <- safe_render_plot("obesity_by_sex", "Obesity by sex plot")
    output$plot_glucose_sex   <- safe_render_plot("glucose_by_sex", "Glucose by sex plot")
    output$plot_bp_age        <- safe_render_plot("bp_by_age", "BP by age plot")
    output$plot_obesity_age   <- safe_render_plot("obesity_by_age", "Obesity by age plot")
    output$plot_sex_dashboard <- safe_render_plot("sex_dashboard", "Sex dashboard")

    # -- Download handler -------------------------------------------------------
    output$dl_plots <- shiny::downloadHandler(
      filename = function() {
        cfg <- upload_out$config()
        glue::glue("STEPS_plots_{cfg$country_name}_{cfg$survey_year}_{Sys.Date()}.zip")
      },
      content = function(file) {
        shiny::req(plots())

        plot_list <- plots()
        cfg <- upload_out$config()

        # Create temporary directory for PNGs
        temp_dir <- tempdir()
        png_files <- character(0)

        # Save each plot as PNG
        plot_names <- c(
          "overview", "forest", "radar",
          "tobacco_by_sex", "bp_by_sex", "obesity_by_sex",
          "glucose_by_sex", "bp_by_age", "obesity_by_age", "sex_dashboard"
        )

        for (plot_name in plot_names) {
          if (!is.null(plot_list[[plot_name]])) {
            png_path <- file.path(temp_dir, glue::glue("{plot_name}.png"))
            ggplot2::ggsave(
              filename = png_path,
              plot = plot_list[[plot_name]],
              width = 10,
              height = 6,
              dpi = 300
            )
            png_files <- c(png_files, png_path)
          }
        }

        # Create ZIP archive
        zip(
          zipfile = file,
          files = png_files,
          flags = "-j"
        )

        # Clean up temporary files
        unlink(png_files)
      }
    )

    # Return NULL (terminal module)
    invisible(NULL)
  })
}

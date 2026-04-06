#' Main Shiny UI
#'
#' Builds the top-level UI using bslib sidebar page.
#'
#' @return A Shiny UI definition.
#' @keywords internal
app_ui <- function() {

  bslib::page_navbar(
    id       = "main_nav",
    title    = shiny::span(
      shiny::img(src = "logo.png", height = "36px",
                 style = "margin-right:10px; vertical-align:middle;"),
      "stepssurvey"
    ),
    theme    = bslib::bs_theme(
      version   = 5,
      bootswatch = "flatly",
      primary   = "#008DC9",
      "navbar-bg" = "#00427A"
    ),
    fillable = TRUE,

    # -- Tab 1: Upload & Detect ---
    bslib::nav_panel(
      title = "1. Upload",
      icon  = shiny::icon("upload"),
      mod_upload_ui("upload")
    ),

    # -- Tab 2: Clean ---
    bslib::nav_panel(
      title = "2. Clean",
      icon  = shiny::icon("broom"),
      mod_clean_ui("clean")
    ),

    # -- Tab 3: Survey Design ---
    bslib::nav_panel(
      title = "3. Design",
      icon  = shiny::icon("scale-balanced"),
      mod_design_ui("design")
    ),

    # -- Tab 4: Indicators ---
    bslib::nav_panel(
      title = "4. Indicators",
      icon  = shiny::icon("chart-bar"),
      mod_indicators_ui("indicators")
    ),

    # -- Tab 5: Visualisations ---
    bslib::nav_panel(
      title = "5. Visualise",
      icon  = shiny::icon("chart-line"),
      mod_visualise_ui("visualise")
    ),

    # -- Tab 6: Reports ---
    bslib::nav_panel(
      title = "6. Reports",
      icon  = shiny::icon("file-word"),
      mod_reports_ui("reports")
    )
  )
}

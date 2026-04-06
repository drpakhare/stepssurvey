#' Launch the stepssurvey Shiny Application
#'
#' Starts the interactive STEPS survey analysis app in the user's browser.
#' The app provides a guided workflow: upload data, clean, set survey design,
#' compute indicators, visualise results, and generate Word reports.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object (invisibly). Called for its side effect of
#'   launching the application.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   run_app()
#' }
run_app <- function(...) {
  # Make inst/www assets (logo) available to the app
  shiny::addResourcePath(
    "www",
    system.file("www", package = "stepssurvey")
  )

  shiny::shinyApp(
    ui     = app_ui(),
    server = app_server,
    ...
  )
}

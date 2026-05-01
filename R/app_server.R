#' Main Shiny Server
#'
#' Wires up all module servers and passes reactive outputs between them.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @keywords internal
app_server <- function(input, output, session) {


  # -- Module chain ----------------------------------------------------------
  # Each module returns reactive(s) consumed by the next.

  # 1. Upload: returns list(raw, cols, config)
  upload_out <- mod_upload_server("upload")

  # 2. Clean: returns reactive(clean_data)
  clean_out <- mod_clean_server("clean", upload_out)

  # 3. Quality: returns reactive(steps_quality)
  mod_quality_server("quality", upload_out, clean_out)

  # 4. Design: returns reactive(design)
  design_out <- mod_design_server("design", clean_out)

  # 5. Indicators: returns list(results, key_indicators)
  results_out <- mod_indicators_server("indicators", design_out)

  # 6. Visualise: consumes indicators + config
  mod_visualise_server("visualise", results_out, upload_out)

  # 7. Reports: consumes config (runs full pipeline internally)
  mod_reports_server("reports", upload_out)

  # About page
  mod_about_server("about")
}

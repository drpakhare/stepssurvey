#' Indicators Module — UI
#'
#' @param id Module namespace id.
#'
#' @return UI definition for indicators computation and display.
#'
#' @keywords internal
mod_indicators_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Indicators",
      width = 280,
      shiny::actionButton(ns("btn_compute"), "Compute indicators",
                          class = "btn-primary btn-lg", width = "100%",
                          icon = shiny::icon("calculator")),
      shiny::hr(),
      shiny::downloadButton(ns("dl_key_indicators"), "Download key indicators CSV",
                            class = "btn-outline-secondary btn-sm",
                            style = "width:100%;")
    ),

    # Summary value boxes
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::value_box("Domains computed", shiny::textOutput(ns("n_domains")),
                       theme = "primary", showcase = shiny::icon("layer-group")),
      bslib::value_box("Key indicators",  shiny::textOutput(ns("n_key_indicators")),
                       theme = "success",  showcase = shiny::icon("chart-line"))
    ),

    # Key indicators summary table
    bslib::navset_card_tab(
      title = "Results",
      bslib::nav_panel(
        "Key indicators",
        DT::DTOutput(ns("tbl_key_indicators"))
      ),
      bslib::nav_panel(
        "Tobacco",
        DT::DTOutput(ns("tbl_tobacco"))
      ),
      bslib::nav_panel(
        "Alcohol",
        DT::DTOutput(ns("tbl_alcohol"))
      ),
      bslib::nav_panel(
        "Diet & PA",
        DT::DTOutput(ns("tbl_diet_pa"))
      ),
      bslib::nav_panel(
        "Anthropometry",
        DT::DTOutput(ns("tbl_anthropometry"))
      ),
      bslib::nav_panel(
        "Blood Pressure",
        DT::DTOutput(ns("tbl_blood_pressure"))
      ),
      bslib::nav_panel(
        "Biochemical",
        DT::DTOutput(ns("tbl_biochemical"))
      )
    )
  )
}


#' Indicators Module — Server
#'
#' @param id Module namespace id.
#' @param design_out Reactive returning a survey::svydesign object.
#'
#' @return A list with two reactive elements:
#'   - `results`: reactive list of indicator results by domain
#'   - `key_indicators`: reactive tibble of headline key indicators
#'
#' @keywords internal
mod_indicators_server <- function(id, design_out) {
  shiny::moduleServer(id, function(input, output, session) {

    # -- Reactive computation of indicators ------------------------------------
    indicators_data <- shiny::eventReactive(input$btn_compute, {
      shiny::req(design_out())

      shiny::withProgress(message = "Computing indicators...", value = 0.1, {
        result <- tryCatch(
          {
            shiny::incProgress(0.2, detail = "Running indicator modules")
            res <- compute_all_indicators(design_out())
            shiny::incProgress(0.6, detail = "Done!")
            res
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error computing indicators:", e$message),
              type = "error", duration = 15
            )
            NULL
          }
        )
      })

      shiny::req(result)
      result
    })

    # Extract individual reactives for downstream modules
    results <- shiny::reactive({
      shiny::req(indicators_data())
      indicators_data()$results
    })

    key_indicators <- shiny::reactive({
      shiny::req(indicators_data())
      indicators_data()$key_indicators
    })

    # -- Summary value boxes ----------------------------------------------------
    output$n_domains <- shiny::renderText({
      shiny::req(results())
      length(results())
    })

    output$n_key_indicators <- shiny::renderText({
      shiny::req(key_indicators())
      nrow(key_indicators())
    })

    # -- Key indicators summary table -------------------------------------------
    output$tbl_key_indicators <- DT::renderDT({
      shiny::req(key_indicators())
      tbl <- key_indicators() |>
        dplyr::mutate(
          estimate = sprintf("%.1f%%", estimate),
          lower = sprintf("%.1f%%", lower),
          upper = sprintf("%.1f%%", upper)
        ) |>
        dplyr::rename(
          Domain = domain,
          Indicator = indicator,
          Estimate = estimate,
          "95% CI Lower" = lower,
          "95% CI Upper" = upper
        )
      DT::datatable(tbl,
                    options = list(dom = "t", pageLength = 15),
                    rownames = FALSE)
    })

    # -- Domain-specific result tables ------------------------------------------
    output$tbl_tobacco <- DT::renderDT({
      shiny::req(results())
      tbl_domain("Tobacco", results()$tobacco)
    })

    output$tbl_alcohol <- DT::renderDT({
      shiny::req(results())
      tbl_domain("Alcohol", results()$alcohol)
    })

    output$tbl_diet_pa <- DT::renderDT({
      shiny::req(results())
      tbl_domain("Diet & PA", results()$diet_pa)
    })

    output$tbl_anthropometry <- DT::renderDT({
      shiny::req(results())
      tbl_domain("Anthropometry", results()$anthropometry)
    })

    output$tbl_blood_pressure <- DT::renderDT({
      shiny::req(results())
      tbl_domain("Blood Pressure", results()$blood_pressure)
    })

    output$tbl_biochemical <- DT::renderDT({
      shiny::req(results())
      tbl_domain("Biochemical", results()$biochemical)
    })

    # -- Download handler -------------------------------------------------------
    output$dl_key_indicators <- shiny::downloadHandler(
      filename = function() paste0("steps_key_indicators_", Sys.Date(), ".csv"),
      content  = function(file) {
        readr::write_csv(key_indicators(), file)
      }
    )

    # -- Return list of reactives for downstream modules ----------------------
    list(
      results = results,
      key_indicators = key_indicators
    )
  })
}


#' Format indicator results tibble for display
#'
#' Internal helper to convert a domain's indicator results list into a
#' formatted data frame for DT table display.
#'
#' @param domain_name Character name of the domain for display.
#' @param domain_results List of indicator results for the domain.
#'
#' @return A formatted data frame ready for DT::datatable, or NULL if empty.
#'
#' @keywords internal
tbl_domain <- function(domain_name, domain_results) {
  if (is.null(domain_results) || length(domain_results) == 0) {
    return(data.frame())
  }

  # Convert list of indicator tibbles into a single long-format table
  result_list <- list()

  for (indicator_name in names(domain_results)) {
    ind_tbl <- domain_results[[indicator_name]]

    # Skip if not a data frame/tibble
    if (!is.data.frame(ind_tbl)) {
      next
    }

    # Ensure required columns exist
    if (!all(c("estimate", "lower", "upper") %in% names(ind_tbl))) {
      next
    }

    # Add indicator name column and format
    ind_tbl <- ind_tbl |>
      dplyr::mutate(
        indicator_name = indicator_name,
        .before = 1
      ) |>
      dplyr::select(indicator_name, dplyr::everything()) |>
      dplyr::mutate(
        estimate = sprintf("%.2f", estimate),
        lower = sprintf("%.2f", lower),
        upper = sprintf("%.2f", upper)
      )

    result_list[[indicator_name]] <- ind_tbl
  }

  if (length(result_list) == 0) {
    return(data.frame())
  }

  # Bind all indicator results
  tbl <- dplyr::bind_rows(result_list)

  # Rename columns for display
  if (nrow(tbl) > 0) {
    names(tbl) <- gsub("_", " ", tools::toTitleCase(names(tbl)))
  }

  DT::datatable(tbl,
                options = list(dom = "t", pageLength = 20, scrollX = TRUE),
                rownames = FALSE)
}

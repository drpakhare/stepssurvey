#' Data Quality Module — UI
#'
#' @param id Module namespace id.
#' @keywords internal
mod_quality_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Data Quality",
      width = 280,
      shiny::actionButton(
        ns("btn_run"), "Run diagnostics",
        class = "btn-primary btn-lg", width = "100%",
        icon = shiny::icon("stethoscope")
      ),
      shiny::hr(),
      shiny::h6("Digit preference"),
      shiny::selectInput(
        ns("dp_measure"), "Measurement",
        choices = c("SBP", "DBP", "Height", "Weight", "Waist"),
        selected = "SBP"
      ),
      shiny::hr(),
      shiny::h6("Weight diagnostics"),
      shiny::selectInput(
        ns("wt_step"), "Sampling weight",
        choices = c(
          "Step 1" = "weight_step1",
          "Step 2" = "weight_step2",
          "Step 3" = "weight_step3"
        ),
        selected = "weight_step1"
      )
    ),

    # ---- Summary cards ----
    bslib::layout_column_wrap(
      width = 1 / 4,
      bslib::value_box(
        "Heaping severity",
        shiny::textOutput(ns("vb_heaping")),
        theme = "primary",
        showcase = shiny::icon("hashtag")
      ),
      bslib::value_box(
        "Overall completeness",
        shiny::textOutput(ns("vb_completeness")),
        theme = "success",
        showcase = shiny::icon("check-circle")
      ),
      bslib::value_box(
        "Implausible values",
        shiny::textOutput(ns("vb_implausible")),
        theme = "warning",
        showcase = shiny::icon("exclamation-triangle")
      ),
      bslib::value_box(
        "Weight CV",
        shiny::textOutput(ns("vb_weight_cv")),
        theme = "info",
        showcase = shiny::icon("balance-scale")
      )
    ),

    # ---- Tabbed panels ----
    bslib::navset_card_tab(
      title = "Diagnostics",

      bslib::nav_panel(
        "Digit preference",
        shiny::plotOutput(ns("plot_dp"), height = "420px"),
        shiny::br(),
        DT::DTOutput(ns("tbl_dp_summary"))
      ),

      bslib::nav_panel(
        "Completeness",
        shiny::plotOutput(ns("plot_completeness"), height = "520px"),
        shiny::br(),
        DT::DTOutput(ns("tbl_completeness"))
      ),

      bslib::nav_panel(
        "Plausibility",
        DT::DTOutput(ns("tbl_plausibility"))
      ),

      bslib::nav_panel(
        "Sampling weights",
        shiny::plotOutput(ns("plot_weights"), height = "400px"),
        shiny::br(),
        DT::DTOutput(ns("tbl_weights"))
      )
    )
  )
}


#' Data Quality Module — Server
#'
#' @param id Module namespace id.
#' @param upload_out Reactive list returned by [mod_upload_server()], containing
#'   `raw` (data frame), `cols` (column mapping), and `config` (survey config).
#' @param clean_out Reactive returning the cleaned data frame from
#'   [mod_clean_server()].
#'
#' @return A reactive returning the `steps_quality` object (or NULL).
#' @keywords internal
mod_quality_server <- function(id, upload_out, clean_out) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Run diagnostics on button click
    dq <- shiny::eventReactive(input$btn_run, {
      shiny::req(upload_out$raw(), upload_out$cols())

      # Use cleaned data if available, otherwise pass raw twice
      cleaned <- tryCatch(clean_out(), error = function(e) NULL)
      if (is.null(cleaned)) cleaned <- upload_out$raw()

      shiny::withProgress(message = "Running data quality diagnostics...", {
        steps_data_quality(
          raw     = upload_out$raw(),
          cleaned = cleaned,
          cols    = upload_out$cols()
        )
      })
    })

    # ---- Value boxes ----

    output$vb_heaping <- shiny::renderText({
      shiny::req(dq())
      dp <- dq()$digit_preference
      if (length(dp) == 0) return("N/A")
      # Worst severity across all measures
      sevs <- vapply(dp, function(x) x$severity, character(1))
      sev_order <- c("None", "Mild", "Moderate", "Severe")
      worst <- sev_order[max(match(sevs, sev_order))]
      worst
    })

    output$vb_completeness <- shiny::renderText({
      shiny::req(dq())
      comp <- dq()$completeness
      all_rows <- do.call(rbind, comp)
      # Exclude "(not detected)" columns — they are expected missing
      detected <- all_rows[all_rows$column != "(not detected)", ]
      if (nrow(detected) == 0) return("N/A")
      overall_pct <- round(100 - mean(detected$pct_missing), 1)
      paste0(overall_pct, "%")
    })

    output$vb_implausible <- shiny::renderText({
      shiny::req(dq())
      pl <- dq()$plausibility
      if (nrow(pl) == 0) return("0")
      format(sum(pl$n_outlier), big.mark = ",")
    })

    output$vb_weight_cv <- shiny::renderText({
      shiny::req(dq())
      wt <- dq()$weights
      if (length(wt) == 0) return("N/A")
      # Show CV of step 1 weight by default
      first_wt <- wt[[1]]
      sprintf("%.3f", first_wt$cv)
    })

    # ---- Digit preference tab ----

    output$plot_dp <- shiny::renderPlot({
      shiny::req(dq(), input$dp_measure)
      dp <- dq()$digit_preference
      if (!input$dp_measure %in% names(dp)) {
        return(NULL)
      }
      plot_digit_preference(dq(), input$dp_measure)
    })

    output$tbl_dp_summary <- DT::renderDT({
      shiny::req(dq())
      dp <- dq()$digit_preference
      if (length(dp) == 0) return(NULL)

      summary_df <- data.frame(
        Measure      = names(dp),
        N            = vapply(dp, function(x) x$n, integer(1)),
        `Heaping (0)` = vapply(dp, function(x) x$heaping_0, numeric(1)),
        `Heaping (5)` = vapply(dp, function(x) x$heaping_5, numeric(1)),
        `Heaping (0+5)` = vapply(dp, function(x) x$heaping_0_5, numeric(1)),
        Severity     = vapply(dp, function(x) x$severity, character(1)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      DT::datatable(
        summary_df,
        options = list(dom = "t", pageLength = 10),
        rownames = FALSE
      )
    })

    # ---- Completeness tab ----

    output$plot_completeness <- shiny::renderPlot({
      shiny::req(dq())
      plot_completeness(dq())
    })

    output$tbl_completeness <- DT::renderDT({
      shiny::req(dq())
      comp <- dq()$completeness
      all_rows <- do.call(rbind, lapply(names(comp), function(domain) {
        df <- comp[[domain]]
        df$domain <- domain
        df
      }))
      all_rows <- all_rows[, c("domain", "variable", "column",
                                "n_total", "n_missing", "pct_missing")]

      DT::datatable(
        all_rows,
        options = list(
          pageLength = 30,
          scrollY = "400px",
          dom = "ft"
        ),
        rownames = FALSE,
        colnames = c("Domain", "Variable", "Column",
                     "Total", "Missing", "Missing %")
      ) |>
        DT::formatStyle(
          "pct_missing",
          backgroundColor = DT::styleInterval(
            c(5, 20, 50),
            c("#d4edda", "#fff3cd", "#f8d7da", "#e24b4a")
          )
        )
    })

    # ---- Plausibility tab ----

    output$tbl_plausibility <- DT::renderDT({
      shiny::req(dq())
      pl <- dq()$plausibility
      if (nrow(pl) == 0) return(NULL)

      pl$range <- sprintf("[%g, %g]", pl$range_low, pl$range_high)
      pl_display <- pl[, c("variable", "n_valid", "n_below", "n_above",
                            "n_outlier", "pct_outlier", "range")]

      DT::datatable(
        pl_display,
        options = list(dom = "t", pageLength = 20),
        rownames = FALSE,
        colnames = c("Variable", "Valid obs", "Below range",
                     "Above range", "Total outliers", "% Outlier",
                     "Plausible range")
      ) |>
        DT::formatStyle(
          "pct_outlier",
          backgroundColor = DT::styleInterval(
            c(1, 5),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # ---- Weights tab ----

    output$plot_weights <- shiny::renderPlot({
      shiny::req(dq(), input$wt_step)
      wt <- dq()$weights
      if (!input$wt_step %in% names(wt)) return(NULL)
      plot_weights(dq(), input$wt_step)
    })

    output$tbl_weights <- DT::renderDT({
      shiny::req(dq())
      wt <- dq()$weights
      if (length(wt) == 0) return(NULL)

      wt_df <- do.call(rbind, lapply(names(wt), function(nm) {
        w <- wt[[nm]]
        data.frame(
          Weight     = w$column,
          N_valid    = w$n_valid,
          N_NA       = w$n_na,
          N_zero     = w$n_zero,
          Min        = w$min,
          Median     = w$median,
          Mean       = w$mean,
          Max        = w$max,
          CV         = w$cv,
          `Max/Min`  = w$ratio_max_min,
          Severity   = w$cv_severity,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }))

      DT::datatable(
        wt_df,
        options = list(dom = "t", pageLength = 5),
        rownames = FALSE
      )
    })

    # Return dq object
    dq
  })
}

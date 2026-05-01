#' About Page Module UI
#'
#' Displays information about the stepssurvey package, authors,
#' and institutional affiliations.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#' @keywords internal
mod_about_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "max-width: 860px; margin: 0 auto; padding: 24px 20px;",

    # Logo and title
    shiny::div(
      style = "text-align: center; margin-bottom: 30px;",
      shiny::uiOutput(ns("logo_img")),
      shiny::h2("stepssurvey",
                 style = "color: #00427A; margin-top: 10px; font-weight: bold;"),
      shiny::h4("WHO STEPS NCD Survey Analysis Pipeline",
                 style = "color: #0072BC; margin-bottom: 4px;"),
      shiny::p(
        paste0("Version ", utils::packageVersion("stepssurvey")),
        style = "color: #888; font-size: 0.9em;"
      )
    ),

    # --- About the Package ---
    shiny::h4(
      shiny::icon("box-open"), " About the Package",
      style = "color: #00427A; border-bottom: 2px solid #008DC9; padding-bottom: 6px; margin-bottom: 14px;"
    ),
    shiny::p(
      "The WHO STEPwise Approach to NCD Risk Factor Surveillance (STEPS) is ",
      "the global standard for monitoring non-communicable disease risk factors. ",
      "Adopted by over 130 countries, it provides a standardised framework for ",
      "collecting behavioural, physical, and biochemical data across three steps: ",
      "questionnaire-based assessment of tobacco, alcohol, diet, and physical activity ",
      "(Step 1); physical measurements including blood pressure and anthropometry ",
      "(Step 2); and biochemical measurements of blood glucose and lipids (Step 3)."
    ),
    shiny::p(
      "Despite well-defined WHO protocols for data collection, ",
      "the analysis of STEPS data remains a significant bottleneck. Country teams ",
      "often rely on manual Epi Info or SPSS scripts that require substantial ",
      "statistical expertise, are error-prone, and produce inconsistent outputs. ",
      "This is particularly challenging for low- and middle-income countries (LMICs) ",
      "where trained biostatisticians are scarce and surveillance cycles are infrequent."
    ),
    shiny::p(
      shiny::tags$strong("stepssurvey"), " addresses this gap by providing a complete, ",
      "end-to-end analysis pipeline in R that automates the entire workflow ",
      "from raw survey data to publication-ready reports, faithfully implementing ",
      "WHO-standard methodology for all ten headline NCD risk factor indicators. ",
      "It supports CSV, Excel, Stata, and SPSS formats; implements WHO-standard ",
      "cleaning rules (smk_cln/smkless_cln, GPAQ P_clean); handles complex survey ",
      "designs with step-specific weights; and produces formatted tables, ",
      "WHO-branded visualisations, and three Word report templates."
    ),
    shiny::p(
      "The package has been validated against published WHO fact sheets from ",
      "nine countries across six WHO regions, achieving ",
      shiny::tags$strong("96% concordance"),
      " (123 of 128 indicators within 1 percentage point of published values) with ",
      "100% overlapping 95% confidence intervals.",
      style = "margin-bottom: 28px;"
    ),

    # --- About AIIMS Bhopal ---
    shiny::h4(
      shiny::icon("university"), " About AIIMS Bhopal",
      style = "color: #00427A; border-bottom: 2px solid #008DC9; padding-bottom: 6px; margin-bottom: 14px;"
    ),
    shiny::p(
      "The All India Institute of Medical Sciences (AIIMS), Bhopal is an Institute ",
      "of National Importance established in 2012, serving as a centre of excellence ",
      "in medical education, research, and tertiary healthcare for central India. ",
      "The Department of Community and Family Medicine has extensive experience in ",
      "population-based NCD surveillance, having conducted STEPS-aligned surveys ",
      "and large-scale epidemiological studies across Madhya Pradesh.",
      style = "margin-bottom: 28px;"
    ),

    # --- Authors ---
    shiny::h4(
      shiny::icon("users"), " Authors",
      style = "color: #00427A; border-bottom: 2px solid #008DC9; padding-bottom: 6px; margin-bottom: 14px;"
    ),

    # Dr Abhijit Pakhare
    shiny::p(
      shiny::tags$strong("Dr Abhijit Pakhare"),
      " — Professor, Department of Community and Family Medicine, AIIMS Bhopal"
    ),

    # Dr Ankur Joshi
    shiny::p(
      shiny::tags$strong("Dr Ankur Joshi"),
      " — Additional Professor, Department of Community and Family Medicine, AIIMS Bhopal",
      style = "margin-bottom: 20px;"
    ),

    # AI acknowledgement
    shiny::p(
      shiny::tags$strong("AI-Assisted Development: "),
      "Substantial portions of this package were developed with the assistance of ",
      "Claude (Anthropic), an AI coding assistant. This includes the Shiny application, ",
      "automated test suite, visualisation functions, report templates, and the ",
      "multi-country validation pipeline. The domain expertise, epidemiological ",
      "methodology, and analytical decisions are entirely the authors'; Claude ",
      "served as a development accelerator that enabled two researchers to build ",
      "and validate a tool that would otherwise require a dedicated software ",
      "engineering team. All AI-generated code was reviewed, tested, and validated ",
      "by the authors against published WHO outputs.",
      style = "color: #666; font-style: italic; margin-bottom: 28px;"
    ),

    # --- Citation ---
    shiny::h4(
      shiny::icon("quote-left"), " Citation",
      style = "color: #00427A; border-bottom: 2px solid #008DC9; padding-bottom: 6px; margin-bottom: 14px;"
    ),
    shiny::p("If you use stepssurvey in your research or surveillance work, please cite:"),
    shiny::tags$blockquote(
      style = paste0(
        "background: #f8f9fa; padding: 12px 16px; border-left: 4px solid #0072BC; ",
        "font-family: monospace; font-size: 0.9em; white-space: pre-wrap; ",
        "margin-bottom: 14px;"
      ),
      paste0(
        "Pakhare A, Joshi A (2026). stepssurvey: Analyse WHO STEPS Survey Data.\n",
        "R package version 0.1.0.\n",
        "https://github.com/drpakhare/stepssurvey"
      )
    ),
    shiny::p(
      shiny::tags$strong("License: "), "MIT",
      " | ",
      shiny::tags$strong("Source: "),
      shiny::tags$a(href = "https://github.com/drpakhare/stepssurvey",
                    "GitHub", target = "_blank"),
      " | ",
      shiny::tags$strong("Docs: "),
      shiny::tags$a(href = "https://drpakhare.github.io/stepssurvey/",
                    "Package website", target = "_blank"),
      " | ",
      shiny::tags$strong("Issues: "),
      shiny::tags$a(href = "https://github.com/drpakhare/stepssurvey/issues",
                    "Bug reports", target = "_blank"),
      style = "font-size: 0.9em;"
    )
  )
}

#' About Page Module Server
#'
#' @param id Module namespace ID.
#' @keywords internal
mod_about_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$logo_img <- shiny::renderUI({
      logo_path <- system.file("www", "logo.png", package = "stepssurvey")
      if (nzchar(logo_path)) {
        shiny::img(src = "www/logo.png", height = "120px",
                   alt = "stepssurvey logo")
      }
    })
  })
}

# stepssurvey 0.1.0

Initial CRAN release.

## Features

* **Data import**: Read WHO STEPS survey data from CSV, Excel (.xlsx/.xls),
  Stata (.dta), and SPSS (.sav) formats. Automatic column detection maps
  variable names across different STEPS instrument versions.

* **Column mapping**: Support for non-standard datasets via an Excel/CSV
  mapping template (`read_column_mapping()`). A blank template is bundled at
  `inst/templates/column_mapping_template.xlsx`.

* **Data cleaning**: WHO-standard recoding of tobacco (with smk_cln/smkless_cln
  data-quality filters), alcohol (skip-pattern aware), diet, physical activity
  (GPAQ with MET-minutes), anthropometry (BMI, waist-hip ratio), blood pressure
  (mean of last two of three readings), and biochemical measurements.

* **Configurable thresholds**: User-adjustable cut-points for raised blood

  pressure (default 140/90 mmHg), BMI overweight/obesity (default 25/30),
  fasting glucose (default 7.0 mmol/L), and total cholesterol (default
  5.0 mmol/L).

* **Complex survey analysis**: Automatic setup of `survey::svydesign()` with
  STEPS-specific strata, PSU, and up to three weight variables (WStep1,
  WStep2, WStep3). Computes weighted prevalence estimates with 95% confidence
  intervals for all standard NCD indicators, stratified by sex and age group.

* **Data quality diagnostics**: Terminal-digit preference analysis, variable
  completeness heatmaps, plausibility range checks, and sampling-weight
  diagnostics (`steps_data_quality()`).

* **Visualisations**: Forest plot of key indicators, risk-profile radar chart,
  sex-stratified bar charts, age-trend line plots, and overview dashboard
  (`build_steps_plots()`, `build_forest_plot()`, `build_radar_plot()`).
  WHO STEPS colour palette and ggplot2 theme included.

* **Report generation**: One-click WHO-style fact sheet in HTML or Word format
  (`render_fact_sheet()`), plus structured indicator tables
  (`build_steps_tables()`).

* **Interactive Shiny app**: Point-and-click interface for the full pipeline
  (`run_app()`), with data upload, threshold configuration, results dashboard,
  data quality tab, and report download.

* **Validation**: Tested against published WHO fact sheets from Moldova (2021),
  Mongolia (2019), and Georgia (2016), with detailed concordance documented in
  the validation vignette.

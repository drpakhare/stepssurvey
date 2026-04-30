#' Build publication-ready STEPS visualizations
#'
#' Generates a list of ggplot2 plots showing key NCD risk factor prevalence
#' with 95% confidence intervals, stratified by sex and age group.
#'
#' @param indicators A list of indicator results from [compute_all_indicators()].
#' @param key_indicators A data frame with key indicators (domain, indicator, estimate, lower, upper).
#' @param country_name Country name for plot titles.
#' @param survey_year Survey year for plot titles.
#'
#' @return A named list of ggplot2 objects:
#'   - `overview`: Horizontal bar chart of key indicators
#'   - `tobacco_by_sex`: Sex-stratified tobacco use
#'   - `bp_by_sex`: Sex-stratified blood pressure
#'   - `obesity_by_sex`: Sex-stratified overweight/obesity
#'   - `glucose_by_sex`: Sex-stratified blood glucose
#'   - `bp_by_age`: Age-stratified blood pressure with ribbon CI
#'   - `obesity_by_age`: Age-stratified overweight/obesity with ribbon CI
#'   - `sex_dashboard`: Combined 2x2 dashboard of sex-stratified charts (if >=2 sex plots available)
#'   NULL entries are preserved in the list.
#'
#' @details
#' All plots use the WHO STEPS colour scheme and professional styling.
#' Error bars represent 95% confidence intervals.
#' Prevalence values are displayed on bars/points with light background text.
#'
#' @examples
#' \dontrun{
#'   all_ind <- compute_all_indicators(design)
#'   indicators <- all_ind$results
#'   key_indicators <- all_ind$key_indicators
#'   plots <- build_steps_plots(indicators, key_indicators, "Senegal", 2023)
#'   names(plots)  # View available plots
#' }
#'
#' @export
build_steps_plots <- function(indicators, key_indicators, country_name, survey_year) {
  plots <- list()

  # Ensure theme functions are available
  theme_steps <- theme_steps()
  steps_colors <- steps_colors()
  config <- list(country_name = country_name, survey_year = survey_year)

  # - 1. Key indicators overview (horizontal bar chart)
  # Wrap long indicator labels so they fit in smaller plot areas
  overview_df <- key_indicators |>
    dplyr::arrange(estimate) |>
    dplyr::mutate(
      indicator = vapply(indicator, function(x) paste(strwrap(x, width = 30), collapse = "\n"), character(1)),
      indicator = factor(indicator, levels = indicator)
    )

  plots$overview <- ggplot2::ggplot(overview_df,
      ggplot2::aes(x = estimate, y = indicator, fill = domain)) +
    ggplot2::geom_col(width = 0.6, alpha = 0.9) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.25, color = "grey40") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(estimate, 1), "%")),
              hjust = -0.15, size = 3.2, color = "grey20") +
    ggplot2::scale_x_continuous(limits = c(0, 105), labels = function(x) paste0(x, "%")) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title    = glue::glue("NCD Risk Factor Prevalence - {config$country_name} {config$survey_year}"),
      subtitle = "Weighted estimates with 95% confidence intervals",
      x        = "Prevalence (%)",
      y        = NULL,
      caption  = "Source: WHO STEPS Survey"
    ) +
    theme_steps +
    ggplot2::theme(
      legend.position = "right",
      plot.margin = ggplot2::margin(5, 10, 5, 5)
    )

  # - 2. By-sex comparison (grouped bar chart)
  # Prefer current_tobacco_any (smoking + smokeless) if available
  tob_by_sex <- indicators$tobacco[["current_tobacco_any_by_sex"]]
  if (is.null(tob_by_sex)) tob_by_sex <- indicators$tobacco[["current_tobacco_by_sex"]]
  plots$tobacco_by_sex <- make_sex_chart(
    tob_by_sex,
    "Current tobacco use by sex", "current_tobacco",
    steps_colors, theme_steps
  )
  plots$bp_by_sex <- make_sex_chart(
    indicators$blood_pressure$raised_bp_by_sex,
    "Raised blood pressure by sex", "raised_bp",
    steps_colors, theme_steps
  )
  plots$obesity_by_sex <- make_sex_chart(
    indicators$anthropometry$overweight_obese_by_sex,
    "Overweight or obese (BMI >=25) by sex", "overweight_obese",
    steps_colors, theme_steps
  )
  plots$glucose_by_sex <- make_sex_chart(
    indicators$biochemical$raised_glucose_by_sex,
    "Raised fasting blood glucose by sex", "raised_glucose",
    steps_colors, theme_steps
  )

  # - 3. By-age trend chart
  plots$bp_by_age <- make_age_chart(
    indicators$blood_pressure$raised_bp_by_age,
    "Raised BP by age group",
    steps_colors, theme_steps
  )
  plots$obesity_by_age <- make_age_chart(
    indicators$anthropometry$overweight_obese_by_age,
    "Overweight/Obese by age group",
    steps_colors, theme_steps
  )

  # - 4. Forest plot
  plots$forest <- build_forest_plot(key_indicators, config$country_name, config$survey_year)

  # - 5. Radar chart
  plots$radar <- build_radar_plot(key_indicators, config$country_name, config$survey_year)

  # - 6. Combined dashboard (patchwork)
  sex_plots <- Filter(Negate(is.null), list(
    plots$tobacco_by_sex, plots$bp_by_sex,
    plots$obesity_by_sex, plots$glucose_by_sex
  ))

  if (length(sex_plots) >= 2) {
    plots$sex_dashboard <- patchwork::wrap_plots(sex_plots, ncol = 2) +
      patchwork::plot_annotation(
        title   = glue::glue("{config$country_name} {config$survey_year} - NCD Risk Factors by Sex"),
        caption = "Source: WHO STEPS Survey",
        theme   = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = steps_colors$dark_blue))
      )
  }

  return(plots)
}

#' Create sex-stratified bar chart
#'
#' @param by_sex_df Data frame with `sex`, `estimate`, `lower`, `upper` columns.
#' @param title Plot title.
#' @param var_name Variable name (for internal use).
#' @param steps_colors Colour list from [steps_colors()].
#' @param theme_steps Theme from [theme_steps()].
#'
#' @return A ggplot2 object or NULL if input is NULL.
#'
#' @keywords internal
make_sex_chart <- function(by_sex_df, title, var_name, steps_colors, theme_steps) {
  if (is.null(by_sex_df)) return(NULL)

  ggplot2::ggplot(by_sex_df, ggplot2::aes(x = sex, y = estimate, fill = sex)) +
    ggplot2::geom_col(width = 0.5, alpha = 0.9) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.15) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(estimate, 1), "%")),
              vjust = -0.5, size = 3.5, fontface = "bold") +
    ggplot2::scale_fill_manual(values = c(Male = steps_colors$male, Female = steps_colors$female)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = title, x = NULL, y = "Prevalence (%)") +
    theme_steps +
    ggplot2::theme(legend.position = "none")
}

#' Create age-stratified trend chart
#'
#' @param by_age_df Data frame with `age_group`, `estimate`, `lower`, `upper` columns.
#' @param title Plot title.
#' @param steps_colors Colour list from [steps_colors()].
#' @param theme_steps Theme from [theme_steps()].
#'
#' @return A ggplot2 object or NULL if input is NULL.
#'
#' @keywords internal
make_age_chart <- function(by_age_df, title, steps_colors, theme_steps) {
  if (is.null(by_age_df)) return(NULL)

  ggplot2::ggplot(by_age_df, ggplot2::aes(x = age_group, y = estimate, group = 1)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = steps_colors$blue, alpha = 0.15) +
    ggplot2::geom_line(color = steps_colors$blue, linewidth = 1.2) +
    ggplot2::geom_point(color = steps_colors$dark_blue, size = 3, fill = "white", shape = 21, stroke = 2) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(estimate, 1), "%")),
              vjust = -1, size = 3, color = steps_colors$dark_blue) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::labs(title = title, x = "Age group (years)", y = "Prevalence (%)") +
    theme_steps
}

#' Build forest plot of key indicators with 95% CIs
#'
#' Creates a horizontal point-and-CI plot (forest plot style) for all
#' key indicators, grouped by domain.
#'
#' @param key_indicators A data frame with domain, indicator, estimate, lower, upper.
#' @param country_name Country name for title.
#' @param survey_year Survey year for title.
#'
#' @return A ggplot2 object.
#' @export
build_forest_plot <- function(key_indicators, country_name, survey_year) {
  theme_steps <- theme_steps()
  steps_colors <- steps_colors()

  df <- key_indicators |>
    dplyr::mutate(
      indicator = vapply(indicator, function(x) paste(strwrap(x, width = 35), collapse = "\n"), character(1)),
      indicator = factor(indicator, levels = rev(indicator))
    )

  ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = indicator, colour = domain)) +
    ggplot2::geom_vline(xintercept = c(10, 20, 30, 40, 50), linetype = "dotted",
                        colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                            height = 0.3, linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", estimate)),
                       hjust = -0.3, size = 3, colour = "grey30") +
    ggplot2::scale_x_continuous(limits = c(0, 100),
                                labels = function(x) paste0(x, "%"),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::labs(
      title    = glue::glue("NCD Risk Factor Prevalence -- {country_name} {survey_year}"),
      subtitle = "Point estimates with 95% confidence intervals",
      x        = "Prevalence (%)",
      y        = NULL,
      colour   = "Domain",
      caption  = "Source: WHO STEPS Survey"
    ) +
    theme_steps +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


#' Build radar / spider chart of NCD risk factor profile
#'
#' Creates a radar-style chart showing prevalence of key risk factors
#' on a polar coordinate system for quick visual comparison.
#'
#' @param key_indicators A data frame with domain, indicator, estimate.
#' @param country_name Country name for title.
#' @param survey_year Survey year for title.
#'
#' @return A ggplot2 object.
#' @export
build_radar_plot <- function(key_indicators, country_name, survey_year) {
  theme_steps <- theme_steps()
  steps_colors <- steps_colors()

  # Use short labels for radar
  short_labels <- c(
    "Current tobacco use" = "Tobacco",
    "Current alcohol use" = "Alcohol",
    "Heavy episodic drinking" = "HED",
    "Insufficient physical activity" = "Low PA",
    "Low fruit & vegetable intake (<5 servings/day)" = "Low F&V",
    "Overweight or obese (BMI >=25)" = "OW/Obese",
    "Obese (BMI >=30)" = "Obese",
    "Raised blood pressure" = "Raised BP",
    "Raised fasting blood glucose" = "Raised glucose",
    "Raised total cholesterol" = "Raised chol"
  )

  df <- key_indicators |>
    dplyr::mutate(
      short_label = dplyr::if_else(
        indicator %in% names(short_labels),
        short_labels[indicator],
        substr(indicator, 1, 15)
      ),
      short_label = factor(short_label, levels = short_label)
    )

  ggplot2::ggplot(df, ggplot2::aes(x = short_label, y = estimate, fill = domain)) +
    ggplot2::geom_col(width = 0.8, alpha = 0.75, colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", estimate)),
                       size = 3, fontface = "bold",
                       position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::coord_polar(start = 0) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(25, 50, 75)) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = glue::glue("{country_name} {survey_year} -- Risk Factor Profile"),
      fill  = "Domain",
      x = NULL, y = NULL
    ) +
    theme_steps +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
      legend.position = "bottom"
    )
}


#' Save STEPS plots to PNG files
#'
#' Exports all plots in a list to PNG files in the specified directory.
#'
#' @param plots A named list of ggplot2 objects (from [build_steps_plots()]).
#' @param output_dir Output directory path (default "outputs/figures").
#'
#' @return NULL (invisibly). Prints messages about saved files.
#'
#' @details
#' Files are named:
#'   - `01_overview_indicators.png` (12x8 in)
#'   - `02_by_sex_dashboard.png` (12x8 in)
#'   - `03_bp_by_age.png` (10x6 in)
#'   - `04_obesity_by_age.png` (10x6 in)
#'
#' All saved at 150 dpi with white background.
#'
#' @export
save_steps_plots <- function(plots, output_dir = "outputs/figures") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  save_plot <- function(plot, name, width = 10, height = 6) {
    if (!is.null(plot)) {
      ggplot2::ggsave(
        file.path(output_dir, paste0(name, ".png")),
        plot = plot, width = width, height = height, dpi = 150, bg = "white"
      )
    }
  }

  save_plot(plots$overview,       "01_overview_indicators", width = 12, height = 8)
  save_plot(plots$sex_dashboard,  "02_by_sex_dashboard",    width = 12, height = 8)
  save_plot(plots$bp_by_age,      "03_bp_by_age")
  save_plot(plots$obesity_by_age, "04_obesity_by_age")

  n_saved <- sum(!sapply(plots, is.null))
  message(glue::glue("\u2713 Saved {n_saved} plots to {output_dir}/"))

  invisible(NULL)
}

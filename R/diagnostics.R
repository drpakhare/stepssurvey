#' Data Quality Diagnostics for WHO STEPS Data
#'
#' Produces a comprehensive data quality report covering digit preference,
#' completeness, plausibility, and sampling weight diagnostics.
#'
#' @param raw  The raw (pre-cleaning) data frame, typically from
#'   [import_steps_data()].
#' @param cleaned The cleaned data frame from [clean_steps_data()].
#' @param cols Column mapping list from [detect_steps_columns()].
#'
#' @return A list of class `"steps_quality"` with elements:
#'   \describe{
#'     \item{digit_preference}{Terminal-digit tables and heaping indices for
#'       physical measurements (SBP, DBP, height, weight, waist).}
#'     \item{completeness}{Per-variable missingness counts and percentages,
#'       grouped by STEPS domain.}
#'     \item{plausibility}{Summary of values outside plausible ranges.}
#'     \item{weights}{Sampling weight distribution statistics.}
#'   }
#'
#' @details
#' **Digit preference / heaping** is assessed using the Whipple-style
#' heaping index: the ratio of observed frequency at a digit (0 or 5)
#' to the expected frequency under uniform distribution.
#' An index of 1.0 = no preference; >1.5 = moderate heaping; >2.0 = severe.
#'
#' **Completeness** reports missing values for key STEPS variables grouped
#' by Step (behavioural, physical, biochemical).
#'
#' **Plausibility** counts values outside WHO-recommended ranges
#' (e.g. height 100--250 cm, weight 20--300 kg, SBP 60--300 mmHg).
#'
#' **Weight diagnostics** summarise the distribution of sampling weights
#' and flag potential issues (high CV, zero/NA weights).
#'
#' @export
steps_data_quality <- function(raw, cleaned, cols) {

  message("Running data quality diagnostics...")

  dq <- list(
    digit_preference = .digit_preference(raw, cols),
    completeness     = .completeness(raw, cols),
    plausibility     = .plausibility(raw, cols),
    weights          = .weight_diagnostics(raw, cols)
  )

  class(dq) <- c("steps_quality", "list")
  message("Data quality diagnostics complete.")
  dq
}


# ---------------------------------------------------------------------------
# Digit preference / heaping
# ---------------------------------------------------------------------------

#' @keywords internal
.digit_preference <- function(data, cols) {

  # Physical measurement variables to check
  measure_map <- list(
    SBP    = c("sbp1", "sbp2", "sbp3"),
    DBP    = c("dbp1", "dbp2", "dbp3"),
    Height = "height",
    Weight = "weight",
    Waist  = "waist"
  )

  results <- list()

  for (measure_name in names(measure_map)) {
    col_keys <- measure_map[[measure_name]]
    # Collect all values across the relevant columns
    vals <- numeric(0)
    for (ck in col_keys) {
      if (!is.null(cols[[ck]]) && cols[[ck]] %in% names(data)) {
        v <- as.numeric(data[[cols[[ck]]]])
        vals <- c(vals, v[!is.na(v)])
      }
    }

    if (length(vals) < 30) next

    # Terminal digit frequency
    terminal <- vals %% 10
    digit_counts <- table(factor(terminal, levels = 0:9))
    digit_pct    <- as.numeric(digit_counts) / sum(digit_counts) * 100
    names(digit_pct) <- 0:9

    # Heaping indices
    expected_pct <- 10  # uniform expectation
    hi_0 <- digit_pct[["0"]] / expected_pct
    hi_5 <- digit_pct[["5"]] / expected_pct
    hi_0_5 <- (digit_pct[["0"]] + digit_pct[["5"]]) / (2 * expected_pct)

    # Severity assessment
    severity <- dplyr::case_when(
      hi_0_5 > 2.0 ~ "Severe",
      hi_0_5 > 1.5 ~ "Moderate",
      hi_0_5 > 1.2 ~ "Mild",
      TRUE          ~ "None"
    )

    results[[measure_name]] <- list(
      n             = length(vals),
      digit_counts  = as.numeric(digit_counts),
      digit_pct     = digit_pct,
      heaping_0     = round(hi_0, 2),
      heaping_5     = round(hi_5, 2),
      heaping_0_5   = round(hi_0_5, 2),
      severity      = severity
    )
  }

  message(sprintf("  Digit preference: checked %d measures", length(results)))
  results
}


# ---------------------------------------------------------------------------
# Completeness / missingness
# ---------------------------------------------------------------------------

#' @keywords internal
.completeness <- function(data, cols) {

  # Group variables by STEPS domain
  domain_map <- list(
    "Demographics" = c("age", "sex"),
    "Survey design" = c("strata", "psu", "weight_step1", "weight_step2", "weight_step3"),
    "Tobacco (Step 1)" = c("tobacco_current", "tobacco_daily", "smokeless_current",
                            "secondhand_home", "secondhand_work"),
    "Alcohol (Step 1)" = c("alcohol_current", "heavy_episode"),
    "Diet (Step 1)" = c("fruit_days", "fruit_servings", "veg_days", "veg_servings"),
    "Physical activity (Step 1)" = c("pa_work_vig", "pa_work_days", "pa_transport",
                                      "pa_rec_vig", "pa_rec_days"),
    "Physical measurements (Step 2)" = c("height", "weight", "waist",
                                          "sbp1", "sbp2", "sbp3",
                                          "dbp1", "dbp2", "dbp3"),
    "Biochemical (Step 3)" = c("fasting_glucose", "total_cholesterol")
  )

  results <- list()
  n_total <- nrow(data)

  for (domain in names(domain_map)) {
    var_keys <- domain_map[[domain]]
    domain_results <- data.frame(
      variable   = character(0),
      column     = character(0),
      n_total    = integer(0),
      n_missing  = integer(0),
      pct_missing = numeric(0),
      stringsAsFactors = FALSE
    )

    for (vk in var_keys) {
      col_name <- cols[[vk]]
      if (is.null(col_name) || !col_name %in% names(data)) {
        domain_results <- rbind(domain_results, data.frame(
          variable = vk, column = "(not detected)", n_total = n_total,
          n_missing = n_total, pct_missing = 100.0,
          stringsAsFactors = FALSE
        ))
      } else {
        n_miss <- sum(is.na(data[[col_name]]))
        domain_results <- rbind(domain_results, data.frame(
          variable = vk, column = col_name, n_total = n_total,
          n_missing = n_miss, pct_missing = round(n_miss / n_total * 100, 1),
          stringsAsFactors = FALSE
        ))
      }
    }

    results[[domain]] <- domain_results
  }

  message(sprintf("  Completeness: checked %d domains",
                  length(results)))
  results
}


# ---------------------------------------------------------------------------
# Plausibility checks
# ---------------------------------------------------------------------------

#' @keywords internal
.plausibility <- function(data, cols) {

  # WHO-recommended plausible ranges
  range_checks <- list(
    list(key = "height",  label = "Height (cm)",         lo = 100, hi = 250),
    list(key = "weight",  label = "Weight (kg)",         lo = 20,  hi = 300),
    list(key = "waist",   label = "Waist (cm)",          lo = 40,  hi = 200),
    list(key = "sbp1",    label = "SBP reading 1 (mmHg)",lo = 60,  hi = 300),
    list(key = "sbp2",    label = "SBP reading 2 (mmHg)",lo = 60,  hi = 300),
    list(key = "sbp3",    label = "SBP reading 3 (mmHg)",lo = 60,  hi = 300),
    list(key = "dbp1",    label = "DBP reading 1 (mmHg)",lo = 30,  hi = 200),
    list(key = "dbp2",    label = "DBP reading 2 (mmHg)",lo = 30,  hi = 200),
    list(key = "dbp3",    label = "DBP reading 3 (mmHg)",lo = 30,  hi = 200),
    list(key = "fasting_glucose", label = "Fasting glucose (mmol/L)", lo = 1, hi = 40),
    list(key = "total_cholesterol", label = "Total cholesterol (mmol/L)", lo = 1, hi = 20)
  )

  results <- data.frame(
    variable   = character(0),
    n_valid    = integer(0),
    n_below    = integer(0),
    n_above    = integer(0),
    n_outlier  = integer(0),
    pct_outlier = numeric(0),
    range_low  = numeric(0),
    range_high = numeric(0),
    stringsAsFactors = FALSE
  )

  for (rc in range_checks) {
    col_name <- cols[[rc$key]]
    if (is.null(col_name) || !col_name %in% names(data)) next

    vals <- as.numeric(data[[col_name]])
    valid <- vals[!is.na(vals)]
    if (length(valid) == 0) next

    n_below <- sum(valid < rc$lo)
    n_above <- sum(valid > rc$hi)
    n_out   <- n_below + n_above

    results <- rbind(results, data.frame(
      variable    = rc$label,
      n_valid     = length(valid),
      n_below     = n_below,
      n_above     = n_above,
      n_outlier   = n_out,
      pct_outlier = round(n_out / length(valid) * 100, 2),
      range_low   = rc$lo,
      range_high  = rc$hi,
      stringsAsFactors = FALSE
    ))
  }

  message(sprintf("  Plausibility: checked %d variables", nrow(results)))
  results
}


# ---------------------------------------------------------------------------
# Sampling weight diagnostics
# ---------------------------------------------------------------------------

#' @keywords internal
.weight_diagnostics <- function(data, cols) {

  wt_keys <- c("weight_step1", "weight_step2", "weight_step3")
  results <- list()

  for (wk in wt_keys) {
    col_name <- cols[[wk]]
    if (is.null(col_name) || !col_name %in% names(data)) next

    w <- as.numeric(data[[col_name]])
    n_total <- length(w)
    n_na    <- sum(is.na(w))
    n_zero  <- sum(w == 0, na.rm = TRUE)
    w_valid <- w[!is.na(w) & w > 0]

    if (length(w_valid) == 0) next

    cv <- stats::sd(w_valid) / mean(w_valid)

    # Severity assessment for CV
    cv_severity <- dplyr::case_when(
      cv > 1.5 ~ "High (>1.5) -- extreme weight variability",
      cv > 1.0 ~ "Moderate (>1.0)",
      cv > 0.5 ~ "Acceptable (0.5-1.0)",
      TRUE     ~ "Low (<0.5)"
    )

    results[[wk]] <- list(
      column    = col_name,
      n_total   = n_total,
      n_na      = n_na,
      n_zero    = n_zero,
      n_valid   = length(w_valid),
      min       = round(min(w_valid), 4),
      q1        = round(stats::quantile(w_valid, 0.25), 4),
      median    = round(stats::median(w_valid), 4),
      mean      = round(mean(w_valid), 4),
      q3        = round(stats::quantile(w_valid, 0.75), 4),
      max       = round(max(w_valid), 4),
      sd        = round(stats::sd(w_valid), 4),
      cv        = round(cv, 3),
      cv_severity = cv_severity,
      ratio_max_min = round(max(w_valid) / min(w_valid), 1)
    )
  }

  message(sprintf("  Weight diagnostics: checked %d weight columns", length(results)))
  results
}


# ---------------------------------------------------------------------------
# Plotting helpers
# ---------------------------------------------------------------------------

#' Plot digit preference histogram for a physical measurement
#'
#' Creates a bar chart of terminal-digit frequencies with the expected
#' uniform line at 10 %.
#'
#' @param dq A `steps_quality` object from [steps_data_quality()].
#' @param measure Character: one of "SBP", "DBP", "Height", "Weight", "Waist".
#'
#' @return A ggplot object.
#' @export
plot_digit_preference <- function(dq, measure) {
  if (!measure %in% names(dq$digit_preference)) {
    stop(sprintf("Measure '%s' not found. Available: %s",
                 measure, paste(names(dq$digit_preference), collapse = ", ")))
  }

  dp <- dq$digit_preference[[measure]]

  df <- data.frame(
    digit = factor(0:9),
    pct   = dp$digit_pct
  )

  ggplot2::ggplot(df, ggplot2::aes(x = digit, y = pct)) +
    ggplot2::geom_col(
      fill = dplyr::case_when(
        dp$severity == "Severe"   ~ "#E24B4A",
        dp$severity == "Moderate" ~ "#EF9F27",
        dp$severity == "Mild"     ~ "#639922",
        TRUE                      ~ "#378ADD"
      ),
      alpha = 0.85
    ) +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed",
                        colour = "grey40", linewidth = 0.6) +
    ggplot2::annotate("text", x = 9.5, y = 11, label = "Expected (10%)",
                      size = 3, hjust = 1, colour = "grey40") +
    ggplot2::labs(
      title    = sprintf("%s: terminal digit distribution (n = %s)",
                         measure, format(dp$n, big.mark = ",")),
      subtitle = sprintf("Heaping index (0+5): %.2f -- %s", dp$heaping_0_5, dp$severity),
      x = "Terminal digit",
      y = "Frequency (%)"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11, colour = "grey40"),
      panel.grid.major.x = ggplot2::element_blank()
    )
}


#' Plot completeness heatmap across STEPS domains
#'
#' Creates a tile heatmap showing missingness percentage by variable,
#' grouped by STEPS domain.
#'
#' @param dq A `steps_quality` object from [steps_data_quality()].
#'
#' @return A ggplot object.
#' @export
plot_completeness <- function(dq) {
  # Combine all domains into one data frame
  all_comp <- do.call(rbind, lapply(names(dq$completeness), function(domain) {
    df <- dq$completeness[[domain]]
    df$domain <- domain
    df
  }))

  # Order by domain appearance, then variable within domain
  all_comp$variable <- factor(all_comp$variable,
                               levels = rev(all_comp$variable))

  ggplot2::ggplot(all_comp, ggplot2::aes(x = domain, y = variable, fill = pct_missing)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", pct_missing)),
                       size = 3, colour = ifelse(all_comp$pct_missing > 50, "white", "grey20")) +
    ggplot2::scale_fill_gradient2(
      low = "#1D9E75", mid = "#EF9F27", high = "#E24B4A",
      midpoint = 25, limits = c(0, 100),
      name = "Missing %"
    ) +
    ggplot2::labs(
      title = "Variable completeness by STEPS domain",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 10),
      plot.title   = ggplot2::element_text(face = "bold", size = 14),
      panel.grid   = ggplot2::element_blank()
    )
}


#' Plot sampling weight distribution
#'
#' Creates a histogram of sampling weights with summary statistics.
#'
#' @param dq A `steps_quality` object from [steps_data_quality()].
#' @param step Character: which weight to plot ("weight_step1", "weight_step2",
#'   or "weight_step3"). Defaults to "weight_step1".
#'
#' @return A ggplot object.
#' @export
plot_weights <- function(dq, step = "weight_step1") {
  if (!step %in% names(dq$weights)) {
    stop(sprintf("Weight '%s' not found. Available: %s",
                 step, paste(names(dq$weights), collapse = ", ")))
  }

  wt_info <- dq$weights[[step]]

  # We need the original data to plot the histogram, but we only have

  # summary stats. Return a stat summary plot instead.
  df <- data.frame(
    stat = c("Min", "Q1", "Median", "Mean", "Q3", "Max"),
    value = c(wt_info$min, wt_info$q1, wt_info$median,
              wt_info$mean, wt_info$q3, wt_info$max)
  )
  df$stat <- factor(df$stat, levels = df$stat)

  subtitle <- sprintf(
    "n = %s | NA: %d | Zero: %d | CV: %.3f (%s) | Max/Min: %.1f",
    format(wt_info$n_valid, big.mark = ","),
    wt_info$n_na, wt_info$n_zero,
    wt_info$cv, wt_info$cv_severity,
    wt_info$ratio_max_min
  )

  ggplot2::ggplot(df, ggplot2::aes(x = stat, y = value)) +
    ggplot2::geom_col(fill = "#378ADD", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                       vjust = -0.5, size = 3.5) +
    ggplot2::labs(
      title    = sprintf("Sampling weight distribution: %s", wt_info$column),
      subtitle = subtitle,
      x = NULL, y = "Weight value"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, colour = "grey40"),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

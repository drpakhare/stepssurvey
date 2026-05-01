#' Generic Table Builder for WHO STEPS Data Book
#'
#' Takes computed results from [compute_table()] or [compute_all_tables()]
#' and produces formatted flextable objects in the standard WHO STEPS
#' 3-panel format (Men / Women / Both Sexes).
#'
#' @name table_builder
#' @family tables
NULL


#' Build a formatted table from a computed result
#'
#' Dispatches to the appropriate formatting method based on table type.
#'
#' @param result A result list from [compute_table()].
#' @return A flextable object, or NULL if the table is not available.
#'
#' @export
build_table <- function(result) {
  if (!result$available) return(NULL)

  tryCatch(
    switch(result$type,
      "proportion" = .build_proportion_table(result),
      "mean"       = .build_mean_table(result),
      "category"   = .build_category_table(result),
      "cascade"    = .build_cascade_table(result),
      "combined"   = .build_cascade_table(result),   # same format
      {
        message(glue::glue("Unknown table type: {result$type}"))
        NULL
      }
    ),
    error = function(e) {
      message(glue::glue("  \u26a0 Could not build table {result$id}: {e$message}"))
      NULL
    }
  )
}


#' Build all tables from computed results
#'
#' @param results A named list of results from [compute_all_tables()].
#' @return A named list of flextable objects. NULL entries excluded.
#'
#' @export
build_all_tables <- function(results) {
  tables <- list()
  for (id in names(results)) {
    tbl <- build_table(results[[id]])
    if (!is.null(tbl)) tables[[id]] <- tbl
  }
  n <- length(tables)
  message(glue::glue("\u2713 Built {n} formatted tables."))
  tables
}


# ============================================================================
# WHO STEPS standard table format
# ============================================================================
# Every table follows this layout:
#
#  [Title]
#  [Description]
#
#  Age Group | n | Men % (95% CI) | n | Women % (95% CI) | n | Both % (95% CI)
#  ----------|---|----------------|---|------------------|---|----------------
#  18-29     |   |                |   |                  |   |
#  30-44     |   |                |   |                  |   |
#  45-59     |   |                |   |                  |   |
#  60-69     |   |                |   |                  |   |
#  18-69     |   |                |   |                  |   |
# ============================================================================

#' Format a single estimate with CI
#' @param est Numeric estimate.
#' @param lower Lower CI bound.
#' @param upper Upper CI bound.
#' @param digits Decimal places.
#' @param is_pct If TRUE, values are already in percent; add "%" suffix.
#' @return Formatted string.
#' @keywords internal
.fmt_ci <- function(est, lower, upper, digits = 1, is_pct = TRUE) {
  suffix <- if (is_pct) "%" else ""
  mapply(function(e, l, u) {
    if (is.na(e)) return("-")
    paste0(round(e, digits), suffix,
           " (", round(l, digits), "–", round(u, digits), ")")
  }, est, lower, upper, USE.NAMES = FALSE)
}


#' Merge by_sex and by_age results into a single wide data frame
#'
#' Produces the standard 3-panel layout: Age Group | Men | Women | Both Sexes.
#'
#' @param total_df Total estimate (1-row data frame).
#' @param by_sex_df By-sex estimates (with `sex` column).
#' @param by_age_df By-age estimates (with `age_group` column).
#' @param is_pct Whether values are percentages.
#'
#' @return A data frame with columns: Age Group, Men, Women, Both Sexes.
#' @keywords internal
.merge_panels <- function(total_df, by_sex_df, by_age_df, is_pct = TRUE,
                          by_sex_age_df = NULL) {

  # Helper: identify male / female rows from a sex column
  .is_male   <- function(x) x %in% c("1", "Male", "male", "M", "m")
  .is_female <- function(x) x %in% c("2", "Female", "female", "F", "f")

  # --- By-age panel (Both Sexes) ---
  age_both <- by_age_df |>
    dplyr::mutate(both = .fmt_ci(estimate, lower, upper, is_pct = is_pct)) |>
    dplyr::select(age_group, both)

  # --- By-sex × age panel (Men / Women columns per age row) ---
  if (!is.null(by_sex_age_df) && nrow(by_sex_age_df) > 0) {
    sex_col <- by_sex_age_df$sex
    if (is.factor(sex_col)) sex_col <- as.character(sex_col)

    men_age <- by_sex_age_df[.is_male(sex_col), , drop = FALSE] |>
      dplyr::mutate(men = .fmt_ci(estimate, lower, upper, is_pct = is_pct)) |>
      dplyr::select(age_group, men)

    women_age <- by_sex_age_df[.is_female(sex_col), , drop = FALSE] |>
      dplyr::mutate(women = .fmt_ci(estimate, lower, upper, is_pct = is_pct)) |>
      dplyr::select(age_group, women)

    age_rows <- age_both |>
      dplyr::left_join(men_age, by = "age_group") |>
      dplyr::left_join(women_age, by = "age_group") |>
      dplyr::mutate(
        men   = dplyr::if_else(is.na(men), "-", men),
        women = dplyr::if_else(is.na(women), "-", women)
      ) |>
      dplyr::select(age_group, men, women, both)
  } else {
    # Fallback: no sex × age data
    age_rows <- age_both |>
      dplyr::mutate(men = "-", women = "-") |>
      dplyr::select(age_group, men, women, both)
  }

  # --- Total row (Men / Women from by_sex, Both from total) ---
  men_est   <- "-"
  women_est <- "-"
  if (!is.null(by_sex_df) && nrow(by_sex_df) >= 2) {
    sex_col <- by_sex_df$sex
    if (is.factor(sex_col)) sex_col <- as.character(sex_col)
    male_row   <- which(.is_male(sex_col))
    female_row <- which(.is_female(sex_col))
    if (length(male_row) > 0) {
      r <- by_sex_df[male_row[1], ]
      men_est <- .fmt_ci(r$estimate, r$lower, r$upper, is_pct = is_pct)
    }
    if (length(female_row) > 0) {
      r <- by_sex_df[female_row[1], ]
      women_est <- .fmt_ci(r$estimate, r$lower, r$upper, is_pct = is_pct)
    }
  }

  total_both <- .fmt_ci(total_df$estimate[1], total_df$lower[1], total_df$upper[1],
                         is_pct = is_pct)

  total_row <- data.frame(
    age_group = "18-69 (total)",
    men       = men_est,
    women     = women_est,
    both      = total_both,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(age_rows, total_row)
}


#' Apply WHO STEPS styling to a flextable
#' @param ft A flextable object.
#' @param title Table title.
#' @param description Optional description text.
#' @return Styled flextable.
#' @keywords internal
.style_steps_table <- function(ft, title, description = NULL) {
  ft <- ft |>
    flextable::set_caption(title) |>
    flextable::theme_vanilla() |>
    flextable::bold(part = "header") |>
    flextable::bg(part = "header", bg = "#00427A") |>
    flextable::color(part = "header", color = "white") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::autofit()

  if (!is.null(description)) {
    ft <- flextable::add_footer_lines(ft, values = description)
    ft <- flextable::italic(ft, part = "footer")
    ft <- flextable::fontsize(ft, size = 8, part = "footer")
  }

  ft
}


# ============================================================================
# Type-specific table builders
# ============================================================================

#' Build a proportion table
#' @keywords internal
.build_proportion_table <- function(result) {
  res <- result$results

  wide <- .merge_panels(
    total_df       = res$total,
    by_sex_df      = res$by_sex,
    by_age_df      = res$by_age,
    is_pct         = TRUE,
    by_sex_age_df  = res$by_sex_age
  )

  ft <- wide |>
    dplyr::rename(
      `Age Group`  = age_group,
      `Men`        = men,
      `Women`      = women,
      `Both Sexes` = both
    ) |>
    flextable::flextable()

  .style_steps_table(ft, result$title, result$results$description)
}


#' Build a mean table
#' @keywords internal
.build_mean_table <- function(result) {
  res <- result$results

  # Single variable
  if ("total" %in% names(res)) {
    unit_label <- if (length(result$unit) == 1) result$unit else ""
    title <- paste0(result$title, if (unit_label != "") paste0(" (", unit_label, ")") else "")

    wide <- .merge_panels(
      total_df       = res$total,
      by_sex_df      = res$by_sex,
      by_age_df      = res$by_age,
      is_pct         = FALSE,
      by_sex_age_df  = res$by_sex_age
    )

    ft <- wide |>
      dplyr::rename(
        `Age Group`  = age_group,
        `Men`        = men,
        `Women`      = women,
        `Both Sexes` = both
      ) |>
      flextable::flextable()

    return(.style_steps_table(ft, title))
  }

  # Multiple variables: build a stacked table
  units <- result$unit
  if (length(units) == 1) units <- rep(units, length(res))

  all_wide <- list()
  var_names <- names(res)
  for (i in seq_along(var_names)) {
    v <- var_names[i]
    sub <- res[[v]]
    u <- if (i <= length(units)) units[i] else ""
    subtitle <- paste0(gsub("_", " ", v), if (u != "") paste0(" (", u, ")") else "")

    wide <- .merge_panels(sub$total, sub$by_sex, sub$by_age, is_pct = FALSE,
                           by_sex_age_df = sub$by_sex_age)
    # Add a header row for this sub-variable
    header_row <- data.frame(
      age_group = subtitle, men = "", women = "", both = "",
      stringsAsFactors = FALSE
    )
    all_wide[[v]] <- dplyr::bind_rows(header_row, wide)
  }

  stacked <- dplyr::bind_rows(all_wide)
  ft <- stacked |>
    dplyr::rename(
      `Age Group`  = age_group,
      `Men`        = men,
      `Women`      = women,
      `Both Sexes` = both
    ) |>
    flextable::flextable()

  .style_steps_table(ft, result$title)
}


#' Build a category table
#' @keywords internal
.build_category_table <- function(result) {
  res <- result$results
  # res is a named list: level_label -> {total, by_sex, by_age}

  all_panels <- list()
  for (lbl in names(res)) {
    sub <- res[[lbl]]
    wide <- .merge_panels(sub$total, sub$by_sex, sub$by_age, is_pct = TRUE,
                           by_sex_age_df = sub$by_sex_age)
    # Add category header row
    header_row <- data.frame(
      age_group = paste0("  ", lbl), men = "", women = "", both = "",
      stringsAsFactors = FALSE
    )
    all_panels[[lbl]] <- dplyr::bind_rows(header_row, wide)
  }

  stacked <- dplyr::bind_rows(all_panels)
  ft <- stacked |>
    dplyr::rename(
      `Age Group`  = age_group,
      `Men`        = men,
      `Women`      = women,
      `Both Sexes` = both
    ) |>
    flextable::flextable()

  .style_steps_table(ft, result$title)
}


#' Build a cascade table (also used for combined risk factors)
#' @keywords internal
.build_cascade_table <- function(result) {
  res <- result$results
  # Same structure as category: named list of {total, by_sex, by_age}

  all_rows <- list()
  for (lbl in names(res)) {
    sub <- res[[lbl]]
    wide <- .merge_panels(sub$total, sub$by_sex, sub$by_age, is_pct = TRUE,
                           by_sex_age_df = sub$by_sex_age)
    # Add cascade step header
    header_row <- data.frame(
      age_group = lbl, men = "", women = "", both = "",
      stringsAsFactors = FALSE
    )
    all_rows[[lbl]] <- dplyr::bind_rows(header_row, wide)
  }

  stacked <- dplyr::bind_rows(all_rows)
  ft <- stacked |>
    dplyr::rename(
      `Age Group`  = age_group,
      `Men`        = men,
      `Women`      = women,
      `Both Sexes` = both
    ) |>
    flextable::flextable()

  .style_steps_table(ft, result$title)
}

#' Generic Compute Engine for WHO STEPS Tables
#'
#' Takes a table specification from [steps_table_registry()] and a survey
#' design object, and produces the survey-weighted estimates needed to fill
#' the standard WHO STEPS data book table.
#'
#' @name compute_engine
#' @family compute
NULL


#' Compute a single table from a registry entry
#'
#' This is the main workhorse: given one registry entry and a survey design,
#' it dispatches to the appropriate method based on `entry$type` and returns
#' a standardised result list.
#'
#' @param entry A single list element from [steps_table_registry()].
#' @param design A survey design object (from [survey::svydesign()]).
#' @param data The cleaned data frame (used for variable availability checks).
#'
#' @return A list with:
#'   \describe{
#'     \item{id}{Table identifier.}
#'     \item{title}{Table title.}
#'     \item{type}{Table type.}
#'     \item{available}{Logical: TRUE if the required variable(s) exist.}
#'     \item{results}{A list of data frames:
#'       For proportion: total, by_sex, by_age (each with estimate, lower, upper).
#'       For mean: total, by_sex, by_age (each with estimate, lower, upper).
#'       For category: total, by_sex, by_age (each with level, estimate, lower, upper).
#'       For cascade: named list of proportion results.
#'     }
#'   }
#'
#' @export
compute_table <- function(entry, design, data = NULL) {
  if (is.null(data)) data <- design$variables

  result <- list(
    id        = entry$id,
    title     = entry$title,
    section   = entry$section,
    type      = entry$type,
    step      = entry$step,
    unit      = entry$unit,
    available = FALSE,
    results   = NULL
  )

  # Dispatch by type
  res <- tryCatch(
    switch(entry$type,
      "proportion" = .compute_proportion(entry, design, data),
      "mean"       = .compute_mean(entry, design, data),
      "category"   = .compute_category(entry, design, data),
      "cascade"    = .compute_cascade(entry, design, data),
      "combined"   = .compute_combined(entry, design, data),
      stop("Unknown table type: ", entry$type)
    ),
    error = function(e) {
      message(glue::glue("  \u26a0 {entry$id}: {e$message}"))
      NULL
    }
  )

  if (!is.null(res)) {
    result$available <- TRUE
    result$results   <- res
  }

  result
}


#' Compute all tables from the registry
#'
#' Iterates through the full [steps_table_registry()] and computes every
#' table that has available data. Returns a named list of results.
#'
#' @param designs A list of survey designs, with elements `step1`, `step2`,
#'   `step3` (as returned by [setup_survey_design()]).
#' @param data The cleaned data frame.
#'
#' @return A named list of table results (from [compute_table()]).
#'   Only entries with `available == TRUE` are included.
#'
#' @export
compute_all_tables <- function(designs, data = NULL) {
  registry <- steps_table_registry()
  results  <- list()

  n <- length(registry)
  message(glue::glue("Computing {n} tables from WHO STEPS registry..."))

  for (i in seq_along(registry)) {
    entry <- registry[[i]]

    # Select the appropriate design for this step
    design <- switch(as.character(entry$step),
      "1" = designs$step1,
      "2" = designs$step2,
      "3" = designs$step3,
      designs$step1  # fallback
    )

    if (is.null(design)) {
      message(glue::glue("  \u26a0 {entry$id}: No Step {entry$step} design available, skipping."))
      next
    }

    res <- compute_table(entry, design, data)
    results[[entry$id]] <- res

    status <- if (res$available) "\u2713" else "\u26a0 (no data)"
    message(glue::glue("  [{i}/{n}] {status} {entry$id}: {entry$title}"))
  }

  # Summary
  n_ok <- sum(vapply(results, function(x) x$available, logical(1)))
  message(glue::glue("\u2713 Computed {n_ok}/{n} tables ({n - n_ok} skipped due to missing data)."))

  results
}


# ============================================================================
# Internal compute methods
# ============================================================================

#' Check if variable(s) exist in data
#' @keywords internal
.vars_available <- function(vars, data) {
  all(vars %in% names(data))
}


#' Optionally subset a design by a denominator variable
#' @keywords internal
.apply_denominator <- function(design, denominator, data) {
  if (is.null(denominator)) return(design)
  if (!denominator %in% names(data)) return(NULL)
  subset(design, data[[denominator]] == TRUE)
}


#' Compute a proportion table (type = "proportion")
#'
#' Produces total, by_sex, by_age data frames with estimate, lower, upper.
#' Values are in percentage (0-100).
#'
#' @keywords internal
.compute_proportion <- function(entry, design, data) {
  var <- entry$variable
  if (length(var) > 1) var <- var[1]  # single variable for proportion
  if (!.vars_available(var, data)) return(NULL)

  # Apply denominator filter if specified
  dsn <- .apply_denominator(design, entry$denominator, data)
  if (is.null(dsn)) return(NULL)

  f <- stats::as.formula(paste0("~as.numeric(", var, ")"))

  list(
    total      = svyprop(f, dsn),
    by_sex     = svyprop(f, dsn, by = ~sex),
    by_age     = svyprop(f, dsn, by = ~age_group),
    by_sex_age = svyprop(f, dsn, by = ~sex + age_group)
  )
}


#' Compute a mean table (type = "mean")
#'
#' For single variable: total, by_sex, by_age.
#' For multiple variables: a named list, each with total, by_sex, by_age.
#'
#' @keywords internal
.compute_mean <- function(entry, design, data) {
  vars <- entry$variable
  dsn  <- .apply_denominator(design, entry$denominator, data)
  if (is.null(dsn)) return(NULL)

  # Filter to variables that actually exist
  vars <- vars[vars %in% names(data)]
  if (length(vars) == 0) return(NULL)

  if (length(vars) == 1) {
    f <- stats::as.formula(paste0("~", vars))
    return(list(
      total      = svymn(f, dsn),
      by_sex     = svymn(f, dsn, by = ~sex),
      by_age     = svymn(f, dsn, by = ~age_group),
      by_sex_age = svymn(f, dsn, by = ~sex + age_group)
    ))
  }

  # Multiple variables: return named list
  res <- list()
  for (v in vars) {
    f <- stats::as.formula(paste0("~", v))
    res[[v]] <- list(
      total      = svymn(f, dsn),
      by_sex     = svymn(f, dsn, by = ~sex),
      by_age     = svymn(f, dsn, by = ~age_group),
      by_sex_age = svymn(f, dsn, by = ~sex + age_group)
    )
  }
  res
}


#' Compute a category table (type = "category")
#'
#' The variable is a factor; compute the proportion in each level.
#' Returns a named list (one element per level), each with total, by_sex, by_age.
#'
#' @keywords internal
.compute_category <- function(entry, design, data) {
  var <- entry$variable
  if (length(var) > 1) var <- var[1]
  if (!.vars_available(var, data)) return(NULL)

  dsn <- .apply_denominator(design, entry$denominator, data)
  if (is.null(dsn)) return(NULL)

  # If levels are specified as named vector mapping label -> binary variable
  if (!is.null(entry$levels)) {
    res <- list()
    for (lbl in names(entry$levels)) {
      bvar <- entry$levels[[lbl]]
      if (bvar %in% names(data)) {
        f <- stats::as.formula(paste0("~as.numeric(", bvar, ")"))
        res[[lbl]] <- list(
          total      = svyprop(f, dsn),
          by_sex     = svyprop(f, dsn, by = ~sex),
          by_age     = svyprop(f, dsn, by = ~age_group),
          by_sex_age = svyprop(f, dsn, by = ~sex + age_group)
        )
      }
    }
    if (length(res) == 0) return(NULL)
    return(res)
  }

  # Otherwise, treat the variable as a factor and compute proportions per level
  if (!is.factor(data[[var]])) {
    data[[var]] <- as.factor(data[[var]])
    dsn$variables[[var]] <- as.factor(dsn$variables[[var]])
  }
  lvls <- levels(data[[var]])

  res <- list()
  for (lv in lvls) {
    bvar_name <- paste0(".cat_", var, "_", make.names(lv))
    dsn$variables[[bvar_name]] <- as.numeric(dsn$variables[[var]] == lv)
    f <- stats::as.formula(paste0("~", bvar_name))
    res[[lv]] <- list(
      total      = svyprop(f, dsn),
      by_sex     = svyprop(f, dsn, by = ~sex),
      by_age     = svyprop(f, dsn, by = ~age_group),
      by_sex_age = svyprop(f, dsn, by = ~sex + age_group)
    )
  }
  if (length(res) == 0) return(NULL)
  res
}


#' Compute a cascade table (type = "cascade")
#'
#' A cascade is a series of proportions, each possibly with a different
#' denominator (nested). Returns named list, each with total, by_sex, by_age.
#'
#' @keywords internal
.compute_cascade <- function(entry, design, data) {
  items <- entry$variable  # named list: label -> variable_name
  if (!is.list(items)) return(NULL)

  dsn <- .apply_denominator(design, entry$denominator, data)
  if (is.null(dsn)) return(NULL)

  res <- list()
  for (lbl in names(items)) {
    var <- items[[lbl]]
    if (var %in% names(data)) {
      f <- stats::as.formula(paste0("~as.numeric(", var, ")"))
      res[[lbl]] <- list(
        total      = svyprop(f, dsn),
        by_sex     = svyprop(f, dsn, by = ~sex),
        by_age     = svyprop(f, dsn, by = ~age_group),
        by_sex_age = svyprop(f, dsn, by = ~sex + age_group)
      )
    }
  }
  if (length(res) == 0) return(NULL)
  res
}


#' Compute combined risk factors (type = "combined")
#'
#' Counts how many of 5 key risk factors each respondent has, then computes
#' the proportion with 0, 1-2, and 3-5 risk factors.
#'
#' @keywords internal
.compute_combined <- function(entry, design, data) {
  items <- entry$variable  # named list of risk factor variables
  if (!is.list(items)) return(NULL)

  vars <- unlist(items)
  avail <- vars[vars %in% names(data)]
  if (length(avail) < 3) return(NULL)  # need at least 3 of 5 to be meaningful

  # Count risk factors per person
  rf_matrix <- as.data.frame(lapply(avail, function(v) {
    x <- as.numeric(data[[v]])
    dplyr::if_else(is.na(x), NA_real_, x)
  }))
  design$variables$.rf_count <- rowSums(rf_matrix, na.rm = FALSE)

  # Create category variables
  design$variables$.rf_0   <- as.numeric(design$variables$.rf_count == 0)
  design$variables$.rf_1_2 <- as.numeric(design$variables$.rf_count >= 1 & design$variables$.rf_count <= 2)
  design$variables$.rf_3_5 <- as.numeric(design$variables$.rf_count >= 3)

  list(
    "0 risk factors" = list(
      total      = svyprop(~.rf_0, design),
      by_sex     = svyprop(~.rf_0, design, by = ~sex),
      by_age     = svyprop(~.rf_0, design, by = ~age_group),
      by_sex_age = svyprop(~.rf_0, design, by = ~sex + age_group)
    ),
    "1-2 risk factors" = list(
      total      = svyprop(~.rf_1_2, design),
      by_sex     = svyprop(~.rf_1_2, design, by = ~sex),
      by_age     = svyprop(~.rf_1_2, design, by = ~age_group),
      by_sex_age = svyprop(~.rf_1_2, design, by = ~sex + age_group)
    ),
    "3-5 risk factors" = list(
      total      = svyprop(~.rf_3_5, design),
      by_sex     = svyprop(~.rf_3_5, design, by = ~sex),
      by_age     = svyprop(~.rf_3_5, design, by = ~age_group),
      by_sex_age = svyprop(~.rf_3_5, design, by = ~sex + age_group)
    )
  )
}

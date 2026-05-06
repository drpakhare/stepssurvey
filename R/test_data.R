#' Generate simulated STEPS test data
#'
#' Creates a realistic simulated dataset matching WHO STEPS survey structure.
#' Includes sampling design variables, demographics, and measures from all
#' three steps (behavioural, physical, biochemical).
#'
#' @param n Number of observations (default 3000).
#' @param seed Random seed for reproducibility (default 42).
#'
#' @return A data frame with `n` rows and the following columns:
#'   - `stratum`: Strata identifier (S1-S5)
#'   - `psu`: Primary sampling unit (PSU1-PSU40)
#'   - `wt_final`: Final analysis weight
#'   - `sex`: Sex (1=Male, 2=Female)
#'   - `age`: Age in years (18-69)
#'   - Step 1 (behavioural): `t1`, `t2` (tobacco), `a1`, `a5` (alcohol),
#'     `met_total` (physical activity), `d1`-`d4` (diet)
#'   - Step 2 (physical): `m1` (height), `m2` (weight), `m3` (waist),
#'     `b1`-`b6` (blood pressure), `b7` (BP medication)
#'   - Step 3 (biochemical): `c1_mmol` (glucose), `c5` (DM meds),
#'     `c6` (cholesterol), `c10` (cholesterol meds)
#'
#' @details
#' Simulation parameters are realistic for low-middle income settings:
#'   - Tobacco prevalence: 32% males, 8% females
#'   - Alcohol current use: 55% males, 28% females
#'   - Heavy episodic drinking: 35% of drinkers
#'   - Physical activity: MET-minutes/week, mean 1800, SD 1200
#'   - Diet: Fruit/veg days and servings per day (0-7, 1-5)
#'   - BP increases with age; medication prevalence 12%
#'   - Glucose: mean 5.2 mmol/L, increases with age
#'   - Total cholesterol: mean 4.8 mmol/L
#'
#' Use this function for:
#'   - Testing the STEPS pipeline
#'   - Developing reports before real data arrives
#'   - Training analysts on the analysis system
#'
#' @examples
#' \donttest{
#'   # Generate smaller dataset for quick testing
#'   test_data <- generate_test_data(n = 500, seed = 123)
#'   head(test_data)
#' }
#'
#' @export
generate_test_data <- function(n = 3000, seed = 42) {
  set.seed(seed)

  # - Sampling design
  strata  <- sample(paste0("S", 1:5), n, replace = TRUE)
  psu     <- sample(paste0("PSU", 1:40), n, replace = TRUE)
  wt      <- runif(n, 0.5, 2.5)

  # - Demographics
  sex <- sample(c(1, 2), n, replace = TRUE, prob = c(0.48, 0.52))
  age <- round(runif(n, 18, 69))

  age_grp <- cut(age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                 labels = c("18-24","25-34","35-44","45-54","55-64","65+"),
                 right = FALSE)

  # - Step 1: Behaviour
  tobacco_current <- rbinom(n, 1, ifelse(sex == 1, 0.32, 0.08))
  tobacco_daily   <- tobacco_current * rbinom(n, 1, 0.75)
  alcohol_current <- rbinom(n, 1, ifelse(sex == 1, 0.55, 0.28))
  heavy_episodic  <- alcohol_current * rbinom(n, 1, 0.35)

  # Physical activity (MET-minutes/week)
  met_total <- round(abs(rnorm(n, mean = 1800, sd = 1200)))

  # Diet
  fruit_days <- sample(0:7, n, replace = TRUE)
  fruit_serv <- sample(1:5, n, replace = TRUE)
  veg_days   <- sample(0:7, n, replace = TRUE)
  veg_serv   <- sample(1:5, n, replace = TRUE)

  # - Step 2: Physical measurements
  height <- round(rnorm(n, ifelse(sex == 1, 170, 158), 8), 1)
  weight <- round(rnorm(n, ifelse(sex == 1, 78, 68), 15), 1)
  waist  <- round(rnorm(n, ifelse(sex == 1, 90, 84), 14), 1)

  # Blood pressure - increases with age
  age_effect <- (age - 18) / 51
  sbp1 <- round(rnorm(n, 115 + 30 * age_effect, 18))
  sbp2 <- sbp1 + round(rnorm(n, -2, 4))
  sbp3 <- sbp1 + round(rnorm(n, -3, 4))
  dbp1 <- round(rnorm(n, 72 + 15 * age_effect, 12))
  dbp2 <- dbp1 + round(rnorm(n, -1, 3))
  dbp3 <- dbp1 + round(rnorm(n, -2, 3))
  bp_meds <- rbinom(n, 1, 0.12)

  # - Step 3: Biochemical
  fasting_glucose <- round(abs(rnorm(n, 5.2 + 0.8 * age_effect, 1.4)), 1)
  dm_meds         <- rbinom(n, 1, 0.06)
  total_chol      <- round(abs(rnorm(n, 4.8, 1.1)), 1)
  chol_meds       <- rbinom(n, 1, 0.04)

  # - Assemble data frame
  test_data <- data.frame(
    # Sampling
    stratum = strata, psu = psu, wt_final = wt,
    # Demographics
    sex = sex, age = age,
    # Step 1
    t1 = tobacco_current, t2 = tobacco_daily,
    a1 = alcohol_current, a5 = heavy_episodic,
    met_total = met_total,
    d1 = fruit_days, d2 = fruit_serv, d3 = veg_days, d4 = veg_serv,
    # Step 2
    m1 = height, m2 = weight, m3 = waist,
    b1 = sbp1, b2 = dbp1, b3 = sbp2, b4 = dbp2, b5 = sbp3, b6 = dbp3,
    b7 = bp_meds,
    # Step 3
    c1_mmol = fasting_glucose, c5 = dm_meds,
    c6 = total_chol, c10 = chol_meds
  )

  message(glue::glue("\u2713 Generated test data: {nrow(test_data)} rows \u00d7 {ncol(test_data)} columns"))

  return(test_data)
}

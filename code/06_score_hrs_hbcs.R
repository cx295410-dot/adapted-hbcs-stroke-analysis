#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data/derived")
results_dir <- file.path(root, "results")

person_file <- file.path(
  derived_dir, "hrs_person_wave_raw.rds"
)
transition_file <- file.path(
  derived_dir, "hrs_transition_intervals_raw.rds"
)

stopifnot(file.exists(person_file), file.exists(transition_file))

log_file <- file.path(results_dir, "06_score_hrs_hbcs.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

person_wave <- readRDS(person_file)
transitions <- readRDS(transition_file)

binary01 <- function(x) {
  ifelse(x %in% c(0, 1), x, NA_real_)
}

score_hbcs <- function(d) {

  # 1. Smoking
  d$score_smoking <- case_when(
    d$smoking_ever == 0 ~ 2,
    d$smoking_ever == 1 & d$smoking_current == 0 ~ 1,
    d$smoking_current == 1 ~ 0,
    TRUE ~ NA_real_
  )

  # 2. Alcohol: drinks per week
  d$alcohol_weekly <- case_when(
    d$alcohol_any == 0 ~ 0,
    d$alcohol_any == 1 &
      !is.na(d$alcohol_days) &
      !is.na(d$alcohol_amount) ~
        d$alcohol_days * d$alcohol_amount,
    TRUE ~ NA_real_
  )

  alcohol_limit <- case_when(
    d$sex == 1 ~ 14,  # men
    d$sex == 2 ~ 7,   # women
    TRUE ~ NA_real_
  )

  d$score_alcohol <- case_when(
    !is.na(d$alcohol_weekly) &
      d$alcohol_weekly <= alcohol_limit ~ 2,
    !is.na(d$alcohol_weekly) &
      d$alcohol_weekly <= 2 * alcohol_limit ~ 1,
    !is.na(d$alcohol_weekly) ~ 0,
    TRUE ~ NA_real_
  )

  # 3. Physical activity
  d$score_activity <- case_when(
    d$activity_vigorous %in% c(1, 2) |
      d$activity_moderate %in% c(1, 2) ~ 2,

    d$activity_vigorous %in% c(3, 4) |
      d$activity_moderate %in% c(3, 4) |
      d$activity_light %in% c(1, 2, 3) ~ 1,

    d$activity_vigorous == 5 &
      d$activity_moderate == 5 &
      d$activity_light %in% c(4, 5) ~ 0,

    TRUE ~ NA_real_
  )

  # 4. BMI
  d$score_bmi <- case_when(
    d$bmi >= 18.5 & d$bmi < 25 ~ 2,
    d$bmi >= 25 & d$bmi < 30 ~ 1,
    !is.na(d$bmi) ~ 0,
    TRUE ~ NA_real_
  )

  # 5–6. Diagnosed hypertension and diabetes
  d$score_hypertension <- case_when(
    d$hypertension_ever == 0 ~ 2,
    d$hypertension_ever == 1 ~ 0,
    TRUE ~ NA_real_
  )

  d$score_diabetes <- case_when(
    d$diabetes_ever == 0 ~ 2,
    d$diabetes_ever == 1 ~ 0,
    TRUE ~ NA_real_
  )

  # 7. Restless sleep
  d$score_sleep <- case_when(
    d$sleep_restless == 0 ~ 2,
    d$sleep_restless == 1 ~ 0,
    TRUE ~ NA_real_
  )

  # 8. Mood: six non-overlapping CESD items
  mood_matrix <- cbind(
    binary01(d$mood_depressed),
    binary01(d$mood_effort),
    binary01(d$mood_sad),
    binary01(d$mood_could_not_get_going),
    1 - binary01(d$mood_happy),
    1 - binary01(d$mood_enjoyed_life)
  )

  mood_observed <- rowSums(!is.na(mood_matrix))
  mood_symptoms <- rowSums(mood_matrix, na.rm = TRUE)

  d$mood_symptom_proportion <- ifelse(
    mood_observed >= 5,
    mood_symptoms / mood_observed,
    NA_real_
  )

  d$score_mood <- case_when(
    d$mood_symptom_proportion == 0 ~ 2,
    d$mood_symptom_proportion < 0.5 ~ 1,
    d$mood_symptom_proportion >= 0.5 ~ 0,
    TRUE ~ NA_real_
  )

  # 9. Social connection:
  # RAND marital-status codes 1–3 indicate married/partnered.
  d$score_social <- case_when(
    d$marital_status %in% c(1, 2, 3) ~ 2,
    d$marital_status %in% c(4, 5, 6, 7, 8) ~ 0,
    TRUE ~ NA_real_
  )

  score_names <- c(
    "score_smoking", "score_alcohol", "score_activity",
    "score_bmi", "score_hypertension", "score_diabetes",
    "score_sleep", "score_mood", "score_social"
  )

  score_matrix <- as.matrix(d[, score_names])

  d$hbcs_domains_observed <- rowSums(!is.na(score_matrix))
  d$hbcs_complete <- d$hbcs_domains_observed == 9

  d$adapted_hbcs_18 <- ifelse(
    d$hbcs_complete,
    rowSums(score_matrix),
    NA_real_
  )

  # Sensitivity-only score scaled to the same 0–18 range.
  d$adapted_hbcs_18_available <- ifelse(
    d$hbcs_domains_observed >= 7,
    rowSums(score_matrix, na.rm = TRUE) *
      9 / d$hbcs_domains_observed,
    NA_real_
  )

  d
}

person_scored <- score_hbcs(person_wave)
transition_scored <- score_hbcs(transitions)

stopifnot(
  all(
    transition_scored$adapted_hbcs_18[
      !is.na(transition_scored$adapted_hbcs_18)
    ] >= 0
  ),
  all(
    transition_scored$adapted_hbcs_18[
      !is.na(transition_scored$adapted_hbcs_18)
    ] <= 18
  )
)

eligible_50plus <- transition_scored |>
  filter(!is.na(age), age >= 50)

analytic_multistate <- eligible_50plus |>
  filter(hbcs_complete)

analytic_stroke_free <- analytic_multistate |>
  filter(state == 0)

score_names <- c(
  "score_smoking", "score_alcohol", "score_activity",
  "score_bmi", "score_hypertension", "score_diabetes",
  "score_sleep", "score_mood", "score_social"
)

component_missing <- map_dfr(score_names, function(v) {
  x <- eligible_50plus[[v]]

  tibble(
    component = v,
    eligible_intervals = length(x),
    observed = sum(!is.na(x)),
    missing_pct = round(100 * mean(is.na(x)), 2),
    score_0 = sum(x == 0, na.rm = TRUE),
    score_1 = sum(x == 1, na.rm = TRUE),
    score_2 = sum(x == 2, na.rm = TRUE)
  )
})

analytic_flow <- tibble(
  step = c(
    "Valid transitions with baseline age 50+",
    "Age 50+ transitions with complete nine-domain hBCS",
    "Stroke-free complete-score intervals",
    "Incident stroke events in stroke-free analytic intervals",
    "Deaths in stroke-free analytic intervals",
    "Post-stroke death events in complete-score intervals"
  ),
  n = c(
    nrow(eligible_50plus),
    nrow(analytic_multistate),
    nrow(analytic_stroke_free),
    sum(
      analytic_stroke_free$transition == "0_to_1_stroke"
    ),
    sum(
      analytic_stroke_free$transition == "0_to_2_death"
    ),
    sum(
      analytic_multistate$transition == "1_to_2_death"
    )
  )
)

score_distribution <- analytic_multistate |>
  summarise(
    n = n(),
    mean = round(mean(adapted_hbcs_18), 2),
    sd = round(sd(adapted_hbcs_18), 2),
    minimum = min(adapted_hbcs_18),
    p25 = quantile(adapted_hbcs_18, 0.25),
    median = median(adapted_hbcs_18),
    p75 = quantile(adapted_hbcs_18, 0.75),
    maximum = max(adapted_hbcs_18)
  )

saveRDS(
  person_scored,
  file.path(derived_dir, "hrs_person_wave_scored.rds")
)

saveRDS(
  transition_scored,
  file.path(derived_dir, "hrs_transition_intervals_scored.rds")
)

saveRDS(
  analytic_multistate,
  file.path(derived_dir, "hrs_multistate_analytic.rds")
)

saveRDS(
  analytic_stroke_free,
  file.path(derived_dir, "hrs_stroke_free_analytic.rds")
)

write_csv(
  component_missing,
  file.path(results_dir, "hrs_hbcs_component_summary.csv")
)

write_csv(
  analytic_flow,
  file.path(results_dir, "hrs_hbcs_analytic_flow.csv")
)

write_csv(
  score_distribution,
  file.path(results_dir, "hrs_hbcs_score_distribution.csv")
)

cat("HRS adapted hBCS analytic flow:\n")
print(as.data.frame(analytic_flow), row.names = FALSE)

cat("\nComponent summary:\n")
print(as.data.frame(component_missing), row.names = FALSE)

cat("\nScore distribution:\n")
print(as.data.frame(score_distribution), row.names = FALSE)

cat("\nOutputs written successfully.\n")

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir, "hrs_hbcs_sessionInfo.txt")
)

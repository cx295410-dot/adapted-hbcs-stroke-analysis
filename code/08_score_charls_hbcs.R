#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data/derived")
results_dir <- file.path(root, "results")

regular_file <- file.path(
  root, "data/CHARLS/Harmonized/H_CHARLS_D_Data.dta"
)

person_file <- file.path(
  derived_dir, "charls_person_wave_raw.rds"
)

transition_file <- file.path(
  derived_dir, "charls_transition_intervals_raw.rds"
)

stopifnot(
  file.exists(regular_file),
  file.exists(person_file),
  file.exists(transition_file)
)

log_file <- file.path(results_dir, "08_score_charls_hbcs.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

waves <- tibble(
  wave = 1:4,
  year = c(2011L, 2013L, 2015L, 2018L)
)

mood_roots <- c(
  "depresl", "effortl", "sleeprl", "whappyl",
  "flonel", "botherl", "goingl", "mindtsl",
  "fhopel", "fearll"
)

mood_vars <- c(
  "ID",
  unlist(map(1:4, \(w) paste0("r", w, mood_roots)))
)

cat("Reading corrected CESD item fields only...\n")

mood_raw <- read_dta(
  regular_file,
  col_select = tidyselect::all_of(mood_vars)
)

get_mood <- function(name) {
  as.numeric(mood_raw[[name]])
}

mood_patch <- map2_dfr(
  waves$wave,
  waves$year,
  function(w, calendar_year) {
    p <- paste0("r", w)

    tibble(
      ID = as.character(mood_raw$ID),
      wave = w,
      year_check = calendar_year,
      mood_depressed = get_mood(paste0(p, "depresl")),
      mood_effort = get_mood(paste0(p, "effortl")),
      sleep_restless = get_mood(paste0(p, "sleeprl")),
      mood_happy = get_mood(paste0(p, "whappyl")),
      mood_lonely = get_mood(paste0(p, "flonel")),
      mood_bothered = get_mood(paste0(p, "botherl")),
      mood_could_not_get_going =
        get_mood(paste0(p, "goingl")),
      mood_mind = get_mood(paste0(p, "mindtsl")),
      mood_hopeful = get_mood(paste0(p, "fhopel")),
      mood_fearful = get_mood(paste0(p, "fearll"))
    )
  }
)

mood_names <- c(
  "mood_depressed", "mood_effort", "sleep_restless",
  "mood_happy", "mood_lonely", "mood_bothered",
  "mood_could_not_get_going", "mood_mind",
  "mood_hopeful", "mood_fearful"
)

person_wave <- readRDS(person_file) |>
  select(-any_of(mood_names)) |>
  left_join(
    select(mood_patch, -year_check),
    by = c("ID", "wave")
  )

transitions <- readRDS(transition_file) |>
  select(-any_of(mood_names)) |>
  left_join(
    select(mood_patch, -year_check),
    by = c("ID", "wave")
  )

map_code <- function(x, code_map) {
  unname(code_map[as.character(x)])
}

negative_symptom <- function(x) {
  case_when(
    x %in% 1:2 ~ 0,
    x %in% 3:4 ~ 1,
    TRUE ~ NA_real_
  )
}

positive_symptom_absent <- function(x) {
  case_when(
    x %in% 1:2 ~ 1,
    x %in% 3:4 ~ 0,
    TRUE ~ NA_real_
  )
}

score_charls <- function(d) {

  # 1. Smoking
  d$score_smoking <- case_when(
    d$smoking_ever == 0 ~ 2,
    d$smoking_ever == 1 & d$smoking_current == 0 ~ 1,
    d$smoking_current == 1 ~ 0,
    TRUE ~ NA_real_
  )

  # 2. Alcohol
  # Approximate drinking days/week from CHARLS frequency categories.
  frequency_map <- c(
    `0` = 0,
    `1` = 0.125,
    `2` = 0.25,
    `3` = 0.625,
    `4` = 1,
    `5` = 2.5,
    `6` = 5,
    `7` = 7,
    `8` = 14,
    `9` = 21
  )

  # Representative drinks/day for each reported range.
  amount_map <- c(
    `0` = 0,
    `1` = 0.5,
    `2` = 1,
    `3` = 2,
    `4` = 3
  )

  d$alcohol_days_approx <- map_code(
    d$alcohol_frequency, frequency_map
  )

  d$alcohol_amount_approx <- map_code(
    d$alcohol_amount_range, amount_map
  )

  d$alcohol_weekly_approx <- case_when(
    d$alcohol_any == 0 ~ 0,
    d$alcohol_any == 1 &
      !is.na(d$alcohol_days_approx) &
      !is.na(d$alcohol_amount_approx) ~
        d$alcohol_days_approx *
        d$alcohol_amount_approx,
    TRUE ~ NA_real_
  )

  alcohol_limit <- case_when(
    d$sex == 1 ~ 14,
    d$sex == 2 ~ 7,
    TRUE ~ NA_real_
  )

  d$score_alcohol <- case_when(
    !is.na(d$alcohol_weekly_approx) &
      d$alcohol_weekly_approx <= alcohol_limit ~ 2,
    !is.na(d$alcohol_weekly_approx) &
      d$alcohol_weekly_approx <= 2 * alcohol_limit ~ 1,
    !is.na(d$alcohol_weekly_approx) ~ 0,
    TRUE ~ NA_real_
  )

  # 3. Physical activity
  d$score_activity <- case_when(
    d$activity_vigorous_days >= 2 |
      d$activity_moderate_days >= 2 ~ 2,

    d$activity_vigorous_days == 1 |
      d$activity_moderate_days == 1 |
      d$activity_light_days >= 3 ~ 1,

    d$activity_vigorous_days == 0 &
      d$activity_moderate_days == 0 &
      d$activity_light_days < 3 ~ 0,

    TRUE ~ NA_real_
  )

  # 4. BMI
  d$score_bmi <- case_when(
    d$bmi >= 18.5 & d$bmi < 25 ~ 2,
    d$bmi >= 25 & d$bmi < 30 ~ 1,
    !is.na(d$bmi) ~ 0,
    TRUE ~ NA_real_
  )

  # 5–6. Hypertension and diabetes
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

  # 7. Restless sleep:
  # codes 1–2 = fewer than 3 days/week;
  # codes 3–4 = at least 3 days/week.
  d$score_sleep <- case_when(
    d$sleep_restless %in% 1:2 ~ 2,
    d$sleep_restless %in% 3:4 ~ 0,
    TRUE ~ NA_real_
  )

  # 8. Eight non-overlapping CESD items.
  # Restless sleep and loneliness are excluded.
  mood_matrix <- cbind(
    negative_symptom(d$mood_depressed),
    negative_symptom(d$mood_effort),
    negative_symptom(d$mood_bothered),
    negative_symptom(d$mood_could_not_get_going),
    negative_symptom(d$mood_mind),
    negative_symptom(d$mood_fearful),
    positive_symptom_absent(d$mood_happy),
    positive_symptom_absent(d$mood_hopeful)
  )

  mood_observed <- rowSums(!is.na(mood_matrix))
  mood_symptoms <- rowSums(mood_matrix, na.rm = TRUE)

  d$mood_symptom_proportion <- ifelse(
    mood_observed >= 7,
    mood_symptoms / mood_observed,
    NA_real_
  )

  d$score_mood <- case_when(
    d$mood_symptom_proportion == 0 ~ 2,
    d$mood_symptom_proportion < 0.5 ~ 1,
    d$mood_symptom_proportion >= 0.5 ~ 0,
    TRUE ~ NA_real_
  )

  # 9. Social connection.
  # Main score uses marital/partner status for HRS comparability.
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

  d$adapted_hbcs_18_available <- ifelse(
    d$hbcs_domains_observed >= 7,
    rowSums(score_matrix, na.rm = TRUE) *
      9 / d$hbcs_domains_observed,
    NA_real_
  )

  d
}

person_scored <- score_charls(person_wave)
transition_scored <- score_charls(transitions)

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

component_summary <- map_dfr(score_names, function(v) {
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
    "Valid CHARLS transitions with baseline age 50+",
    "Complete nine-domain hBCS transitions",
    "Stroke-free complete-score intervals",
    "Incident stroke events",
    "Deaths without previously recorded stroke",
    "Post-stroke death events"
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
  file.path(derived_dir, "charls_person_wave_scored.rds")
)

saveRDS(
  transition_scored,
  file.path(derived_dir, "charls_transition_intervals_scored.rds")
)

saveRDS(
  analytic_multistate,
  file.path(derived_dir, "charls_multistate_analytic.rds")
)

saveRDS(
  analytic_stroke_free,
  file.path(derived_dir, "charls_stroke_free_analytic.rds")
)

write_csv(
  component_summary,
  file.path(results_dir, "charls_hbcs_component_summary.csv")
)

write_csv(
  analytic_flow,
  file.path(results_dir, "charls_hbcs_analytic_flow.csv")
)

write_csv(
  score_distribution,
  file.path(results_dir, "charls_hbcs_score_distribution.csv")
)

cat("CHARLS hBCS analytic flow:\n")
print(as.data.frame(analytic_flow), row.names = FALSE)

cat("\nComponent summary:\n")
print(as.data.frame(component_summary), row.names = FALSE)

cat("\nScore distribution:\n")
print(as.data.frame(score_distribution), row.names = FALSE)

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir, "charls_hbcs_sessionInfo.txt")
)

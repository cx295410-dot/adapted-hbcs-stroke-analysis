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

score_names <- c(
  "score_smoking", "score_alcohol", "score_activity",
  "score_bmi", "score_hypertension", "score_diabetes",
  "score_sleep", "score_mood", "score_social"
)

clean_and_rescore <- function(d) {

  d$bmi_original <- d$bmi

  d$bmi_outlier <- (
    !is.na(d$bmi_original) &
      (d$bmi_original < 12 | d$bmi_original > 60)
  )

  d$bmi <- ifelse(
    !is.na(d$bmi_original) &
      d$bmi_original >= 12 &
      d$bmi_original <= 60,
    d$bmi_original,
    NA_real_
  )

  d$score_bmi <- case_when(
    d$bmi >= 18.5 & d$bmi < 25 ~ 2,
    d$bmi >= 25 & d$bmi < 30 ~ 1,
    !is.na(d$bmi) ~ 0,
    TRUE ~ NA_real_
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

summarise_bmi <- function(d, cohort) {
  d |>
    group_by(wave) |>
    summarise(
      cohort = cohort,
      person_wave_rows = n(),
      bmi_observed_before =
        sum(!is.na(bmi_original)),
      bmi_below_12 =
        sum(bmi_original < 12, na.rm = TRUE),
      bmi_above_60 =
        sum(bmi_original > 60, na.rm = TRUE),
      bmi_removed =
        sum(bmi_outlier, na.rm = TRUE),
      bmi_observed_after =
        sum(!is.na(bmi)),
      bmi_mean_after =
        round(mean(bmi, na.rm = TRUE), 2),
      bmi_sd_after =
        round(sd(bmi, na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    select(cohort, everything())
}

hrs_person_file <- file.path(
  derived_dir, "hrs_person_wave_scored.rds"
)
hrs_transition_file <- file.path(
  derived_dir, "hrs_transition_intervals_scored.rds"
)
charls_person_file <- file.path(
  derived_dir, "charls_person_wave_scored.rds"
)
charls_transition_file <- file.path(
  derived_dir, "charls_transition_intervals_scored.rds"
)

stopifnot(
  file.exists(hrs_person_file),
  file.exists(hrs_transition_file),
  file.exists(charls_person_file),
  file.exists(charls_transition_file)
)

hrs_person <- clean_and_rescore(
  readRDS(hrs_person_file)
)
hrs_transition <- clean_and_rescore(
  readRDS(hrs_transition_file)
)

charls_person <- clean_and_rescore(
  readRDS(charls_person_file)
)
charls_transition <- clean_and_rescore(
  readRDS(charls_transition_file)
)

bmi_summary <- bind_rows(
  summarise_bmi(hrs_person, "HRS"),
  summarise_bmi(charls_person, "CHARLS")
)

hrs_analytic <- hrs_transition |>
  filter(
    !is.na(age),
    age >= 50,
    hbcs_complete
  )

charls_analytic <- charls_transition |>
  filter(
    !is.na(age),
    age >= 50,
    hbcs_complete
  )

hrs_stroke_free <- hrs_analytic |>
  filter(state == 0)

charls_stroke_free <- charls_analytic |>
  filter(state == 0)

flow_after_cleaning <- tibble(
  cohort = c(
    "HRS", "HRS", "HRS", "HRS",
    "CHARLS", "CHARLS", "CHARLS", "CHARLS"
  ),
  measure = c(
    "Complete-score transitions",
    "Stroke-free transitions",
    "Incident strokes",
    "Deaths without prior stroke",
    "Complete-score transitions",
    "Stroke-free transitions",
    "Incident strokes",
    "Deaths without prior stroke"
  ),
  n = c(
    nrow(hrs_analytic),
    nrow(hrs_stroke_free),
    sum(hrs_stroke_free$transition == "0_to_1_stroke"),
    sum(hrs_stroke_free$transition == "0_to_2_death"),
    nrow(charls_analytic),
    nrow(charls_stroke_free),
    sum(charls_stroke_free$transition == "0_to_1_stroke"),
    sum(charls_stroke_free$transition == "0_to_2_death")
  )
)

saveRDS(
  hrs_person,
  hrs_person_file
)
saveRDS(
  hrs_transition,
  hrs_transition_file
)
saveRDS(
  hrs_analytic,
  file.path(derived_dir, "hrs_multistate_analytic.rds")
)
saveRDS(
  hrs_stroke_free,
  file.path(derived_dir, "hrs_stroke_free_analytic.rds")
)

saveRDS(
  charls_person,
  charls_person_file
)
saveRDS(
  charls_transition,
  charls_transition_file
)
saveRDS(
  charls_analytic,
  file.path(derived_dir, "charls_multistate_analytic.rds")
)
saveRDS(
  charls_stroke_free,
  file.path(derived_dir, "charls_stroke_free_analytic.rds")
)

write_csv(
  bmi_summary,
  file.path(results_dir, "bmi_cleaning_summary.csv")
)

write_csv(
  flow_after_cleaning,
  file.path(
    results_dir,
    "analytic_flow_after_bmi_cleaning.csv"
  )
)

cat("BMI cleaning summary:\n")
print(as.data.frame(bmi_summary), row.names = FALSE)

cat("\nAnalytic flow after BMI cleaning:\n")
print(
  as.data.frame(flow_after_cleaning),
  row.names = FALSE
)

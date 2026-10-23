#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)

input_file <- file.path(
  root, "data/HRS/randhrs1992_2022v1.dta"
)

derived_dir <- file.path(root, "data/derived")
results_dir <- file.path(root, "results")

dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(input_file))

log_file <- file.path(results_dir, "05_build_hrs_long_data.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("Build HRS person-wave and transition data\n")
cat("Input:", input_file, "\n\n")

waves <- tibble(
  wave = 8:16,
  year = seq(2006, 2022, by = 2)
)

static_vars <- c(
  "hhidpn", "ragender", "raracem", "rahispan",
  "raeduc", "raedyrs", "rabyear", "radyear", "radmonth"
)

wave_roots <- c(
  "iwstat", "iwmid", "agey_m", "wtresp",
  "mstat", "mpart", "socwk",
  "stroke", "stroke2",
  "hibp", "hibpe", "diab", "diabe", "bmi",
  "smoken", "smokev",
  "drink", "drinkd", "drinkn",
  "vgactx", "mdactx", "ltactx",
  "cesd", "cesdm",
  "depres", "effort", "sleepr", "whappy",
  "flone", "flonel", "fsad", "sad",
  "going", "enlife",
  "sleepfal", "sleeprt", "sleepwkn", "sleepwke",
  "lblonely3"
)

cat("Reading one row to verify field names...\n")
header <- read_dta(input_file, n_max = 1)
available_names <- names(header)

wave_candidates <- unlist(
  map(waves$wave, \(w) paste0("r", w, wave_roots))
)

selected_vars <- intersect(
  unique(c(static_vars, wave_candidates)),
  available_names
)

missing_static <- setdiff(static_vars, selected_vars)

if (length(missing_static) > 0) {
  stop(
    "Required static fields missing: ",
    paste(missing_static, collapse = ", ")
  )
}

cat("Selected variables:", length(selected_vars), "\n")
cat("Reading selected fields only...\n")

raw <- read_dta(
  input_file,
  col_select = tidyselect::all_of(selected_vars)
)

stopifnot(nrow(raw) > 40000)
stopifnot(anyDuplicated(raw$hhidpn) == 0)

cat("Person records:", nrow(raw), "\n\n")

get_num <- function(candidates) {
  hit <- candidates[candidates %in% names(raw)]

  if (length(hit) == 0) {
    return(rep(NA_real_, nrow(raw)))
  }

  as.numeric(raw[[hit[1]]])
}

person_wave <- map2_dfr(
  waves$wave,
  waves$year,
  function(w, calendar_year) {

    rvar <- function(roots) {
      get_num(paste0("r", w, roots))
    }

    tibble(
      hhidpn = as.character(raw$hhidpn),
      wave = w,
      year = calendar_year,

      sex = get_num("ragender"),
      race = get_num("raracem"),
      ethnicity = get_num("rahispan"),
      education_category = get_num("raeduc"),
      education_years = get_num("raedyrs"),
      birth_year = get_num("rabyear"),
      death_year = get_num("radyear"),
      death_month = get_num("radmonth"),

      interview_status = rvar("iwstat"),
      interview_date = rvar("iwmid"),
      age = rvar("agey_m"),
      analysis_weight = rvar("wtresp"),

      marital_status = rvar("mstat"),
      partnered = rvar("mpart"),
      social_activity = rvar("socwk"),

      stroke = rvar("stroke"),
      stroke_confirmed = rvar("stroke2"),

      hypertension_current = rvar("hibp"),
      hypertension_ever = rvar("hibpe"),
      diabetes_current = rvar("diab"),
      diabetes_ever = rvar("diabe"),
      bmi = rvar("bmi"),

      smoking_current = rvar("smoken"),
      smoking_ever = rvar("smokev"),

      alcohol_any = rvar("drink"),
      alcohol_days = rvar("drinkd"),
      alcohol_amount = rvar("drinkn"),

      activity_vigorous = rvar("vgactx"),
      activity_moderate = rvar("mdactx"),
      activity_light = rvar("ltactx"),

      cesd_total = rvar("cesd"),
      cesd_missing_items = rvar("cesdm"),

      mood_depressed = rvar("depres"),
      mood_effort = rvar("effort"),
      sleep_restless = rvar("sleepr"),
      mood_happy = rvar("whappy"),
      mood_lonely = rvar(c("flone", "flonel")),
      mood_sad = rvar(c("fsad", "sad")),
      mood_could_not_get_going = rvar("going"),
      mood_enjoyed_life = rvar("enlife"),

      sleep_difficulty_falling = rvar("sleepfal"),
      sleep_rested_morning = rvar("sleeprt"),
      sleep_waking_night = rvar("sleepwkn"),
      sleep_waking_early = rvar("sleepwke"),
      loneliness_psychosocial = rvar("lblonely3")
    )
  }
) |>
  mutate(
    interviewed_alive = interview_status == 1,

    stroke_binary = case_when(
      stroke == 0 ~ 0L,
      stroke == 1 ~ 1L,
      TRUE ~ NA_integer_
    ),

    state = case_when(
      interview_status %in% c(5, 6) ~ 2L,
      interviewed_alive & stroke_binary == 0 ~ 0L,
      interviewed_alive & stroke_binary == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) |>
  arrange(hhidpn, wave)

cat("Person-wave rows:", nrow(person_wave), "\n")
cat(
  "Interviewed person-waves:",
  sum(person_wave$interviewed_alive, na.rm = TRUE),
  "\n\n"
)

transitions <- person_wave |>
  group_by(hhidpn) |>
  arrange(wave, .by_group = TRUE) |>
  mutate(
    next_wave = lead(wave),
    next_year = lead(year),
    next_state = lead(state),
    next_interview_status = lead(interview_status),
    next_stroke = lead(stroke_binary),
    interval_years = next_year - year
  ) |>
  ungroup() |>
  filter(
    next_wave == wave + 1,
    state %in% c(0L, 1L),
    next_state %in% c(0L, 1L, 2L)
  ) |>
  mutate(
    transition = case_when(
      state == 0L & next_state == 0L ~ "0_to_0",
      state == 0L & next_state == 1L ~ "0_to_1_stroke",
      state == 0L & next_state == 2L ~ "0_to_2_death",
      state == 1L & next_state == 1L ~ "1_to_1",
      state == 1L & next_state == 2L ~ "1_to_2_death",
      TRUE ~ "other"
    ),
    incident_stroke = as.integer(transition == "0_to_1_stroke"),
    death_without_prior_stroke =
      as.integer(transition == "0_to_2_death"),
    death_after_stroke =
      as.integer(transition == "1_to_2_death"),
    baseline_age_50plus =
      !is.na(age) & age >= 50
  )

stroke_free_intervals <- transitions |>
  filter(
    baseline_age_50plus,
    state == 0L
  )

transition_counts <- transitions |>
  count(transition, name = "intervals") |>
  arrange(transition)

sample_flow <- tibble(
  step = c(
    "HRS persons in source file",
    "Person-wave rows, waves 8-16",
    "Interviewed and alive person-waves",
    "Interviewed person-waves age 50+",
    "Valid adjacent-wave transitions",
    "Stroke-free baseline transitions age 50+",
    "Observed incident stroke transitions",
    "Deaths without previously recorded stroke",
    "Deaths after previously recorded stroke"
  ),
  n = c(
    n_distinct(person_wave$hhidpn),
    nrow(person_wave),
    sum(person_wave$interviewed_alive, na.rm = TRUE),
    sum(
      person_wave$interviewed_alive &
        !is.na(person_wave$age) &
        person_wave$age >= 50,
      na.rm = TRUE
    ),
    nrow(transitions),
    nrow(stroke_free_intervals),
    sum(transitions$incident_stroke, na.rm = TRUE),
    sum(transitions$death_without_prior_stroke, na.rm = TRUE),
    sum(transitions$death_after_stroke, na.rm = TRUE)
  )
)

variable_dictionary <- tribble(
  ~variable, ~definition,
  "state", "0=alive without reported stroke; 1=alive with reported stroke; 2=death recorded by next HRS wave",
  "transition", "Observed transition between two adjacent biennial waves",
  "incident_stroke", "0-to-1 transition among respondents without stroke at interval baseline",
  "death_without_prior_stroke", "0-to-2 transition without a stroke recorded before death",
  "death_after_stroke", "1-to-2 transition among respondents with stroke at interval baseline",
  "sleep_restless", "CESD restless-sleep item; retained as candidate sleep domain",
  "cesd_total", "Published RAND CESD summary; not used directly with overlapping sleep/loneliness domains",
  "social_activity", "Social participation candidate; marital_status is also retained",
  "analysis_weight", "Wave-specific RAND respondent analysis weight"
)

saveRDS(
  person_wave,
  file.path(derived_dir, "hrs_person_wave_raw.rds")
)

saveRDS(
  transitions,
  file.path(derived_dir, "hrs_transition_intervals_raw.rds")
)

saveRDS(
  stroke_free_intervals,
  file.path(derived_dir, "hrs_stroke_free_intervals_raw.rds")
)

write_csv(
  sample_flow,
  file.path(results_dir, "hrs_sample_flow_preliminary.csv")
)

write_csv(
  transition_counts,
  file.path(results_dir, "hrs_transition_counts.csv")
)

write_csv(
  variable_dictionary,
  file.path(results_dir, "hrs_long_variable_dictionary.csv")
)

cat("Preliminary sample flow:\n")
print(as.data.frame(sample_flow), row.names = FALSE)

cat("\nTransition counts:\n")
print(as.data.frame(transition_counts), row.names = FALSE)

cat("\nOutputs:\n")
cat(" - data/derived/hrs_person_wave_raw.rds\n")
cat(" - data/derived/hrs_transition_intervals_raw.rds\n")
cat(" - data/derived/hrs_stroke_free_intervals_raw.rds\n")
cat(" - results/hrs_sample_flow_preliminary.csv\n")
cat(" - results/hrs_transition_counts.csv\n")
cat(" - results/hrs_long_variable_dictionary.csv\n")

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir, "hrs_long_data_sessionInfo.txt")
)

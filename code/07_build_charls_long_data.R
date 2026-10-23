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
eol_file <- file.path(
  root, "data/CHARLS/End_of_Life/H_CHARLS_EOL_a.dta"
)

stopifnot(file.exists(regular_file), file.exists(eol_file))

dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(results_dir, "07_build_charls_long_data.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

waves <- tibble(
  wave = 1:4,
  year = c(2011L, 2013L, 2015L, 2018L)
)

static_vars <- c(
  "ID", "ragender", "raeduc_c", "rabyear",
  "radyear", "radmonth"
)

wave_roots <- c(
  "iwstat", "iwmid", "agey", "wtrespb",
  "mstat", "socwk",
  "stroke", "hibpe", "diabe",
  "mbmi", "bmi", "bmi_c",
  "smokev", "smoken",
  "drinkl", "drinkn_c", "drinkr_c",
  "vgact_c", "vgactx_c",
  "mdact_c", "mdactx_c",
  "ltact_c", "ltactx_c",
  "cesd10", "cesd10m",
  "bother", "botherl", "mindts", "depres",
  "effort", "hope", "fear", "sleeprl",
  "whappy", "flonel", "going"
)

header <- read_dta(regular_file, n_max = 1)
available_names <- names(header)

wave_candidates <- unlist(
  map(waves$wave, \(w) paste0("r", w, wave_roots))
)

selected <- intersect(
  unique(c(static_vars, wave_candidates)),
  available_names
)

cat("Reading", length(selected),
    "selected CHARLS regular variables...\n")

raw <- read_dta(
  regular_file,
  col_select = tidyselect::all_of(selected)
)

stopifnot(nrow(raw) > 25000)
stopifnot(anyDuplicated(raw$ID) == 0)

eol_header <- read_dta(eol_file, n_max = 1)

eol_selected <- intersect(
  c(
    "ID", "inxt", "raxt", "raxyear", "raxmonth",
    "radage", "ralstroke", "raxstrok", "raxstroke",
    "racod_c", "ragcod"
  ),
  names(eol_header)
)

eol <- read_dta(
  eol_file,
  col_select = tidyselect::all_of(eol_selected)
) |>
  distinct(ID, .keep_all = TRUE)

names(eol)[names(eol) != "ID"] <- paste0(
  "eol_", names(eol)[names(eol) != "ID"]
)

raw <- left_join(raw, eol, by = "ID")

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

    regular_death_year <- get_num("radyear")
    regular_death_month <- get_num("radmonth")
    eol_death_year <- get_num("eol_raxyear")
    eol_death_month <- get_num("eol_raxmonth")

    tibble(
      ID = as.character(raw$ID),
      wave = w,
      year = calendar_year,

      sex = get_num("ragender"),
      education = get_num("raeduc_c"),
      birth_year = get_num("rabyear"),

      death_year = coalesce(
        regular_death_year, eol_death_year
      ),
      death_month = coalesce(
        regular_death_month, eol_death_month
      ),

      interview_status = rvar("iwstat"),
      interview_date = rvar("iwmid"),
      age = rvar("agey"),
      analysis_weight = rvar("wtrespb"),

      marital_status = rvar("mstat"),
      social_activity = rvar("socwk"),

      stroke = rvar("stroke"),
      hypertension_ever = rvar("hibpe"),
      diabetes_ever = rvar("diabe"),

      bmi = rvar(c("mbmi", "bmi", "bmi_c")),

      smoking_ever = rvar("smokev"),
      smoking_current = rvar("smoken"),

      alcohol_any = rvar("drinkl"),
      alcohol_frequency = rvar("drinkn_c"),
      alcohol_amount_range = rvar("drinkr_c"),

      activity_vigorous_any = rvar("vgact_c"),
      activity_vigorous_days = rvar("vgactx_c"),
      activity_moderate_any = rvar("mdact_c"),
      activity_moderate_days = rvar("mdactx_c"),
      activity_light_any = rvar("ltact_c"),
      activity_light_days = rvar("ltactx_c"),

      cesd10 = rvar("cesd10"),
      cesd10_missing = rvar("cesd10m"),

      mood_bothered = rvar(c("bother", "botherl")),
      mood_mind = rvar("mindts"),
      mood_depressed = rvar("depres"),
      mood_effort = rvar("effort"),
      mood_hopeful = rvar("hope"),
      mood_fearful = rvar("fear"),
      sleep_restless = rvar("sleeprl"),
      mood_happy = rvar("whappy"),
      mood_lonely = rvar("flonel"),
      mood_could_not_get_going = rvar("going")
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

    state_observed = case_when(
      interview_status %in% c(5, 6) ~ 2L,
      interviewed_alive & stroke_binary == 0 ~ 0L,
      interviewed_alive & stroke_binary == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) |>
  arrange(ID, wave)

transitions <- person_wave |>
  group_by(ID) |>
  arrange(wave, .by_group = TRUE) |>
  mutate(
    next_wave = lead(wave),
    next_year = lead(year),
    next_state_observed = lead(state_observed),
    next_interview_status = lead(interview_status),
    interval_years = next_year - year,

    next_state = case_when(
      !is.na(next_state_observed) ~ next_state_observed,

      state_observed %in% c(0L, 1L) &
        !is.na(death_year) &
        death_year > year &
        death_year <= next_year ~ 2L,

      TRUE ~ NA_integer_
    )
  ) |>
  ungroup() |>
  filter(
    next_wave == wave + 1,
    state_observed %in% c(0L, 1L),
    next_state %in% c(0L, 1L, 2L)
  ) |>
  mutate(
    state = state_observed,

    transition = case_when(
      state == 0L & next_state == 0L ~ "0_to_0",
      state == 0L & next_state == 1L ~ "0_to_1_stroke",
      state == 0L & next_state == 2L ~ "0_to_2_death",
      state == 1L & next_state == 1L ~ "1_to_1",
      state == 1L & next_state == 2L ~ "1_to_2_death",
      TRUE ~ "other"
    )
  )

flow <- tibble(
  step = c(
    "CHARLS persons",
    "Person-wave rows",
    "Interviewed and alive person-waves",
    "Valid adjacent-wave transitions",
    "Valid transitions with baseline age 50+",
    "Stroke-free transitions age 50+",
    "Incident stroke transitions age 50+",
    "Deaths without recorded stroke age 50+",
    "Deaths after recorded stroke age 50+"
  ),
  n = c(
    n_distinct(person_wave$ID),
    nrow(person_wave),
    sum(person_wave$interviewed_alive, na.rm = TRUE),
    nrow(transitions),
    sum(!is.na(transitions$age) &
          transitions$age >= 50),
    sum(!is.na(transitions$age) &
          transitions$age >= 50 &
          transitions$state == 0),
    sum(!is.na(transitions$age) &
          transitions$age >= 50 &
          transitions$transition == "0_to_1_stroke"),
    sum(!is.na(transitions$age) &
          transitions$age >= 50 &
          transitions$transition == "0_to_2_death"),
    sum(!is.na(transitions$age) &
          transitions$age >= 50 &
          transitions$transition == "1_to_2_death")
  )
)

check_vars <- c(
  "sex", "marital_status", "social_activity",
  "hypertension_ever", "diabetes_ever",
  "smoking_ever", "smoking_current",
  "alcohol_any", "alcohol_frequency",
  "alcohol_amount_range",
  "activity_vigorous_any", "activity_vigorous_days",
  "activity_moderate_any", "activity_moderate_days",
  "activity_light_any", "activity_light_days",
  "cesd10", "cesd10_missing",
  "mood_bothered", "mood_mind", "mood_depressed",
  "mood_effort", "mood_hopeful", "mood_fearful",
  "sleep_restless", "mood_happy", "mood_lonely",
  "mood_could_not_get_going"
)

availability <- map_dfr(check_vars, function(v) {
  x <- person_wave[[v]]

  tibble(
    variable = v,
    observed = sum(!is.na(x)),
    missing_pct = round(100 * mean(is.na(x)), 2),
    distinct_values = n_distinct(x, na.rm = TRUE)
  )
})

categorical_counts <- map_dfr(
  setdiff(check_vars, "cesd10"),
  function(v) {
    x <- person_wave[[v]]

    tibble(
      variable = v,
      value = ifelse(is.na(x), "NA", as.character(x))
    ) |>
      count(variable, value, name = "n")
  }
)

saveRDS(
  person_wave,
  file.path(derived_dir, "charls_person_wave_raw.rds")
)

saveRDS(
  transitions,
  file.path(derived_dir, "charls_transition_intervals_raw.rds")
)

write_csv(
  flow,
  file.path(results_dir, "charls_sample_flow_preliminary.csv")
)

write_csv(
  availability,
  file.path(results_dir, "charls_component_availability.csv")
)

write_csv(
  categorical_counts,
  file.path(results_dir, "charls_component_value_counts.csv")
)

cat("CHARLS preliminary flow:\n")
print(as.data.frame(flow), row.names = FALSE)

cat("\nComponent availability:\n")
print(as.data.frame(availability), row.names = FALSE)

cat("\nOutputs written successfully.\n")

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir, "charls_long_data_sessionInfo.txt")
)

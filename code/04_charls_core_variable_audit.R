#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
regular_file <- file.path(
  root, "data/CHARLS/Harmonized/H_CHARLS_D_Data.dta"
)
eol_file <- file.path(
  root, "data/CHARLS/End_of_Life/H_CHARLS_EOL_a.dta"
)
results_dir <- file.path(root, "results")

stopifnot(file.exists(regular_file), file.exists(eol_file))
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(
  results_dir, "04_charls_core_variable_audit.log"
)
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("CHARLS core-variable audit\n\n")

header <- read_dta(regular_file, n_max = 1)
available_names <- names(header)

pick_first <- function(x) {
  hit <- x[x %in% available_names]
  if (length(hit) == 0) NA_character_ else hit[1]
}

years <- c(`1` = 2011L, `2` = 2013L,
           `3` = 2015L, `4` = 2018L)

wave_spec <- map_dfr(names(years), function(w) {
  p <- paste0("r", w)

  tibble(
    domain = c(
      "interview_status", "age", "analysis_weight",
      "marital_status", "social_activity", "loneliness",
      "stroke", "hypertension", "diabetes", "bmi",
      "smoking_ever", "smoking_current",
      "alcohol_any", "alcohol_frequency", "alcohol_amount",
      "activity_vigorous_any", "activity_vigorous_days",
      "activity_moderate_any", "activity_moderate_days",
      "activity_light_any", "activity_light_days",
      "mood_cesd10", "mood_missing_items", "sleep_restless"
    ),
    wave = as.integer(w),
    year = years[[w]],
    variable = c(
      paste0(p, "iwstat"),
      paste0(p, "agey"),
      paste0(p, "wtrespb"),
      paste0(p, "mstat"),
      paste0(p, "socwk"),
      paste0(p, "flonel"),
      paste0(p, "stroke"),
      paste0(p, "hibpe"),
      paste0(p, "diabe"),
      pick_first(c(
        paste0(p, "mbmi"),
        paste0(p, "bmi"),
        paste0(p, "bmi_c")
      )),
      paste0(p, "smokev"),
      paste0(p, "smoken"),
      paste0(p, "drinkl"),
      paste0(p, "drinkn_c"),
      paste0(p, "drinkr_c"),
      paste0(p, "vgact_c"),
      paste0(p, "vgactx_c"),
      paste0(p, "mdact_c"),
      paste0(p, "mdactx_c"),
      paste0(p, "ltact_c"),
      paste0(p, "ltactx_c"),
      paste0(p, "cesd10"),
      paste0(p, "cesd10m"),
      paste0(p, "sleeprl")
    ),
    denominator = paste0(p, "iwstat")
  )
})

static_spec <- tibble(
  domain = c(
    "identifier", "birth_year", "sex",
    "education", "death_year", "death_month"
  ),
  wave = NA_integer_,
  year = NA_integer_,
  variable = c(
    "ID", "rabyear", "ragender",
    "raeduc_c", "radyear", "radmonth"
  ),
  denominator = NA_character_
)

requested <- bind_rows(static_spec, wave_spec) |>
  mutate(
    available = !is.na(variable) &
      variable %in% available_names
  )

write_csv(
  requested,
  file.path(results_dir,
            "charls_core_requested_variables.csv"),
  na = ""
)

write_csv(
  filter(requested, !available),
  file.path(results_dir,
            "charls_core_missing_variables.csv"),
  na = ""
)

selected <- unique(c(
  requested$variable[requested$available],
  requested$denominator[!is.na(requested$denominator)]
))

cat("Reading", length(selected),
    "selected variables only...\n")

dat <- read_dta(
  regular_file,
  col_select = all_of(selected)
)

cat("Regular CHARLS records:", nrow(dat), "\n\n")

audit <- requested |>
  filter(available) |>
  pmap_dfr(function(domain, wave, year, variable,
                    denominator, available) {

    if (is.na(wave)) {
      eligible <- rep(TRUE, nrow(dat))
    } else {
      eligible <- !is.na(dat[[denominator]]) &
        dat[[denominator]] == 1
    }

    n_eligible <- sum(eligible)
    n_observed <- sum(!is.na(dat[[variable]][eligible]))

    tibble(
      domain = domain,
      wave = wave,
      year = year,
      variable = variable,
      total_records = nrow(dat),
      interviewed_records = n_eligible,
      observed_interviewed = n_observed,
      missing_pct_interviewed =
        if (n_eligible == 0) NA_real_ else
          round(100 * (n_eligible - n_observed) /
                  n_eligible, 2)
    )
  })

write_csv(
  audit,
  file.path(results_dir,
            "charls_core_availability.csv"),
  na = ""
)

domain_summary <- audit |>
  group_by(domain) |>
  summarise(
    variables_available = n(),
    minimum_observed_interviewed =
      min(observed_interviewed),
    median_missing_pct_interviewed =
      round(median(
        missing_pct_interviewed,
        na.rm = TRUE
      ), 2),
    .groups = "drop"
  )

write_csv(
  domain_summary,
  file.path(results_dir,
            "charls_core_domain_summary.csv"),
  na = ""
)

eol_header <- read_dta(eol_file, n_max = 1)
eol_names <- names(eol_header)

eol_requested <- tibble(
  domain = c(
    "identifier", "exit_status", "exit_wave",
    "death_year", "death_month", "age_at_death",
    "stroke_last_interview",
    "new_stroke_since_last_interview",
    "lifetime_stroke_exit",
    "cause_of_death", "grouped_cause_of_death"
  ),
  variable = c(
    "ID", "inxt", "raxt", "raxyear", "raxmonth",
    "radage", "ralstroke", "raxstrok", "raxstroke",
    "racod_c", "ragcod"
  )
) |>
  mutate(available = variable %in% eol_names)

write_csv(
  eol_requested,
  file.path(results_dir,
            "charls_eol_requested_variables.csv")
)

eol_vars <- eol_requested$variable[
  eol_requested$available
]

eol <- read_dta(
  eol_file,
  col_select = all_of(eol_vars)
)

eol_availability <- eol_requested |>
  filter(available) |>
  transmute(
    domain,
    variable,
    total_records = nrow(eol),
    observed = map_int(
      variable,
      \(x) sum(!is.na(eol[[x]]))
    ),
    missing_pct = map_dbl(
      variable,
      \(x) round(100 * mean(is.na(eol[[x]])), 2)
    )
  )

write_csv(
  eol_availability,
  file.path(results_dir,
            "charls_eol_availability.csv"),
  na = ""
)

cat("Core-domain summary:\n")
print(as.data.frame(domain_summary), row.names = FALSE)

cat("\nEnd-of-life availability:\n")
print(as.data.frame(eol_availability), row.names = FALSE)

cat("\nOutputs written to results/.\n")

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir,
            "charls_core_audit_sessionInfo.txt")
)

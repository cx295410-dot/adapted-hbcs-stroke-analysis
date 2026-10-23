#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(survival)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(tibble)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data/derived")
results_dir <- file.path(root, "results")

hrs <- readRDS(file.path(
  derived_dir, "hrs_multistate_analytic.rds"
))

charls <- readRDS(file.path(
  derived_dir, "charls_multistate_analytic.rds"
))

score_group <- function(x) {
  factor(
    case_when(
      x <= 10 ~ "Q1_0_to_10",
      x <= 12 ~ "Q2_11_to_12",
      x <= 14 ~ "Q3_13_to_14",
      x >= 15 ~ "Q4_15_to_18",
      TRUE ~ NA_character_
    ),
    levels = c(
      "Q1_0_to_10",
      "Q2_11_to_12",
      "Q3_13_to_14",
      "Q4_15_to_18"
    )
  )
}

hrs <- hrs |>
  mutate(
    hbcs_category = score_group(adapted_hbcs_18),
    id_cluster = hhidpn,
    tstart = 0,
    tstop = interval_years
  )

charls <- charls |>
  mutate(
    hbcs_category = score_group(adapted_hbcs_18),
    id_cluster = ID,
    tstart = 0,
    tstop = interval_years
  )

hrs_baseline <- hrs |>
  filter(state == 0) |>
  arrange(hhidpn, wave) |>
  group_by(hhidpn) |>
  slice(1) |>
  ungroup() |>
  mutate(
    female = sex == 2,
    married_partnered = marital_status %in% c(1, 2, 3),
    current_smoker = smoking_current == 1,
    hypertension = hypertension_ever == 1,
    diabetes = diabetes_ever == 1
  )

charls_baseline <- charls |>
  filter(state == 0) |>
  arrange(ID, wave) |>
  group_by(ID) |>
  slice(1) |>
  ungroup() |>
  mutate(
    female = sex == 2,
    married_partnered = marital_status %in% c(1, 2, 3),
    current_smoker = smoking_current == 1,
    hypertension = hypertension_ever == 1,
    diabetes = diabetes_ever == 1
  )

fmt_mean_sd <- function(x) {
  sprintf(
    "%.2f (%.2f)",
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

fmt_n_pct <- function(x) {
  denominator <- sum(!is.na(x))
  numerator <- sum(x, na.rm = TRUE)

  sprintf(
    "%d (%.1f%%)",
    numerator,
    100 * numerator / denominator
  )
}

make_table1 <- function(data, education_variable,
                        education_label) {

  group_names <- c(
    "Overall",
    levels(data$hbcs_category)
  )

  long_table <- map_dfr(group_names, function(g) {

    d <- if (g == "Overall") {
      data
    } else {
      filter(data, hbcs_category == g)
    }

    education_value <- d[[education_variable]]

    tibble(
      group = g,
      characteristic = c(
        "Participants, n",
        "Age, mean (SD)",
        "Women, n (%)",
        education_label,
        "Married/partnered, n (%)",
        "Current smoker, n (%)",
        "Hypertension, n (%)",
        "Diabetes, n (%)",
        "BMI, mean (SD)",
        "Adapted hBCS, mean (SD)",
        "Ideal smoking score, n (%)",
        "Ideal alcohol score, n (%)",
        "Ideal activity score, n (%)",
        "Ideal BMI score, n (%)",
        "Ideal sleep score, n (%)",
        "Ideal mood score, n (%)",
        "Ideal social score, n (%)"
      ),
      value = c(
        as.character(nrow(d)),
        fmt_mean_sd(d$age),
        fmt_n_pct(d$female),
        fmt_mean_sd(education_value),
        fmt_n_pct(d$married_partnered),
        fmt_n_pct(d$current_smoker),
        fmt_n_pct(d$hypertension),
        fmt_n_pct(d$diabetes),
        fmt_mean_sd(d$bmi),
        fmt_mean_sd(d$adapted_hbcs_18),
        fmt_n_pct(d$score_smoking == 2),
        fmt_n_pct(d$score_alcohol == 2),
        fmt_n_pct(d$score_activity == 2),
        fmt_n_pct(d$score_bmi == 2),
        fmt_n_pct(d$score_sleep == 2),
        fmt_n_pct(d$score_mood == 2),
        fmt_n_pct(d$score_social == 2)
      )
    )
  })

  long_table |>
    pivot_wider(
      names_from = group,
      values_from = value
    )
}

hrs_table1 <- make_table1(
  hrs_baseline,
  "education_years",
  "Education years, mean (SD)"
)

charls_table1 <- make_table1(
  charls_baseline,
  "education",
  "Education category code, mean (SD)"
)

write_csv(
  hrs_table1,
  file.path(results_dir, "table1_hrs_baseline.csv")
)

write_csv(
  charls_table1,
  file.path(results_dir, "table1_charls_baseline.csv")
)

category_counts <- bind_rows(
  count(
    hrs_baseline,
    hbcs_category,
    name = "participants"
  ) |>
    mutate(cohort = "HRS", .before = 1),

  count(
    charls_baseline,
    hbcs_category,
    name = "participants"
  ) |>
    mutate(cohort = "CHARLS", .before = 1)
)

write_csv(
  category_counts,
  file.path(results_dir, "hbcs_category_counts.csv")
)

fit_category_model <- function(
    data, cohort, transition_name,
    start_state, event_transition,
    covariates) {

  dat <- data |>
    filter(
      state == start_state,
      !is.na(hbcs_category)
    ) |>
    mutate(
      event = as.integer(
        transition == event_transition
      ),
      hbcs_category_number =
        as.numeric(hbcs_category)
    )

  rhs <- paste(
    c(
      "hbcs_category",
      covariates,
      "strata(wave)",
      "cluster(id_cluster)"
    ),
    collapse = " + "
  )

  fit <- coxph(
    as.formula(
      paste0(
        "Surv(tstart, tstop, event) ~ ",
        rhs
      )
    ),
    data = dat,
    ties = "efron",
    na.action = na.omit
  )

  trend_rhs <- paste(
    c(
      "hbcs_category_number",
      covariates,
      "strata(wave)",
      "cluster(id_cluster)"
    ),
    collapse = " + "
  )

  trend_fit <- coxph(
    as.formula(
      paste0(
        "Surv(tstart, tstop, event) ~ ",
        trend_rhs
      )
    ),
    data = dat,
    ties = "efron",
    na.action = na.omit
  )

  trend_summary <- summary(trend_fit)
  trend_coef <- as.data.frame(
    trend_summary$coefficients
  )

  p_trend <- trend_coef[
    "hbcs_category_number",
    "Pr(>|z|)"
  ]

  sm <- summary(fit)
  coefficients <- as.data.frame(sm$coefficients)

  robust_column <- if (
    "robust se" %in% colnames(coefficients)
  ) {
    "robust se"
  } else {
    "se(coef)"
  }

  reference <- tibble(
    cohort = cohort,
    transition = transition_name,
    category = "Q1_0_to_10",
    model_intervals = fit$n,
    model_events = fit$nevent,
    hazard_ratio = 1,
    lower_95 = 1,
    upper_95 = 1,
    p_value = NA_real_,
    p_trend = p_trend
  )

  estimates <- map_dfr(
    c("Q2_11_to_12", "Q3_13_to_14", "Q4_15_to_18"),
    function(category_name) {

      term <- paste0(
        "hbcs_category",
        category_name
      )

      beta <- coefficients[term, "coef"]
      robust_se <- coefficients[
        term, robust_column
      ]

      tibble(
        cohort = cohort,
        transition = transition_name,
        category = category_name,
        model_intervals = fit$n,
        model_events = fit$nevent,
        hazard_ratio = exp(beta),
        lower_95 = exp(beta - 1.96 * robust_se),
        upper_95 = exp(beta + 1.96 * robust_se),
        p_value = coefficients[
          term, "Pr(>|z|)"
        ],
        p_trend = p_trend
      )
    }
  )

  bind_rows(reference, estimates)
}

hrs_covariates <- c(
  "age", "factor(sex)", "education_years",
  "factor(race)", "factor(ethnicity)"
)

charls_covariates <- c(
  "age", "factor(sex)", "factor(education)"
)

category_models <- bind_rows(
  fit_category_model(
    hrs, "HRS", "0_to_1_stroke",
    0, "0_to_1_stroke", hrs_covariates
  ),
  fit_category_model(
    hrs, "HRS", "0_to_2_death",
    0, "0_to_2_death", hrs_covariates
  ),
  fit_category_model(
    hrs, "HRS", "1_to_2_death",
    1, "1_to_2_death", hrs_covariates
  ),
  fit_category_model(
    charls, "CHARLS", "0_to_1_stroke",
    0, "0_to_1_stroke", charls_covariates
  ),
  fit_category_model(
    charls, "CHARLS", "0_to_2_death",
    0, "0_to_2_death", charls_covariates
  )
) |>
  mutate(
    across(
      c(
        hazard_ratio, lower_95, upper_95,
        p_value, p_trend
      ),
      \(x) round(x, 4)
    ),
    p_trend_label = ifelse(
      p_trend < 0.0001,
      "<0.0001",
      formatC(
        p_trend,
        format = "f",
        digits = 4
      )
    )
  )

write_csv(
  category_models,
  file.path(results_dir,
            "hbcs_category_model_summary.csv")
)

cat("HRS baseline Table 1:\n")
print(as.data.frame(hrs_table1), row.names = FALSE)

cat("\nCHARLS baseline Table 1:\n")
print(as.data.frame(charls_table1), row.names = FALSE)

cat("\nCategorical dose-response models:\n")
print(as.data.frame(category_models), row.names = FALSE)

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir,
            "descriptive_models_sessionInfo.txt")
)

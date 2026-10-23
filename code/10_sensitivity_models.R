#!/usr/bin/env Rscript

if (!requireNamespace("survival", quietly = TRUE)) {
  stop("Package 'survival' is required")
}

suppressPackageStartupMessages({
  library(survival)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data/derived")
results_dir <- file.path(root, "results")

hrs <- readRDS(file.path(
  derived_dir, "hrs_transition_intervals_scored.rds"
))

charls <- readRDS(file.path(
  derived_dir, "charls_transition_intervals_scored.rds"
))

no_activity_components <- c(
  "score_smoking", "score_alcohol", "score_bmi",
  "score_hypertension", "score_diabetes",
  "score_sleep", "score_mood", "score_social"
)

prepare_scores <- function(d, id_name) {
  no_activity_matrix <- as.matrix(
    d[, no_activity_components]
  )

  d |>
    mutate(
      id_cluster = .data[[id_name]],
      tstart = 0,
      tstop = interval_years,

      hbcs_no_activity_16 = ifelse(
        rowSums(!is.na(no_activity_matrix)) == 8,
        rowSums(no_activity_matrix),
        NA_real_
      )
    ) |>
    filter(!is.na(age), age >= 50)
}

hrs <- prepare_scores(hrs, "hhidpn")
charls <- prepare_scores(charls, "ID")

hrs_covariates <- c(
  "age", "factor(sex)", "education_years",
  "factor(race)", "factor(ethnicity)"
)

charls_covariates <- c(
  "age", "factor(sex)", "factor(education)"
)

variants <- tribble(
  ~variant, ~score_variable, ~weighted,
  "Primary_complete_9_domains",
  "adapted_hbcs_18", FALSE,

  "Survey_weighted_complete_9_domains",
  "adapted_hbcs_18", TRUE,

  "Available_score_7plus_domains",
  "adapted_hbcs_18_available", FALSE,

  "Complete_8_domains_without_activity",
  "hbcs_no_activity_16", FALSE
)

fit_sensitivity <- function(
    data, cohort, transition_name,
    state_at_start, event_transition,
    covariates) {

  map_dfr(seq_len(nrow(variants)), function(i) {

    variant <- variants$variant[i]
    score_variable <- variants$score_variable[i]
    weighted <- variants$weighted[i]

    dat <- data |>
      filter(
        state == state_at_start,
        !is.na(.data[[score_variable]])
      ) |>
      mutate(
        exposure_per3 = .data[[score_variable]] / 3,
        event = as.integer(transition == event_transition)
      )

    if (weighted) {
      dat <- dat |>
        filter(
          !is.na(analysis_weight),
          analysis_weight > 0
        ) |>
        mutate(
          model_weight =
            analysis_weight /
            mean(analysis_weight)
        )
    } else {
      dat$model_weight <- 1
    }

    if (sum(dat$event) < 30) {
      return(tibble())
    }

    rhs <- paste(
      c(
        "exposure_per3",
        covariates,
        "strata(wave)",
        "cluster(id_cluster)"
      ),
      collapse = " + "
    )

    form <- as.formula(
      paste0(
        "Surv(tstart, tstop, event) ~ ",
        rhs
      )
    )

    fit <- coxph(
      form,
      data = dat,
      weights = model_weight,
      ties = "efron",
      na.action = na.omit
    )

    sm <- summary(fit)
    coef_table <- as.data.frame(sm$coefficients)

    term <- "exposure_per3"
    beta <- coef_table[term, "coef"]

    robust_se <- if (
      "robust se" %in% colnames(coef_table)
    ) {
      coef_table[term, "robust se"]
    } else {
      coef_table[term, "se(coef)"]
    }

    p_value <- coef_table[term, "Pr(>|z|)"]

    model_data <- model.frame(fit)
    score_sd <- sd(
      dat[[score_variable]],
      na.rm = TRUE
    )

    tibble(
      cohort = cohort,
      transition = transition_name,
      sensitivity_analysis = variant,
      score_variable = score_variable,
      model_intervals = fit$n,
      model_events = fit$nevent,
      hazard_ratio_per_3_points = exp(beta),
      lower_95 = exp(beta - 1.96 * robust_se),
      upper_95 = exp(beta + 1.96 * robust_se),
      p_value = p_value,
      p_label = ifelse(
        p_value < 0.0001,
        "<0.0001",
        formatC(p_value, format = "f", digits = 4)
      ),
      score_sd = score_sd,
      hazard_ratio_per_sd =
        exp(beta * score_sd / 3),
      lower_95_per_sd =
        exp((beta - 1.96 * robust_se) * score_sd / 3),
      upper_95_per_sd =
        exp((beta + 1.96 * robust_se) * score_sd / 3),
      concordance = unname(sm$concordance[1])
    )
  })
}

results <- bind_rows(
  fit_sensitivity(
    hrs, "HRS", "0_to_1_stroke",
    0, "0_to_1_stroke", hrs_covariates
  ),
  fit_sensitivity(
    hrs, "HRS", "0_to_2_death",
    0, "0_to_2_death", hrs_covariates
  ),
  fit_sensitivity(
    hrs, "HRS", "1_to_2_death",
    1, "1_to_2_death", hrs_covariates
  ),
  fit_sensitivity(
    charls, "CHARLS", "0_to_1_stroke",
    0, "0_to_1_stroke", charls_covariates
  ),
  fit_sensitivity(
    charls, "CHARLS", "0_to_2_death",
    0, "0_to_2_death", charls_covariates
  )
) |>
  mutate(
    across(
      c(
        hazard_ratio_per_3_points,
        lower_95, upper_95, p_value,
        score_sd, hazard_ratio_per_sd,
        lower_95_per_sd, upper_95_per_sd,
        concordance
      ),
      \(x) round(x, 4)
    )
  )

write_csv(
  results,
  file.path(results_dir,
            "sensitivity_model_estimates.csv")
)

compact <- results |>
  select(
    cohort, transition, sensitivity_analysis,
    model_intervals, model_events,
    hazard_ratio_per_3_points,
    lower_95, upper_95, p_label,
    hazard_ratio_per_sd,
    lower_95_per_sd, upper_95_per_sd
  )

write_csv(
  compact,
  file.path(results_dir,
            "sensitivity_model_summary.csv")
)

cat("Sensitivity analyses:\n")
print(as.data.frame(compact), row.names = FALSE)

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir,
            "sensitivity_models_sessionInfo.txt")
)

#!/usr/bin/env Rscript

if (!requireNamespace("survival", quietly = TRUE)) {
  stop(
    "Package 'survival' is required. Install in Ubuntu with: ",
    "install.packages('survival')"
  )
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

hrs_file <- file.path(
  derived_dir, "hrs_multistate_analytic.rds"
)
charls_file <- file.path(
  derived_dir, "charls_multistate_analytic.rds"
)

stopifnot(file.exists(hrs_file), file.exists(charls_file))

log_file <- file.path(results_dir, "09_primary_transition_models.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

hrs <- readRDS(hrs_file) |>
  mutate(
    id_cluster = hhidpn,
    hbcs_per3 = adapted_hbcs_18 / 3,
    tstart = 0,
    tstop = interval_years
  )

charls <- readRDS(charls_file) |>
  mutate(
    id_cluster = ID,
    hbcs_per3 = adapted_hbcs_18 / 3,
    tstart = 0,
    tstop = interval_years
  )

extract_exposure <- function(
    fit, cohort, transition_name, model_name,
    score_sd, data_n, event_n) {

  sm <- summary(fit)
  coef_table <- as.data.frame(sm$coefficients)
  ci_table <- as.data.frame(sm$conf.int)

  term <- "hbcs_per3"

  if (!term %in% rownames(coef_table)) {
    stop("hBCS coefficient not found in fitted model")
  }

  beta <- coef_table[term, "coef"]
  robust_se <- if ("robust se" %in% colnames(coef_table)) {
    coef_table[term, "robust se"]
  } else {
    coef_table[term, "se(coef)"]
  }

  p_value <- coef_table[term, "Pr(>|z|)"]
  lower <- ci_table[term, "lower .95"]
  upper <- ci_table[term, "upper .95"]

  model_n <- fit$n
  model_events <- fit$nevent

  tibble(
    cohort = cohort,
    transition = transition_name,
    model = model_name,
    exposure = "Adapted hBCS per 3-point increase",
    input_intervals = data_n,
    input_events = event_n,
    model_intervals = model_n,
    model_events = model_events,
    beta = beta,
    robust_se = robust_se,
    hazard_ratio = exp(beta),
    lower_95 = lower,
    upper_95 = upper,
    p_value = p_value,
    score_sd = score_sd,
    hazard_ratio_per_sd = exp(beta * score_sd / 3),
    lower_95_per_sd = exp(
      (beta - 1.96 * robust_se) * score_sd / 3
    ),
    upper_95_per_sd = exp(
      (beta + 1.96 * robust_se) * score_sd / 3
    ),
    concordance = unname(sm$concordance[1])
  )
}

fit_models <- function(
    data, event_expression, cohort,
    transition_name, covariate_sets) {

  analysis_data <- data |>
    mutate(event = as.integer({{ event_expression }})) |>
    filter(
      !is.na(hbcs_per3),
      !is.na(tstop),
      tstop > 0
    )

  data_n <- nrow(analysis_data)
  event_n <- sum(analysis_data$event)
  score_sd <- sd(analysis_data$adapted_hbcs_18)

  if (event_n < 30) {
    cat(
      "Skipped", cohort, transition_name,
      "- fewer than 30 events\n"
    )
    return(tibble())
  }

  map_dfr(names(covariate_sets), function(model_name) {

    rhs <- paste(
      c(
        "hbcs_per3",
        covariate_sets[[model_name]],
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
      data = analysis_data,
      ties = "efron",
      na.action = na.omit
    )

    extract_exposure(
      fit = fit,
      cohort = cohort,
      transition_name = transition_name,
      model_name = model_name,
      score_sd = score_sd,
      data_n = data_n,
      event_n = event_n
    )
  })
}

hrs_covariates <- list(
  Model_1 = character(0),
  Model_2 = c(
    "age",
    "factor(sex)"
  ),
  Model_3 = c(
    "age",
    "factor(sex)",
    "education_years",
    "factor(race)",
    "factor(ethnicity)"
  )
)

charls_covariates <- list(
  Model_1 = character(0),
  Model_2 = c(
    "age",
    "factor(sex)"
  ),
  Model_3 = c(
    "age",
    "factor(sex)",
    "factor(education)"
  )
)

hrs_state0 <- hrs |>
  filter(state == 0)

hrs_state1 <- hrs |>
  filter(state == 1)

charls_state0 <- charls |>
  filter(state == 0)

results <- bind_rows(

  fit_models(
    hrs_state0,
    transition == "0_to_1_stroke",
    "HRS",
    "0_to_1_stroke",
    hrs_covariates
  ),

  fit_models(
    hrs_state0,
    transition == "0_to_2_death",
    "HRS",
    "0_to_2_death",
    hrs_covariates
  ),

  fit_models(
    hrs_state1,
    transition == "1_to_2_death",
    "HRS",
    "1_to_2_death",
    hrs_covariates
  ),

  fit_models(
    charls_state0,
    transition == "0_to_1_stroke",
    "CHARLS",
    "0_to_1_stroke",
    charls_covariates
  ),

  fit_models(
    charls_state0,
    transition == "0_to_2_death",
    "CHARLS",
    "0_to_2_death",
    charls_covariates
  )
)

results_clean <- results |>
  mutate(
    across(
      c(
        beta, robust_se, hazard_ratio,
        lower_95, upper_95, p_value,
        score_sd, hazard_ratio_per_sd,
        lower_95_per_sd, upper_95_per_sd,
        concordance
      ),
      \(x) round(x, 4)
    )
  )

write_csv(
  results_clean,
  file.path(results_dir,
            "primary_transition_model_estimates.csv")
)

model_summary <- results_clean |>
  select(
    cohort, transition, model,
    model_intervals, model_events,
    hazard_ratio, lower_95, upper_95,
    p_value, hazard_ratio_per_sd,
    lower_95_per_sd, upper_95_per_sd,
    concordance
  )

write_csv(
  model_summary,
  file.path(results_dir,
            "primary_transition_model_summary.csv")
)

cat("Primary transition models:\n")
print(as.data.frame(model_summary), row.names = FALSE)

cat("\nInterpretation: hazard ratios below 1 indicate that\n")
cat("higher adapted hBCS is associated with lower transition hazard.\n")

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir,
            "primary_models_sessionInfo.txt")
)

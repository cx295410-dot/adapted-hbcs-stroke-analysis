#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
results_dir <- file.path(root, "results")
table_dir <- file.path(results_dir, "tables")
figure_dir <- file.path(results_dir, "figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

transition_label <- function(x) {
  recode(
    x,
    "0_to_1_stroke" = "Stroke-free to incident stroke",
    "0_to_2_death" = "Stroke-free to death",
    "1_to_2_death" = "Post-stroke to death"
  )
}

primary <- read_csv(
  file.path(
    results_dir,
    "primary_transition_model_summary.csv"
  ),
  show_col_types = FALSE
)

main_results <- primary |>
  filter(model == "Model_3") |>
  mutate(
    outcome = transition_label(transition),
    `HR per 3-point increase (95% CI)` = sprintf(
      "%.3f (%.3f–%.3f)",
      hazard_ratio, lower_95, upper_95
    ),
    `P value` = ifelse(
      p_value < 0.0001,
      "<0.0001",
      sprintf("%.4f", p_value)
    ),
    `HR per SD increase (95% CI)` = sprintf(
      "%.3f (%.3f–%.3f)",
      hazard_ratio_per_sd,
      lower_95_per_sd,
      upper_95_per_sd
    )
  ) |>
  select(
    Cohort = cohort,
    Outcome = outcome,
    Intervals = model_intervals,
    Events = model_events,
    `HR per 3-point increase (95% CI)`,
    `HR per SD increase (95% CI)`,
    `P value`,
    Concordance = concordance
  )

write_csv(
  main_results,
  file.path(
    table_dir,
    "Table_2_Primary_transition_models.csv"
  )
)

sensitivity <- read_csv(
  file.path(
    results_dir,
    "sensitivity_model_summary.csv"
  ),
  show_col_types = FALSE
) |>
  mutate(
    outcome = transition_label(transition),
    estimate = sprintf(
      "%.3f (%.3f–%.3f)",
      hazard_ratio_per_3_points,
      lower_95,
      upper_95
    )
  ) |>
  select(
    Cohort = cohort,
    Outcome = outcome,
    Analysis = sensitivity_analysis,
    Intervals = model_intervals,
    Events = model_events,
    `HR per 3 points (95% CI)` = estimate,
    `P value` = p_label
  )

write_csv(
  sensitivity,
  file.path(
    table_dir,
    "Table_S1_Sensitivity_analyses.csv"
  )
)

category <- read_csv(
  file.path(
    results_dir,
    "hbcs_category_model_summary.csv"
  ),
  show_col_types = FALSE
) |>
  mutate(
    outcome = transition_label(transition),
    estimate = sprintf(
      "%.3f (%.3f–%.3f)",
      hazard_ratio,
      lower_95,
      upper_95
    ),
    category = recode(
      category,
      "Q1_0_to_10" = "0–10",
      "Q2_11_to_12" = "11–12",
      "Q3_13_to_14" = "13–14",
      "Q4_15_to_18" = "15–18"
    )
  )

category_table <- category |>
  select(
    Cohort = cohort,
    Outcome = outcome,
    `hBCS category` = category,
    Intervals = model_intervals,
    Events = model_events,
    `HR (95% CI)` = estimate,
    `P for trend` = p_trend_label
  )

write_csv(
  category_table,
  file.path(
    table_dir,
    "Table_S2_hBCS_categories.csv"
  )
)
  file.path(results_dir, "table1_hrs_baseline.csv"),
  file.path(table_dir, "Table_1A_HRS_baseline.csv"),
  overwrite = TRUE
)

file.copy(
  file.path(results_dir, "table1_charls_baseline.csv"),
  file.path(table_dir, "Table_1B_CHARLS_baseline.csv"),
  overwrite = TRUE
)layout(
  matrix(
    c(
      1, 1, 2, 2, 3, 3,
      4, 4, 4, 5, 5, 5
    ),
    nrow = 2,
    byrow = TRUE
  )
)

par(
  mar = c(5, 5, 3, 1),
  family = "sans"
)
file.copy(
  file.path(results_dir, "table1_charls_baseline.csv"),
  file.path(table_dir, "Table_1B_CHARLS_baseline.csv"),
  overwrite = TRUE
)
# Figure 2: primary forest plot
forest <- primary |>
  filter(model == "Model_3") |>
  mutate(
    label = paste(
      cohort,
      transition_label(transition),
      sep = ": "
    )
  ) |>
  arrange(desc(cohort), transition)

png(
  file.path(
    figure_dir,
    "Figure_2_Primary_forest_plot.png"
  ),
  width = 2400,
  height = 1600,
  res = 300
)

par(
  mar = c(5, 12, 2, 2),
  family = "sans"
)

y <- rev(seq_len(nrow(forest)))
x_ticks <- c(0.4, 0.5, 0.6, 0.7, 0.8, 1)

plot( log(forest$hazard_ratio), y, type = "n", xlim = log(c(0.35, 1.02)), ylim = c(0.5, nrow(forest) + 0.5), 
  xaxt = "n", yaxt = "n", xlab = "Hazard ratio per 3-point increase in adapted hBCS", ylab = "", bty = "n"
)

axis(
  1,
  at = log(x_ticks),
  labels = x_ticks
)

axis(
  2,
  at = y,
  labels = forest$label,
  las = 1,
  tick = FALSE
)

abline(v = log(1), lty = 2, col = "grey50")

segments(
  log(forest$lower_95),
  y,
  log(forest$upper_95),
  y,
  lwd = 2,
  col = ifelse(
    forest$cohort == "HRS",
    "#225EA8",
    "#D94801"
  )
)

points(
  log(forest$hazard_ratio),
  y,
  pch = 19,
  cex = 1.3,
  col = ifelse(
    forest$cohort == "HRS",
    "#225EA8",
    "#D94801"
  )
)

dev.off()

# Figure 3: categorical dose-response
panels <- split(
  category,
  interaction(
    category$cohort,
    category$transition,
    drop = TRUE
  )
)

png(
  file.path(
    figure_dir,
    "Figure_3_hBCS_dose_response.png"
  ),
  width = 3000,
  height = 2000,
  res = 300
)
layout(
  matrix(
    c(
      1, 1, 2, 2, 3, 3,
      4, 4, 4, 5, 5, 5
    ),
    nrow = 2,
    byrow = TRUE
  )
)

par(
  mar = c(5, 5, 3, 1),
  family = "sans"
)par(
  mfrow = c(2, 3),
  mar = c(5, 5, 3, 1),
  family = "sans"
)

for (panel in panels) {
  panel <- panel |>
    arrange(
      match(
        category,
        c("0–10", "11–12", "13–14", "15–18")
      )
    )

  x <- 1:4

  plot(
    x,
    panel$hazard_ratio,
    type = "b",
    pch = 19,
    lwd = 2,
    log = "y",
    ylim = c(0.18, 1.15),
    xaxt = "n",
    xlab = "Adapted hBCS category",
    ylab = "Hazard ratio",
    main = paste(
      unique(panel$cohort),
      transition_label(
        unique(panel$transition)
      ),
      sep = "\n"
    ),
    col = ifelse(
      unique(panel$cohort) == "HRS",
      "#225EA8",
      "#D94801"
    )
  )

  axis(
    1,
    at = x,
    labels = panel$category
  )

  arrows(
    x,
    panel$lower_95,
    x,
    panel$upper_95,
    angle = 90,
    code = 3,
    length = 0.05,
    col = ifelse(
      unique(panel$cohort) == "HRS",
      "#225EA8",
      "#D94801"
    )
  )

  abline(h = 1, lty = 2, col = "grey50")
}
plot.new()

text(
  0.5, 0.68,
  "CHARLS: stroke-to-death transition",
  font = 2,
  cex = 1.15
)

text(
  0.5, 0.46,
  "Not modeled because only 20\npost-stroke deaths were observed",
  cex = 1.05,
  col = "grey30"
)

box(col = "grey70")plot.new()
text(
  0.5, 0.55,
  "Higher adapted hBCS\nwas associated with\nlower transition hazards",
  cex = 1.2
)

dev.off()

at("Publication outputs created:\n")
cat(" - results/tables/Table_1A_HRS_baseline.csv\n")
cat(" - results/tables/Table_1B_CHARLS_baseline.csv\n")
cat(" - results/tables/Table_2_Primary_transition_models.csv\n")
cat(" - results/tables/Table_S1_Sensitivity_analyses.csv\n")
cat(" - results/tables/Table_S2_hBCS_categories.csv\n")
cat(" - results/figures/Figure_2_Primary_forest_plot.png\n")
cat(" - results/figures/Figure_3_hBCS_dose_response.png\n")

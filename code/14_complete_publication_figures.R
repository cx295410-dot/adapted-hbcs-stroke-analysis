#!/usr/bin/env Rscript

# Complete publication figures from existing CSV results.
# No raw .dta files are read and no models are refitted.

options(stringsAsFactors = FALSE)

results_dir <- "results"
figure_dir <- file.path(results_dir, "figures")

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

read_result <- function(filename) {
  path <- file.path(results_dir, filename)

  if (!file.exists(path)) {
    stop("Missing file: ", path)
  }

  read.csv(
    path,
    check.names = FALSE
  )
}

fmt_n <- function(x) {
  format(
    round(as.numeric(x)),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

find_n <- function(data, field, pattern) {
  hit <- grepl(
    pattern,
    data[[field]],
    ignore.case = TRUE
  )

  if (!any(hit)) {
    return(NA_real_)
  }

  as.numeric(data$n[which(hit)[1]])
}

save_figure <- function(
  filename,
  width,
  height,
  drawing_function
) {
  png(
    filename = file.path(
      figure_dir,
      paste0(filename, ".png")
    ),
    width = width * 300,
    height = height * 300,
    res = 300
  )

  drawing_function()
  dev.off()

  tiff(
    filename = file.path(
      figure_dir,
      paste0(filename, ".tiff")
    ),
    width = width * 600,
    height = height * 600,
    res = 600,
    compression = "lzw"
  )

  drawing_function()
  dev.off()
}

cohort_colours <- c(
  HRS = "#225EA8",
  CHARLS = "#D94801"
)

transition_titles <- c(
  "0_to_1_stroke" = "Stroke-free to stroke",
  "0_to_2_death" = "Stroke-free to death",
  "1_to_2_death" = "Stroke to death"
)

panel_specification <- data.frame(
  cohort = c(
    "HRS", "HRS", "HRS",
    "CHARLS", "CHARLS"
  ),
  transition = c(
    "0_to_1_stroke",
    "0_to_2_death",
    "1_to_2_death",
    "0_to_1_stroke",
    "0_to_2_death"
  ),
  stringsAsFactors = FALSE
)

five_panel_layout <- function() {
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
}

# ============================================================
# Figure 1: study flow
# ============================================================

hrs_pre <- read_result(
  "hrs_sample_flow_preliminary.csv"
)

charls_pre <- read_result(
  "charls_sample_flow_preliminary.csv"
)

hrs_score_flow <- read_result(
  "hrs_hbcs_analytic_flow.csv"
)

charls_score_flow <- read_result(
  "charls_hbcs_analytic_flow.csv"
)

clean_flow <- read_result(
  "analytic_flow_after_bmi_cleaning.csv"
)

primary <- read_result(
  "primary_transition_model_summary.csv"
)

primary_m3 <- primary[
  primary$model == "Model_3",
]

clean_count <- function(
  cohort_name,
  measure_pattern
) {
  hit <- clean_flow$cohort == cohort_name &
    grepl(
      measure_pattern,
      clean_flow$measure,
      ignore.case = TRUE
    )

  if (!any(hit)) {
    return(NA_real_)
  }

  as.numeric(clean_flow$n[which(hit)[1]])
}

model_line <- function(
  cohort_name,
  transition_name
) {
  row <- primary_m3[
    primary_m3$cohort == cohort_name &
      primary_m3$transition == transition_name,
  ]

  if (nrow(row) == 0) {
    return(NULL)
  }

  paste0(
    transition_titles[transition_name],
    ": ",
    fmt_n(row$model_intervals[1]),
    " intervals; ",
    fmt_n(row$model_events[1]),
    " events"
  )
}

make_flow_labels <- function(
  cohort_name,
  preliminary,
  score_flow
) {
  outcome_lines <- c(
    model_line(
      cohort_name,
      "0_to_1_stroke"
    ),
    model_line(
      cohort_name,
      "0_to_2_death"
    ),
    model_line(
      cohort_name,
      "1_to_2_death"
    )
  )

  outcome_lines <- outcome_lines[
    !vapply(
      outcome_lines,
      is.null,
      logical(1)
    )
  ]

  c(
    paste0(
      "Persons in source dataset\n",
      fmt_n(
        find_n(
          preliminary,
          "step",
          "persons"
        )
      )
    ),
    paste0(
      "Interviewed and alive person-waves\n",
      fmt_n(
        find_n(
          preliminary,
          "step",
          "Interviewed and alive person-waves"
        )
      )
    ),
    paste0(
      "Valid transitions with baseline age >=50\n",
      fmt_n(
        find_n(
          score_flow,
          "step",
          "Valid.*age 50"
        )
      )
    ),
    paste0(
      "Complete nine-domain hBCS transitions\n",
      fmt_n(
        clean_count(
          cohort_name,
          "Complete-score transitions"
        )
      )
    ),
    paste0(
      "Stroke-free complete-score intervals\n",
      fmt_n(
        clean_count(
          cohort_name,
          "Stroke-free transitions"
        )
      )
    ),
    paste(
      c(
        "Fully adjusted transition models",
        outcome_lines
      ),
      collapse = "\n"
    )
  )
}

draw_flow_panel <- function(
  cohort_name,
  labels
) {
  plot.new()

  plot.window(
    xlim = c(0, 1),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i"
  )

  y <- c(
    0.90,
    0.74,
    0.58,
    0.42,
    0.26,
    0.09
  )

  box_height <- c(
    0.055,
    0.055,
    0.055,
    0.055,
    0.055,
    0.075
  )

  for (i in seq_along(y)) {
    rect(
      xleft = 0.05,
      ybottom = y[i] - box_height[i],
      xright = 0.95,
      ytop = y[i] + box_height[i],
      border = cohort_colours[cohort_name],
      lwd = 1.5,
      col = "white"
    )

    text(
      x = 0.5,
      y = y[i],
      labels = labels[i],
      cex = ifelse(i == 6, 0.67, 0.78)
    )

    if (i < length(y)) {
      arrows(
        x0 = 0.5,
        y0 = y[i] - box_height[i],
        x1 = 0.5,
        y1 = y[i + 1] + box_height[i + 1],
        length = 0.07,
        col = "grey40"
      )
    }
  }

  title(
    main = cohort_name,
    font.main = 2,
    col.main = cohort_colours[cohort_name]
  )
}

hrs_flow_labels <- make_flow_labels(
  "HRS",
  hrs_pre,
  hrs_score_flow
)

charls_flow_labels <- make_flow_labels(
  "CHARLS",
  charls_pre,
  charls_score_flow
)

draw_figure_1 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(1, 1, 3, 1),
    oma = c(1, 1, 3, 1),
    family = "sans"
  )

  draw_flow_panel(
    "HRS",
    hrs_flow_labels
  )

  draw_flow_panel(
    "CHARLS",
    charls_flow_labels
  )

  mtext(
    "Study flow and transition-specific analytic samples",
    side = 3,
    outer = TRUE,
    line = 1,
    font = 2,
    cex = 1.3
  )
}

save_figure(
  "Figure_1_Study_flow",
  11,
  8,
  draw_figure_1
)

# ============================================================
# Figure 4: hBCS category distribution
# ============================================================

category_counts <- read_result(
  "hbcs_category_counts.csv"
)

category_order <- c(
  "Q1_0_to_10",
  "Q2_11_to_12",
  "Q3_13_to_14",
  "Q4_15_to_18"
)

category_labels <- c(
  "0-10",
  "11-12",
  "13-14",
  "15-18"
)

draw_category_panel <- function(
  cohort_name
) {
  data <- category_counts[
    category_counts$cohort == cohort_name,
  ]

  data <- data[
    match(
      category_order,
      data$hbcs_category
    ),
  ]

  percentages <-
    data$participants /
    sum(data$participants) * 100

  positions <- barplot(
    percentages,
    names.arg = category_labels,
    col = cohort_colours[cohort_name],
    border = NA,
    ylim = c(
      0,
      max(percentages) * 1.25
    ),
    xlab = "Adapted hBCS category",
    ylab = "Participants (%)",
    main = cohort_name
  )

  text(
    x = positions,
    y = percentages,
    labels = paste0(
      fmt_n(data$participants),
      "\n(",
      sprintf("%.1f", percentages),
      "%)"
    ),
    pos = 3,
    cex = 0.82
  )
}

draw_figure_4 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(5, 5, 3, 1),
    oma = c(1, 1, 3, 1),
    family = "sans"
  )

  draw_category_panel("HRS")
  draw_category_panel("CHARLS")

  mtext(
    "Baseline distribution of adapted hBCS categories",
    side = 3,
    outer = TRUE,
    line = 1,
    font = 2,
    cex = 1.25
  )
}

save_figure(
  "Figure_4_hBCS_category_distribution",
  10,
  6,
  draw_figure_4
)

# ============================================================
# Figure S1: sensitivity analyses
# ============================================================

sensitivity <- read_result(
  "sensitivity_model_summary.csv"
)

sensitivity_order <- c(
  "Primary_complete_9_domains",
  "Survey_weighted_complete_9_domains",
  "Available_score_7plus_domains",
  "Complete_8_domains_without_activity"
)

sensitivity_labels <- c(
  "Primary: complete 9 domains",
  "Survey weighted",
  "Available score: >=7 domains",
  "Complete 8 domains: no activity"
)

draw_sensitivity_panel <- function(
  cohort_name,
  transition_name
) {
  data <- sensitivity[
    sensitivity$cohort == cohort_name &
      sensitivity$transition == transition_name,
  ]

  data <- data[
    match(
      sensitivity_order,
      data$sensitivity_analysis
    ),
  ]

  y <- 4:1
  ticks <- c(
    0.45,
    0.55,
    0.65,
    0.75,
    0.90,
    1.00
  )

  plot(
    log(
      data$hazard_ratio_per_3_points
    ),
    y,
    type = "n",
    xlim = log(c(0.42, 1.03)),
    ylim = c(0.5, 4.5),
    xaxt = "n",
    yaxt = "n",
    xlab = "Hazard ratio per 3-point increase",
    ylab = "",
    main = paste0(
      cohort_name,
      ": ",
      transition_titles[transition_name]
    ),
    bty = "l"
  )

  axis(
    side = 1,
    at = log(ticks),
    labels = ticks
  )

  axis(
    side = 2,
    at = y,
    labels = sensitivity_labels,
    las = 1,
    cex.axis = 0.70
  )

  abline(
    v = log(1),
    lty = 2,
    col = "grey55"
  )

  arrows(
    x0 = log(data$lower_95),
    y0 = y,
    x1 = log(data$upper_95),
    y1 = y,
    angle = 90,
    code = 3,
    length = 0.04,
    col = cohort_colours[cohort_name],
    lwd = 1.5
  )

  points(
    log(
      data$hazard_ratio_per_3_points
    ),
    y,
    pch = 19,
    col = cohort_colours[cohort_name]
  )
}

draw_figure_s1 <- function() {
  five_panel_layout()

  par(
    mar = c(5, 11, 3, 1),
    oma = c(1, 1, 3, 1),
    family = "sans"
  )

  for (
    i in seq_len(
      nrow(panel_specification)
    )
  ) {
    draw_sensitivity_panel(
      panel_specification$cohort[i],
      panel_specification$transition[i]
    )
  }

  mtext(
    "Sensitivity analyses",
    side = 3,
    outer = TRUE,
    line = 1,
    font = 2,
    cex = 1.25
  )
}

save_figure(
  "Figure_S1_Sensitivity_forest",
  13,
  8,
  draw_figure_s1
)

# ============================================================
# Figures S2 and S3: component scores and missingness
# ============================================================

hrs_components <- read_result(
  "hrs_hbcs_component_summary.csv"
)

charls_components <- read_result(
  "charls_hbcs_component_summary.csv"
)

component_order <- c(
  "score_smoking",
  "score_alcohol",
  "score_activity",
  "score_bmi",
  "score_hypertension",
  "score_diabetes",
  "score_sleep",
  "score_mood",
  "score_social"
)

component_labels <- c(
  "Smoking",
  "Alcohol",
  "Physical activity",
  "BMI",
  "Hypertension",
  "Diabetes",
  "Sleep",
  "Mood",
  "Social connection"
)

prepare_components <- function(data) {
  data[
    match(
      component_order,
      data$component
    ),
  ]
}

hrs_components <- prepare_components(
  hrs_components
)

charls_components <- prepare_components(
  charls_components
)

draw_component_panel <- function(
  data,
  cohort_name
) {
  matrix_counts <- rbind(
    data$score_0,
    data$score_1,
    data$score_2
  )

  denominators <- colSums(
    matrix_counts,
    na.rm = TRUE
  )

  proportions <- sweep(
    matrix_counts,
    2,
    denominators,
    "/"
  )

  barplot(
    proportions,
    horiz = TRUE,
    names.arg = component_labels,
    las = 1,
    col = c(
      "#B2182B",
      "#FDB863",
      "#1B7837"
    ),
    border = NA,
    xlim = c(0, 1),
    xlab = "Observed intervals (%)",
    main = cohort_name,
    xaxt = "n"
  )

  axis(
    side = 1,
    at = seq(0, 1, 0.2),
    labels = paste0(
      seq(0, 100, 20),
      "%"
    )
  )

  legend(
    "bottomright",
    legend = c(
      "Poor (0)",
      "Intermediate (1)",
      "Ideal (2)"
    ),
    fill = c(
      "#B2182B",
      "#FDB863",
      "#1B7837"
    ),
    bty = "n",
    cex = 0.78
  )
}

draw_figure_s2 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(5, 10, 3, 1),
    oma = c(1, 1, 3, 1),
    family = "sans"
  )

  draw_component_panel(
    hrs_components,
    "HRS"
  )

  draw_component_panel(
    charls_components,
    "CHARLS"
  )

  mtext(
    "Distribution of adapted hBCS component scores",
    side = 3,
    outer = TRUE,
    line = 1,
    font = 2,
    cex = 1.25
  )
}

save_figure(
  "Figure_S2_Component_score_distribution",
  12,
  7,
  draw_figure_s2
)

draw_missingness_panel <- function(
  data,
  cohort_name
) {
  values <- data$missing_pct

  upper_limit <- max(
    values,
    na.rm = TRUE
  ) * 1.22

  positions <- barplot(
    values,
    horiz = TRUE,
    names.arg = component_labels,
    las = 1,
    col = cohort_colours[cohort_name],
    border = NA,
    xlim = c(0, upper_limit),
    xlab = "Missing observations (%)",
    main = cohort_name
  )

  text(
    x = values,
    y = positions,
    labels = paste0(
      sprintf("%.1f", values),
      "%"
    ),
    pos = 4,
    cex = 0.78
  )
}

draw_figure_s3 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(5, 10, 3, 1),
    oma = c(1, 1, 4, 1),
    family = "sans"
  )

  draw_missingness_panel(
    hrs_components,
    "HRS"
  )

  draw_missingness_panel(
    charls_components,
    "CHARLS"
  )

  mtext(
    "Missingness of adapted hBCS components",
    side = 3,
    outer = TRUE,
    line = 2,
    font = 2,
    cex = 1.25
  )

  mtext(
    paste(
      "CHARLS physical activity was administered",
      "to a subsample in early waves"
    ),
    side = 3,
    outer = TRUE,
    line = 0.5,
    cex = 0.85
  )
}

save_figure(
  "Figure_S3_Component_missingness",
  12,
  7,
  draw_figure_s3
)

cat(
  "\nAdditional publication figures completed:\n",
  " - Figure_1_Study_flow.png/.tiff\n",
  " - Figure_4_hBCS_category_distribution.png/.tiff\n",
  " - Figure_S1_Sensitivity_forest.png/.tiff\n",
  " - Figure_S2_Component_score_distribution.png/.tiff\n",
  " - Figure_S3_Component_missingness.png/.tiff\n",
  sep = ""
)

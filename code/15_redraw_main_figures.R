#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(survival)
  library(ggplot2)
  library(grid)
})

options(stringsAsFactors = FALSE)

results_dir <- "results"
derived_dir <- "data/derived"
figure_dir <- file.path(results_dir, "figures")

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# General helpers
# ============================================================

read_csv_base <- function(filename) {
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

fmt_p <- function(p) {
  if (is.na(p)) {
    return("NA")
  }

  if (p < 0.0001) {
    return("<0.0001")
  }

  if (p < 0.001) {
    return(sprintf("%.4f", p))
  }

  sprintf("%.3f", p)
}

find_n <- function(
  data,
  field,
  pattern
) {
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

open_png <- function(
  filename,
  width,
  height
) {
  png(
    filename = file.path(
      figure_dir,
      paste0(filename, ".png")
    ),
    width = width * 300,
    height = height * 300,
    res = 300,
    bg = "white"
  )
}

open_tiff <- function(
  filename,
  width,
  height
) {
  tiff(
    filename = file.path(
      figure_dir,
      paste0(filename, ".tiff")
    ),
    width = width * 600,
    height = height * 600,
    res = 600,
    compression = "lzw",
    bg = "white"
  )
}

save_grid_figure <- function(
  filename,
  width,
  height,
  drawing_function
) {
  open_png(filename, width, height)
  drawing_function()
  dev.off()

  open_tiff(filename, width, height)
  drawing_function()
  dev.off()
}

colours <- c(
  HRS = "#315F9E",
  CHARLS = "#C55A3D"
)

light_colours <- c(
  HRS = "#DCE7F3",
  CHARLS = "#F5E2DC"
)

transition_titles <- c(
  "0_to_1_stroke" = "Stroke-free to stroke",
  "0_to_2_death" = "Stroke-free to death",
  "1_to_2_death" = "Stroke to death"
)

# ============================================================
# Read existing results
# ============================================================

hrs_flow <- read_csv_base(
  "hrs_hbcs_analytic_flow.csv"
)

charls_flow <- read_csv_base(
  "charls_hbcs_analytic_flow.csv"
)

clean_flow <- read_csv_base(
  "analytic_flow_after_bmi_cleaning.csv"
)

primary <- read_csv_base(
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

model_row <- function(
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

  row[1, ]
}

# ============================================================
# Figure 1: styled study flow
# ============================================================

hrs_valid <- find_n(
  hrs_flow,
  "step",
  "Valid.*age 50"
)

charls_valid <- find_n(
  charls_flow,
  "step",
  "Valid.*age 50"
)

hrs_complete <- clean_count(
  "HRS",
  "Complete-score transitions"
)

charls_complete <- clean_count(
  "CHARLS",
  "Complete-score transitions"
)

hrs_incomplete <- hrs_valid - hrs_complete
charls_incomplete <- charls_valid - charls_complete

hrs_stroke <- model_row(
  "HRS",
  "0_to_1_stroke"
)

hrs_direct_death <- model_row(
  "HRS",
  "0_to_2_death"
)

hrs_poststroke_death <- model_row(
  "HRS",
  "1_to_2_death"
)

charls_stroke <- model_row(
  "CHARLS",
  "0_to_1_stroke"
)

charls_direct_death <- model_row(
  "CHARLS",
  "0_to_2_death"
)

draw_box <- function(
  x,
  y,
  width,
  height,
  label,
  fill,
  border = "#C7C7C7",
  text_colour = "#111111",
  fontsize = 10,
  fontface = "plain"
) {
  grid.roundrect(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(width, "npc"),
    height = unit(height, "npc"),
    r = unit(0.006, "npc"),
    gp = gpar(
      fill = fill,
      col = border,
      lwd = 1.2
    )
  )

  grid.text(
    label,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    gp = gpar(
      col = text_colour,
      fontsize = fontsize,
      fontface = fontface,
      fontfamily = "sans"
    )
  )
}

draw_arrow <- function(
  x0,
  y0,
  x1,
  y1
) {
  grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y0, y1), "npc"),
    arrow = arrow(
      type = "closed",
      length = unit(0.075, "inches")
    ),
    gp = gpar(
      col = "#B5B5B5",
      lwd = 1.5
    )
  )
}

draw_stage_band <- function(
  y,
  height,
  label,
  fill
) {
  grid.rect(
    x = unit(0.045, "npc"),
    y = unit(y, "npc"),
    width = unit(0.06, "npc"),
    height = unit(height, "npc"),
    gp = gpar(
      fill = fill,
      col = NA
    )
  )

  grid.text(
    label,
    x = unit(0.045, "npc"),
    y = unit(y, "npc"),
    rot = 90,
    gp = gpar(
      col = "white",
      fontsize = 11,
      fontface = "bold",
      fontfamily = "sans"
    )
  )
}

draw_flow_figure <- function() {
  grid.newpage()

  grid.text(
    "Study flow and construction of transition-specific analytic samples",
    x = unit(0.52, "npc"),
    y = unit(0.975, "npc"),
    gp = gpar(
      fontsize = 17,
      fontface = "bold",
      fontfamily = "sans"
    )
  )

  draw_stage_band(
    0.855, 0.17,
    "Source cohorts",
    "#CDD9E8"
  )

  draw_stage_band(
    0.665, 0.17,
    "Person-waves",
    "#AFC4DF"
  )

  draw_stage_band(
    0.425, 0.27,
    "Eligibility",
    "#87A6D2"
  )

  draw_stage_band(
    0.145, 0.25,
    "Analytic samples",
    "#3E609B"
  )

  # Column headings
  draw_box(
    0.275, 0.875,
    0.25, 0.105,
    "HRS\nN = 45,234 persons",
    "#F5F7F9",
    border = colours["HRS"],
    fontsize = 12,
    fontface = "bold"
  )

  draw_box(
    0.675, 0.875,
    0.25, 0.105,
    "CHARLS\nN = 25,586 persons",
    "#F5F7F9",
    border = colours["CHARLS"],
    fontsize = 12,
    fontface = "bold"
  )

  # Interviewed person-waves
  draw_arrow(
    0.275, 0.822,
    0.275, 0.745
  )

  draw_arrow(
    0.675, 0.822,
    0.675, 0.745
  )

  draw_box(
    0.275, 0.690,
    0.25, 0.105,
    "Interviewed and alive\nperson-waves\nN = 166,658",
    light_colours["HRS"],
    fontsize = 10
  )

  draw_box(
    0.675, 0.690,
    0.25, 0.105,
    "Interviewed and alive\nperson-waves\nN = 77,233",
    light_colours["CHARLS"],
    fontsize = 10
  )

  # Age-eligible transitions
  draw_arrow(
    0.275, 0.637,
    0.275, 0.565
  )

  draw_arrow(
    0.675, 0.637,
    0.675, 0.565
  )

  draw_box(
    0.275, 0.515,
    0.25, 0.095,
    paste0(
      "Valid transitions with\nbaseline age 50+\nN = ",
      fmt_n(hrs_valid)
    ),
    light_colours["HRS"],
    fontsize = 9.8
  )

  draw_box(
    0.675, 0.515,
    0.25, 0.095,
    paste0(
      "Valid transitions with\nbaseline age 50+\nN = ",
      fmt_n(charls_valid)
    ),
    light_colours["CHARLS"],
    fontsize = 9.8
  )

  # Complete hBCS
  draw_arrow(
    0.275, 0.467,
    0.275, 0.390
  )

  draw_arrow(
    0.675, 0.467,
    0.675, 0.390
  )

  draw_box(
    0.275, 0.340,
    0.25, 0.095,
    paste0(
      "Complete nine-domain hBCS\ntransitions\nN = ",
      fmt_n(hrs_complete)
    ),
    "#BDD0E7",
    fontsize = 9.8
  )

  draw_box(
    0.675, 0.340,
    0.25, 0.095,
    paste0(
      "Complete nine-domain hBCS\ntransitions\nN = ",
      fmt_n(charls_complete)
    ),
    "#EDC9BE",
    fontsize = 9.8
  )

  # Missing-score exclusions
  draw_arrow(
    0.400, 0.427,
    0.475, 0.427
  )

  draw_box(
    0.515, 0.427,
    0.19, 0.078,
    paste0(
      "Incomplete hBCS\nN = ",
      fmt_n(hrs_incomplete)
    ),
    "#F2F2F2",
    fontsize = 8.6
  )

  draw_arrow(
    0.800, 0.427,
    0.855, 0.427
  )

  draw_box(
    0.895, 0.427,
    0.17, 0.078,
    paste0(
      "Incomplete hBCS\nN = ",
      fmt_n(charls_incomplete)
    ),
    "#F2F2F2",
    fontsize = 8.3
  )

  # Final model samples
  draw_arrow(
    0.275, 0.292,
    0.275, 0.238
  )

  draw_arrow(
    0.675, 0.292,
    0.675, 0.238
  )

  hrs_final_text <- paste0(
    "Fully adjusted HRS models\n",
    "0 to 1: ",
    fmt_n(hrs_stroke$model_intervals),
    " intervals; ",
    fmt_n(hrs_stroke$model_events),
    " events\n",
    "0 to 2: ",
    fmt_n(hrs_direct_death$model_intervals),
    " intervals; ",
    fmt_n(hrs_direct_death$model_events),
    " events\n",
    "1 to 2: ",
    fmt_n(hrs_poststroke_death$model_intervals),
    " intervals; ",
    fmt_n(hrs_poststroke_death$model_events),
    " events"
  )

  charls_final_text <- paste0(
    "External-validation models\n",
    "0 to 1: ",
    fmt_n(charls_stroke$model_intervals),
    " intervals; ",
    fmt_n(charls_stroke$model_events),
    " events\n",
    "0 to 2: ",
    fmt_n(charls_direct_death$model_intervals),
    " intervals; ",
    fmt_n(charls_direct_death$model_events),
    " events"
  )

  draw_box(
    0.275, 0.150,
    0.30, 0.165,
    hrs_final_text,
    colours["HRS"],
    border = colours["HRS"],
    text_colour = "white",
    fontsize = 9.2,
    fontface = "bold"
  )

  draw_box(
    0.675, 0.150,
    0.30, 0.165,
    charls_final_text,
    colours["CHARLS"],
    border = colours["CHARLS"],
    text_colour = "white",
    fontsize = 9.2,
    fontface = "bold"
  )

  grid.text(
    paste(
      "Counts refer to persons, person-waves, or transition intervals",
      "as explicitly indicated."
    ),
    x = unit(0.52, "npc"),
    y = unit(0.025, "npc"),
    gp = gpar(
      fontsize = 8.5,
      col = "#555555",
      fontfamily = "sans"
    )
  )
}

save_grid_figure(
  "Figure_1_Study_flow_styled",
  13,
  8,
  draw_flow_figure
)

# ============================================================
# Figure 2: table-style primary forest plot
# ============================================================

forest_order <- data.frame(
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

forest <- do.call(
  rbind,
  lapply(
    seq_len(nrow(forest_order)),
    function(i) {
      model_row(
        forest_order$cohort[i],
        forest_order$transition[i]
      )
    }
  )
)

forest$transition_label <-
  transition_titles[forest$transition]

forest$hr_text <- sprintf(
  "%.2f (%.2f-%.2f)",
  forest$hazard_ratio,
  forest$lower_95,
  forest$upper_95
)

forest$p_text <- vapply(
  forest$p_value,
  fmt_p,
  character(1)
)

log_min <- log(0.45)
log_max <- log(1.03)

map_hr <- function(value) {
  0.72 +
    (
      log(value) - log_min
    ) /
    (
      log_max - log_min
    ) * 0.19
}

draw_forest_row <- function(
  row,
  y
) {
  cohort_name <- row$cohort
  point_colour <- colours[cohort_name]

  grid.text(
    row$transition_label,
    x = unit(0.020, "npc"),
    y = unit(y, "npc"),
    just = "left",
    gp = gpar(
      fontsize = 10.5,
      fontfamily = "sans"
    )
  )

  grid.text(
    fmt_n(row$model_intervals),
    x = unit(0.345, "npc"),
    y = unit(y, "npc"),
    gp = gpar(
      fontsize = 10.5,
      fontfamily = "sans"
    )
  )

  grid.text(
    fmt_n(row$model_events),
    x = unit(0.445, "npc"),
    y = unit(y, "npc"),
    gp = gpar(
      fontsize = 10.5,
      fontfamily = "sans"
    )
  )

  grid.text(
    row$hr_text,
    x = unit(0.575, "npc"),
    y = unit(y, "npc"),
    gp = gpar(
      fontsize = 10.5,
      fontfamily = "sans"
    )
  )

  grid.lines(
    x = unit(
      c(
        map_hr(row$lower_95),
        map_hr(row$upper_95)
      ),
      "npc"
    ),
    y = unit(c(y, y), "npc"),
    gp = gpar(
      col = "#222222",
      lwd = 2
    )
  )

  grid.lines(
    x = unit(
      rep(map_hr(row$lower_95), 2),
      "npc"
    ),
    y = unit(
      c(y - 0.012, y + 0.012),
      "npc"
    ),
    gp = gpar(
      col = "#222222",
      lwd = 1.5
    )
  )

  grid.lines(
    x = unit(
      rep(map_hr(row$upper_95), 2),
      "npc"
    ),
    y = unit(
      c(y - 0.012, y + 0.012),
      "npc"
    ),
    gp = gpar(
      col = "#222222",
      lwd = 1.5
    )
  )

  grid.points(
    x = unit(
      map_hr(row$hazard_ratio),
      "npc"
    ),
    y = unit(y, "npc"),
    pch = 18,
    size = unit(4, "mm"),
    gp = gpar(
      col = point_colour,
      fill = point_colour
    )
  )

  grid.text(
    row$p_text,
    x = unit(0.965, "npc"),
    y = unit(y, "npc"),
    gp = gpar(
      fontsize = 10.5,
      fontfamily = "sans"
    )
  )
}

draw_forest_figure <- function() {
  grid.newpage()

  # Top and header rules
  grid.lines(
    x = unit(c(0.01, 0.99), "npc"),
    y = unit(c(0.965, 0.965), "npc"),
    gp = gpar(lwd = 2)
  )

  grid.text(
    "Transition",
    x = unit(0.02, "npc"),
    y = unit(0.915, "npc"),
    just = "left",
    gp = gpar(
      fontsize = 12,
      fontface = "bold"
    )
  )

  grid.text(
    "Intervals",
    x = unit(0.345, "npc"),
    y = unit(0.915, "npc"),
    gp = gpar(
      fontsize = 12,
      fontface = "bold"
    )
  )

  grid.text(
    "Events",
    x = unit(0.445, "npc"),
    y = unit(0.915, "npc"),
    gp = gpar(
      fontsize = 12,
      fontface = "bold"
    )
  )

  grid.text(
    "HR per 3 points (95% CI)",
    x = unit(0.575, "npc"),
    y = unit(0.915, "npc"),
    gp = gpar(
      fontsize = 12,
      fontface = "bold"
    )
  )

  grid.text(
    "P value",
    x = unit(0.965, "npc"),
    y = unit(0.915, "npc"),
    gp = gpar(
      fontsize = 12,
      fontface = "bold"
    )
  )

  grid.lines(
    x = unit(c(0.01, 0.99), "npc"),
    y = unit(c(0.875, 0.875), "npc"),
    gp = gpar(lwd = 1.2)
  )

  # HR=1 reference
  grid.lines(
    x = unit(
      rep(map_hr(1), 2),
      "npc"
    ),
    y = unit(c(0.16, 0.85), "npc"),
    gp = gpar(
      col = "#A8A8A8",
      lwd = 1,
      lty = 2
    )
  )

  # HRS group
  grid.text(
    "HRS",
    x = unit(0.02, "npc"),
    y = unit(0.825, "npc"),
    just = "left",
    gp = gpar(
      fontsize = 12,
      fontface = "bold",
      col = colours["HRS"]
    )
  )

  draw_forest_row(
    forest[1, ],
    0.745
  )

  draw_forest_row(
    forest[2, ],
    0.655
  )

  draw_forest_row(
    forest[3, ],
    0.565
  )

  # CHARLS group
  grid.text(
    "CHARLS",
    x = unit(0.02, "npc"),
    y = unit(0.475, "npc"),
    just = "left",
    gp = gpar(
      fontsize = 12,
      fontface = "bold",
      col = colours["CHARLS"]
    )
  )

  draw_forest_row(
    forest[4, ],
    0.395
  )

  draw_forest_row(
    forest[5, ],
    0.305
  )

  # Forest axis
  axis_ticks <- c(
    0.5,
    0.6,
    0.7,
    0.8,
    0.9,
    1.0
  )

  grid.lines(
    x = unit(
      c(
        map_hr(0.45),
        map_hr(1.03)
      ),
      "npc"
    ),
    y = unit(c(0.19, 0.19), "npc"),
    gp = gpar(lwd = 1.1)
  )

  for (tick in axis_ticks) {
    tick_x <- map_hr(tick)

    grid.lines(
      x = unit(c(tick_x, tick_x), "npc"),
      y = unit(c(0.18, 0.20), "npc"),
      gp = gpar(lwd = 1)
    )

    grid.text(
      sprintf("%.1f", tick),
      x = unit(tick_x, "npc"),
      y = unit(0.15, "npc"),
      gp = gpar(fontsize = 9.5)
    )
  }

  grid.text(
    "Hazard ratio per 3-point higher adapted hBCS",
    x = unit(0.815, "npc"),
    y = unit(0.095, "npc"),
    gp = gpar(
      fontsize = 11,
      fontfamily = "sans"
    )
  )

  grid.text(
    "Fully adjusted transition-specific Cox models",
    x = unit(0.50, "npc"),
    y = unit(0.040, "npc"),
    gp = gpar(
      fontsize = 9,
      col = "#555555",
      fontfamily = "sans"
    )
  )

  grid.lines(
    x = unit(c(0.01, 0.99), "npc"),
    y = unit(c(0.015, 0.015), "npc"),
    gp = gpar(lwd = 2)
  )
}

save_grid_figure(
  "Figure_2_Primary_forest_plot_styled",
  14,
  8,
  draw_forest_figure
)

# ============================================================
# Figure 3: restricted cubic spline dose-response
# ============================================================

hrs <- readRDS(
  file.path(
    derived_dir,
    "hrs_multistate_analytic.rds"
  )
)

charls <- readRDS(
  file.path(
    derived_dir,
    "charls_multistate_analytic.rds"
  )
)

# Restricted cubic spline basis with four knots.
# The first column is the linear term; remaining columns
# represent nonlinear terms.

rcs_basis <- function(
  x,
  knots
) {
  knots <- sort(
    as.numeric(knots)
  )

  if (length(knots) != 4) {
    stop("Exactly four unique knots are required.")
  }

  positive_cube <- function(z) {
    pmax(z, 0)^3
  }

  last <- knots[4]
  penultimate <- knots[3]
  scale_value <- (
    last - knots[1]
  )^2

  nonlinear_1 <- (
    positive_cube(
      x - knots[1]
    ) -
      positive_cube(
        x - penultimate
      ) *
      (
        last - knots[1]
      ) /
      (
        last - penultimate
      ) +
      positive_cube(
        x - last
      ) *
      (
        penultimate - knots[1]
      ) /
      (
        last - penultimate
      )
  ) / scale_value

  nonlinear_2 <- (
    positive_cube(
      x - knots[2]
    ) -
      positive_cube(
        x - penultimate
      ) *
      (
        last - knots[2]
      ) /
      (
        last - penultimate
      ) +
      positive_cube(
        x - last
      ) *
      (
        penultimate - knots[2]
      ) /
      (
        last - penultimate
      )
  ) / scale_value

  output <- cbind(
    rcs_linear = x,
    rcs_nl1 = nonlinear_1,
    rcs_nl2 = nonlinear_2
  )

  output
}

wald_p <- function(
  fit,
  coefficient_names
) {
  beta <- coef(fit)[
    coefficient_names
  ]

  variance <- vcov(fit)[
    coefficient_names,
    coefficient_names,
    drop = FALSE
  ]

  statistic <- as.numeric(
    t(beta) %*%
      qr.solve(
        variance,
        beta
      )
  )

  pchisq(
    statistic,
    df = length(beta),
    lower.tail = FALSE
  )
}

prepare_transition <- function(
  data,
  cohort_name,
  origin_state,
  target_transition
) {
  analysis <- data[
    data$state == origin_state,
  ]

  analysis$event <- as.integer(
    analysis$transition ==
      target_transition
  )

  analysis$tstart <- 0
  analysis$tstop <-
    as.numeric(
      analysis$interval_years
    )

  analysis$id_cluster <- if (
    cohort_name == "HRS"
  ) {
    analysis$hhidpn
  } else {
    analysis$ID
  }

  analysis <- analysis[
    !is.na(
      analysis$adapted_hbcs_18
    ) &
      !is.na(
        analysis$tstop
      ) &
      analysis$tstop > 0,
  ]

  analysis
}

fit_rcs_model <- function(
  data,
  cohort_name,
  transition_name,
  origin_state,
  reference_score = 12
) {
  analysis <- prepare_transition(
    data,
    cohort_name,
    origin_state,
    transition_name
  )

  score <- analysis$adapted_hbcs_18

  knots <- as.numeric(
    quantile(
      score,
      probs = c(
        0.05,
        0.35,
        0.65,
        0.95
      ),
      na.rm = TRUE,
      names = FALSE,
      type = 2
    )
  )

  if (length(unique(knots)) != 4) {
    stop(
      "Non-unique spline knots for ",
      cohort_name,
      ": ",
      transition_name
    )
  }

  basis <- rcs_basis(
    score,
    knots
  )

  analysis$rcs_linear <-
    basis[, "rcs_linear"]

  analysis$rcs_nl1 <-
    basis[, "rcs_nl1"]

  analysis$rcs_nl2 <-
    basis[, "rcs_nl2"]

  covariates <- if (
    cohort_name == "HRS"
  ) {
    c(
      "age",
      "factor(sex)",
      "education_years",
      "factor(race)",
      "factor(ethnicity)"
    )
  } else {
    c(
      "age",
      "factor(sex)",
      "factor(education)"
    )
  }

  formula_text <- paste(
    "Surv(tstart, tstop, event) ~",
    paste(
      c(
        "rcs_linear",
        "rcs_nl1",
        "rcs_nl2",
        covariates,
        "strata(wave)",
        "cluster(id_cluster)"
      ),
      collapse = " + "
    )
  )

  fit <- coxph(
    as.formula(formula_text),
    data = analysis,
    ties = "efron",
    na.action = na.omit,
    x = TRUE
  )

  basis_names <- c(
    "rcs_linear",
    "rcs_nl1",
    "rcs_nl2"
  )

  p_overall <- wald_p(
    fit,
    basis_names
  )

  p_nonlinear <- wald_p(
    fit,
    c(
      "rcs_nl1",
      "rcs_nl2"
    )
  )

  score_limits <- as.numeric(
    quantile(
      score,
      probs = c(
        0.025,
        0.975
      ),
      na.rm = TRUE,
      names = FALSE,
      type = 2
    )
  )

  grid_score <- seq(
    score_limits[1],
    score_limits[2],
    length.out = 250
  )

  if (
    reference_score < score_limits[1] ||
      reference_score > score_limits[2]
  ) {
    reference_score <- median(
      score,
      na.rm = TRUE
    )
  }

  grid_basis <- rcs_basis(
    grid_score,
    knots
  )

  reference_basis <- rcs_basis(
    reference_score,
    knots
  )

  contrasts <- sweep(
    grid_basis,
    2,
    reference_basis[1, ],
    "-"
  )

  beta <- coef(fit)[
    basis_names
  ]

  variance <- vcov(fit)[
    basis_names,
    basis_names,
    drop = FALSE
  ]

  log_hr <- as.numeric(
    contrasts %*% beta
  )

  standard_error <- sqrt(
    rowSums(
      (
        contrasts %*% variance
      ) * contrasts
    )
  )

  curve <- data.frame(
    cohort = cohort_name,
    transition = transition_name,
    score = grid_score,
    hazard_ratio = exp(log_hr),
    lower_95 = exp(
      log_hr -
        1.96 * standard_error
    ),
    upper_95 = exp(
      log_hr +
        1.96 * standard_error
    ),
    reference_score = reference_score,
    p_overall = p_overall,
    p_nonlinear = p_nonlinear,
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    cohort = cohort_name,
    transition = transition_name,
    model_intervals = fit$n,
    model_events = fit$nevent,
    reference_score = reference_score,
    knot_1 = knots[1],
    knot_2 = knots[2],
    knot_3 = knots[3],
    knot_4 = knots[4],
    p_overall = p_overall,
    p_nonlinear = p_nonlinear,
    stringsAsFactors = FALSE
  )

  list(
    curve = curve,
    summary = summary
  )
}

rcs_specification <- data.frame(
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
  origin_state = c(
    0, 0, 1,
    0, 0
  ),
  stringsAsFactors = FALSE
)

rcs_results <- vector(
  "list",
  nrow(rcs_specification)
)

for (
  i in seq_len(
    nrow(rcs_specification)
  )
) {
  cohort_name <-
    rcs_specification$cohort[i]

  transition_name <-
    rcs_specification$transition[i]

  message(
    "Fitting RCS: ",
    cohort_name,
    " ",
    transition_name
  )

  cohort_data <- if (
    cohort_name == "HRS"
  ) {
    hrs
  } else {
    charls
  }

  rcs_results[[i]] <- fit_rcs_model(
    data = cohort_data,
    cohort_name = cohort_name,
    transition_name = transition_name,
    origin_state =
      rcs_specification$origin_state[i],
    reference_score = 12
  )
}

rcs_curves <- do.call(
  rbind,
  lapply(
    rcs_results,
    function(x) {
      x$curve
    }
  )
)

rcs_summary <- do.call(
  rbind,
  lapply(
    rcs_results,
    function(x) {
      x$summary
    }
  )
)

write.csv(
  rcs_curves,
  file.path(
    results_dir,
    "rcs_curve_predictions.csv"
  ),
  row.names = FALSE
)

write.csv(
  rcs_summary,
  file.path(
    results_dir,
    "rcs_model_summary.csv"
  ),
  row.names = FALSE
)

make_rcs_panel <- function(
  cohort_name,
  transition_name
) {
  data <- rcs_curves[
    rcs_curves$cohort == cohort_name &
      rcs_curves$transition ==
      transition_name,
  ]

  p_label <- paste0(
    "P-overall ",
    fmt_p(data$p_overall[1]),
    "\nP-nonlinear ",
    fmt_p(data$p_nonlinear[1])
  )

  ggplot(
    data,
    aes(
      x = score,
      y = hazard_ratio
    )
  ) +
    geom_ribbon(
      aes(
        ymin = lower_95,
        ymax = upper_95
      ),
      fill = light_colours[cohort_name],
      alpha = 0.85
    ) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "#777777"
    ) +
    geom_vline(
      xintercept =
        data$reference_score[1],
      linetype = "dotted",
      linewidth = 0.45,
      colour = "#888888"
    ) +
    geom_line(
      colour = colours[cohort_name],
      linewidth = 1.05
    ) +
    annotate(
      "label",
      x = -Inf,
      y = Inf,
      label = p_label,
      hjust = -0.05,
      vjust = 1.15,
      size = 3.1,
      label.size = 0,
      fill = "white"
    ) +
    scale_y_log10() +
    labs(
      title = paste0(
        cohort_name,
        ": ",
        transition_titles[transition_name]
      ),
      x = "Adapted hBCS",
      y = "Hazard ratio (95% CI)"
    ) +
    theme_classic(
      base_size = 10.5
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5
      ),
      axis.title = element_text(
        face = "plain"
      ),
      axis.text = element_text(
        colour = "#222222"
      ),
      panel.grid.major.y = element_line(
        colour = "#E7E7E7",
        linewidth = 0.35
      ),
      plot.margin = margin(
        8, 10, 8, 8
      )
    )
}

rcs_plots <- lapply(
  seq_len(
    nrow(rcs_specification)
  ),
  function(i) {
    make_rcs_panel(
      rcs_specification$cohort[i],
      rcs_specification$transition[i]
    )
  }
)

draw_rcs_figure <- function() {
  grid.newpage()

  layout_definition <- grid.layout(
    nrow = 3,
    ncol = 6,
    heights = unit(
      c(0.08, 0.46, 0.46),
      "npc"
    )
  )

  pushViewport(
    viewport(
      layout = layout_definition
    )
  )

  grid.text(
    "Dose-response associations of continuous adapted hBCS",
    vp = viewport(
      layout.pos.row = 1,
      layout.pos.col = 1:6
    ),
    gp = gpar(
      fontsize = 16,
      fontface = "bold",
      fontfamily = "sans"
    )
  )

  print(
    rcs_plots[[1]],
    vp = viewport(
      layout.pos.row = 2,
      layout.pos.col = 1:2
    )
  )

  print(
    rcs_plots[[2]],
    vp = viewport(
      layout.pos.row = 2,
      layout.pos.col = 3:4
    )
  )

  print(
    rcs_plots[[3]],
    vp = viewport(
      layout.pos.row = 2,
      layout.pos.col = 5:6
    )
  )

  print(
    rcs_plots[[4]],
    vp = viewport(
      layout.pos.row = 3,
      layout.pos.col = 1:3
    )
  )

  print(
    rcs_plots[[5]],
    vp = viewport(
      layout.pos.row = 3,
      layout.pos.col = 4:6
    )
  )

  popViewport()
}

save_grid_figure(
  "Figure_3_RCS_dose_response_styled",
  13,
  8,
  draw_rcs_figure
)

cat(
  "\nStyled main figures completed:\n",
  " - Figure_1_Study_flow_styled.png/.tiff\n",
  " - Figure_2_Primary_forest_plot_styled.png/.tiff\n",
  " - Figure_3_RCS_dose_response_styled.png/.tiff\n",
  "\nRCS outputs:\n",
  " - results/rcs_curve_predictions.csv\n",
  " - results/rcs_model_summary.csv\n",
  sep = ""
)

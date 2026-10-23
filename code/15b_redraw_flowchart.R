#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(grid)
})

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

fmt_n <- function(x) {
  format(
    round(as.numeric(x)),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

preliminary_hrs <- read_result(
  "hrs_sample_flow_preliminary.csv"
)

preliminary_charls <- read_result(
  "charls_sample_flow_preliminary.csv"
)

score_flow_hrs <- read_result(
  "hrs_hbcs_analytic_flow.csv"
)

score_flow_charls <- read_result(
  "charls_hbcs_analytic_flow.csv"
)

clean_flow <- read_result(
  "analytic_flow_after_bmi_cleaning.csv"
)

primary <- read_result(
  "primary_transition_model_summary.csv"
)

primary <- primary[
  primary$model == "Model_3",
]

clean_n <- function(
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

model_n <- function(
  cohort_name,
  transition_name,
  field = "model_intervals"
) {
  hit <- primary$cohort == cohort_name &
    primary$transition == transition_name

  if (!any(hit)) {
    return(NA_real_)
  }

  as.numeric(primary[[field]][which(hit)[1]])
}

make_flow_data <- function(
  cohort_name,
  preliminary,
  score_flow
) {
  persons <- find_n(
    preliminary,
    "step",
    "persons"
  )

  person_waves <- find_n(
    preliminary,
    "step",
    "Person-wave rows"
  )

  interviewed <- find_n(
    preliminary,
    "step",
    "Interviewed and alive person-waves"
  )

  valid_adjacent <- find_n(
    preliminary,
    "step",
    "Valid adjacent-wave transitions"
  )

  age50 <- find_n(
    score_flow,
    "step",
    "Valid.*baseline age 50"
  )

  complete <- clean_n(
    cohort_name,
    "Complete-score transitions"
  )

  stroke_free <- clean_n(
    cohort_name,
    "Stroke-free transitions"
  )

  poststroke <- complete - stroke_free

  model_state0 <- model_n(
    cohort_name,
    "0_to_1_stroke"
  )

  model_state1 <- model_n(
    cohort_name,
    "1_to_2_death"
  )

  poststroke_deaths <- find_n(
    score_flow,
    "step",
    "Post-stroke death events"
  )

  list(
    persons = persons,
    person_waves = person_waves,
    interviewed = interviewed,
    valid_adjacent = valid_adjacent,
    age50 = age50,
    complete = complete,
    stroke_free = stroke_free,
    poststroke = poststroke,
    model_state0 = model_state0,
    model_state1 = model_state1,
    poststroke_deaths = poststroke_deaths,

    excluded_not_interviewed =
      person_waves - interviewed,

    excluded_no_adjacent =
      interviewed - valid_adjacent,

    excluded_age =
      valid_adjacent - age50,

    excluded_incomplete =
      age50 - complete,

    excluded_model_state0 =
      stroke_free - model_state0,

    excluded_model_state1 =
      ifelse(
        is.na(model_state1),
        NA_real_,
        poststroke - model_state1
      )
  )
}

hrs <- make_flow_data(
  "HRS",
  preliminary_hrs,
  score_flow_hrs
)

charls <- make_flow_data(
  "CHARLS",
  preliminary_charls,
  score_flow_charls
)

colours <- c(
  HRS = "#315F9E",
  CHARLS = "#C55A3D"
)

light_colours <- c(
  HRS = "#DCE7F3",
  CHARLS = "#F5E2DC"
)

middle_colours <- c(
  HRS = "#B8CEE7",
  CHARLS = "#EDC7BB"
)

draw_box <- function(
  x,
  y,
  width,
  height,
  label,
  fill,
  border = "#BBBBBB",
  text_colour = "#111111",
  fontsize = 8.5,
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

draw_vertical_arrow <- function(
  x,
  y0,
  y1
) {
  grid.lines(
    x = unit(c(x, x), "npc"),
    y = unit(c(y0, y1), "npc"),
    arrow = arrow(
      type = "closed",
      length = unit(0.065, "inches")
    ),
    gp = gpar(
      col = "#B1B1B1",
      lwd = 1.4
    )
  )
}

draw_exclusion_arrow <- function(
  x0,
  y,
  x1
) {
  grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y, y), "npc"),
    arrow = arrow(
      type = "closed",
      length = unit(0.06, "inches")
    ),
    gp = gpar(
      col = "#B1B1B1",
      lwd = 1.35
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
    x = unit(0.04, "npc"),
    y = unit(y, "npc"),
    width = unit(0.055, "npc"),
    height = unit(height, "npc"),
    gp = gpar(
      fill = fill,
      col = NA
    )
  )

  grid.text(
    label,
    x = unit(0.04, "npc"),
    y = unit(y, "npc"),
    rot = 90,
    gp = gpar(
      col = "white",
      fontsize = 10.5,
      fontface = "bold",
      fontfamily = "sans"
    )
  )
}

draw_cohort_panel <- function(
  cohort_name,
  data
) {
  main_x <- 0.27
  exclusion_x <- 0.76

  main_width <- 0.46
  exclusion_width <- 0.39

  y <- c(
    0.94,
    0.80,
    0.66,
    0.52,
    0.38,
    0.24,
    0.075
  )

  main_height <- c(
    0.095,
    0.085,
    0.085,
    0.085,
    0.085,
    0.085,
    0.115
  )

  main_labels <- c(
    paste0(
      cohort_name,
      " source cohort\n",
      "N = ",
      fmt_n(data$persons),
      " persons"
    ),

    paste0(
      "Constructed person-wave rows\n",
      "N = ",
      fmt_n(data$person_waves)
    ),

    paste0(
      "Interviewed and alive\n",
      "N = ",
      fmt_n(data$interviewed),
      " person-waves"
    ),

    paste0(
      "Valid adjacent-wave transitions\n",
      "N = ",
      fmt_n(data$valid_adjacent)
    ),

    paste0(
      "Baseline age 50+ transitions\n",
      "N = ",
      fmt_n(data$age50)
    ),

    paste0(
      "Complete nine-domain hBCS\n",
      "N = ",
      fmt_n(data$complete),
      " transitions"
    ),

    if (cohort_name == "HRS") {
      paste0(
        "Fully adjusted analytic samples\n",
        "State 0: N = ",
        fmt_n(data$model_state0),
        " intervals\n",
        "State 1: N = ",
        fmt_n(data$model_state1),
        " intervals"
      )
    } else {
      paste0(
        "External-validation analytic sample\n",
        "State 0: N = ",
        fmt_n(data$model_state0),
        " intervals"
      )
    }
  )

  for (i in seq_along(y)) {
    if (i == 1) {
      fill_colour <- "#F4F6F8"
    } else if (i == 6) {
      fill_colour <- middle_colours[cohort_name]
    } else if (i == 7) {
      fill_colour <- colours[cohort_name]
    } else {
      fill_colour <- light_colours[cohort_name]
    }

    draw_box(
      main_x,
      y[i],
      main_width,
      main_height[i],
      main_labels[i],
      fill_colour,
      border = ifelse(
        i == 1,
        colours[cohort_name],
        "#BBBBBB"
      ),
      text_colour = ifelse(
        i == 7,
        "white",
        "#111111"
      ),
      fontsize = ifelse(
        i == 7,
        8.1,
        8.5
      ),
      fontface = ifelse(
        i %in% c(1, 7),
        "bold",
        "plain"
      )
    )

    if (i < length(y)) {
      draw_vertical_arrow(
        main_x,
        y[i] - main_height[i] / 2,
        y[i + 1] +
          main_height[i + 1] / 2
      )
    }
  }

  exclusion_y <- c(
    0.73,
    0.59,
    0.45,
    0.31,
    0.145
  )

  exclusion_labels <- c(
    paste0(
      "Not interviewed/alive\n",
      "N = ",
      fmt_n(
        data$excluded_not_interviewed
      ),
      " person-waves"
    ),

    paste0(
      "No valid adjacent-wave transition\n",
      "N = ",
      fmt_n(
        data$excluded_no_adjacent
      )
    ),

    paste0(
      "Baseline age under 50 or missing\n",
      "N = ",
      fmt_n(
        data$excluded_age
      ),
      " intervals"
    ),

    paste0(
      "Incomplete nine-domain hBCS\n",
      "N = ",
      fmt_n(
        data$excluded_incomplete
      ),
      " intervals"
    ),

    if (cohort_name == "HRS") {
      paste0(
        "Missing Model 3 covariates\n",
        "State 0: N = ",
        fmt_n(
          data$excluded_model_state0
        ),
        "\nState 1: N = ",
        fmt_n(
          data$excluded_model_state1
        )
      )
    } else {
      paste0(
        "Post-stroke branch not modeled\n",
        "N = ",
        fmt_n(data$poststroke),
        " intervals; ",
        fmt_n(data$poststroke_deaths),
        " deaths"
      )
    }
  )

  for (i in seq_along(exclusion_y)) {
    draw_exclusion_arrow(
      main_x,
      exclusion_y[i],
      exclusion_x -
        exclusion_width / 2
    )

    draw_box(
      exclusion_x,
      exclusion_y[i],
      exclusion_width,
      ifelse(i == 5, 0.085, 0.075),
      exclusion_labels[i],
      "#F2F2F2",
      border = "#C8C8C8",
      fontsize = ifelse(
        i == 5,
        7.6,
        7.8
      )
    )
  }
}

draw_flowchart <- function() {
  grid.newpage()

  grid.text(
    "Study flow and construction of transition-specific analytic samples",
    x = unit(0.52, "npc"),
    y = unit(0.976, "npc"),
    gp = gpar(
      fontsize = 16,
      fontface = "bold",
      fontfamily = "sans"
    )
  )

  draw_stage_band(
    0.88,
    0.105,
    "Source cohort",
    "#CBD7E7"
  )

  draw_stage_band(
    0.715,
    0.19,
    "Person-waves",
    "#AFC3DE"
  )

  draw_stage_band(
    0.435,
    0.34,
    "Eligibility",
    "#85A4D0"
  )

  draw_stage_band(
    0.13,
    0.20,
    "Analytic sample",
    "#405F9A"
  )

  pushViewport(
    viewport(
      x = unit(0.295, "npc"),
      y = unit(0.485, "npc"),
      width = unit(0.42, "npc"),
      height = unit(0.88, "npc")
    )
  )

  draw_cohort_panel(
    "HRS",
    hrs
  )

  popViewport()

  pushViewport(
    viewport(
      x = unit(0.745, "npc"),
      y = unit(0.485, "npc"),
      width = unit(0.42, "npc"),
      height = unit(0.88, "npc")
    )
  )

  draw_cohort_panel(
    "CHARLS",
    charls
  )

  popViewport()

  grid.text(
    paste(
      "State 0 models the stroke-free to stroke and stroke-free to death transitions;",
      "State 1 models the stroke to death transition."
    ),
    x = unit(0.52, "npc"),
    y = unit(0.018, "npc"),
    gp = gpar(
      fontsize = 8.2,
      col = "#555555",
      fontfamily = "sans"
    )
  )
}

png(
  file.path(
    figure_dir,
    "Figure_1_Study_flow_styled_v2.png"
  ),
  width = 4200,
  height = 3000,
  res = 300,
  bg = "white"
)

draw_flowchart()
dev.off()

tiff(
  file.path(
    figure_dir,
    "Figure_1_Study_flow_styled_v2.tiff"
  ),
  width = 8400,
  height = 6000,
  res = 600,
  compression = "lzw",
  bg = "white"
)

draw_flowchart()
dev.off()

cat(
  "\nRevised flowchart completed:\n",
  " - results/figures/Figure_1_Study_flow_styled_v2.png\n",
  " - results/figures/Figure_1_Study_flow_styled_v2.tiff\n",
  sep = ""
)

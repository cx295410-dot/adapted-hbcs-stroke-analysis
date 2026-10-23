#!/usr/bin/env Rscript

# Redraw Figure 3 only.
# No raw data are read and no statistical models are refitted.

options(stringsAsFactors = FALSE)

input_file <- "results/hbcs_category_model_summary.csv"
figure_dir <- "results/figures"

if (!file.exists(input_file)) {
  stop("Missing input file: ", input_file)
}

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dat <- read.csv(
  input_file,
  check.names = FALSE
)

required_columns <- c(
  "cohort",
  "transition",
  "category",
  "hazard_ratio",
  "lower_95",
  "upper_95",
  "p_trend_label"
)

missing_columns <- setdiff(
  required_columns,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

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

transition_titles <- c(
  "0_to_1_stroke" = "Stroke-free to stroke",
  "0_to_2_death" = "Stroke-free to death",
  "1_to_2_death" = "Stroke to death"
)

# Explicit order:
# top row = three HRS transitions
# bottom row = two CHARLS transitions
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

draw_figure_3 <- function() {

  layout(
    matrix(
      c(
        1, 1, 2, 2, 3, 3,
        4, 4, 4, 5, 5, 5
      ),
      nrow = 2,
      byrow = TRUE
    ),
    heights = c(1, 1)
  )

  par(
    mar = c(4.8, 4.8, 3.2, 1.2),
    oma = c(1.5, 1, 2.5, 1),
    family = "sans"
  )

  for (i in seq_len(nrow(panel_specification))) {

    current_cohort <-
      panel_specification$cohort[i]

    current_transition <-
      panel_specification$transition[i]

    panel <- dat[
      dat$cohort == current_cohort &
        dat$transition == current_transition,
    ]

    panel <- panel[
      match(category_order, panel$category),
    ]

    if (nrow(panel) != 4 ||
        any(is.na(panel$category))) {
      stop(
        "Incomplete category results for ",
        current_cohort,
        ": ",
        current_transition
      )
    }

    x <- seq_len(4)

    panel_colour <- if (
      current_cohort == "HRS"
    ) {
      "#225EA8"
    } else {
      "#D94801"
    }

    plot(
      x,
      panel$hazard_ratio,
      type = "n",
      log = "y",
      ylim = c(0.18, 1.20),
      xlim = c(0.75, 4.25),
      xaxt = "n",
      xlab = "Adapted hBCS category",
      ylab = "Hazard ratio (95% CI)",
      main = paste0(
        current_cohort,
        ": ",
        transition_titles[
          current_transition
        ]
      ),
      bty = "l"
    )

    axis(
      side = 1,
      at = x,
      labels = category_labels
    )

    axis(
      side = 2,
      at = c(
        0.2,
        0.3,
        0.4,
        0.5,
        0.7,
        1.0
      ),
      labels = c(
        "0.2",
        "0.3",
        "0.4",
        "0.5",
        "0.7",
        "1.0"
      ),
      las = 1
    )

    abline(
      h = 1,
      lty = 2,
      col = "grey55"
    )

    arrows(
      x0 = x,
      y0 = panel$lower_95,
      x1 = x,
      y1 = panel$upper_95,
      angle = 90,
      code = 3,
      length = 0.05,
      col = panel_colour,
      lwd = 1.6
    )

    lines(
      x,
      panel$hazard_ratio,
      type = "b",
      pch = 19,
      lwd = 2,
      col = panel_colour
    )

    trend_text <- unique(
      panel$p_trend_label
    )

    trend_text <- trend_text[
      !is.na(trend_text)
    ]

    if (length(trend_text) > 0) {
      text(
        x = 1,
        y = 0.205,
        labels = paste0(
          "P for trend ",
          trend_text[1]
        ),
        adj = c(0, 0.5),
        cex = 0.82
      )
    }
  }

  mtext(
    "Dose-response associations of adapted hBCS categories",
    side = 3,
    outer = TRUE,
    line = 0.7,
    font = 2,
    cex = 1.25
  )
}

png(
  filename = file.path(
    figure_dir,
    "Figure_3_hBCS_dose_response.png"
  ),
  width = 3000,
  height = 2000,
  res = 300
)

draw_figure_3()
dev.off()

tiff(
  filename = file.path(
    figure_dir,
    "Figure_3_hBCS_dose_response.tiff"
  ),
  width = 3000,
  height = 2000,
  res = 300,
  compression = "lzw"
)

draw_figure_3()
dev.off()

cat(
  "Figure 3 completed:\n",
  " - results/figures/",
  "Figure_3_hBCS_dose_response.png\n",
  " - results/figures/",
  "Figure_3_hBCS_dose_response.tiff\n",
  sep = ""
)

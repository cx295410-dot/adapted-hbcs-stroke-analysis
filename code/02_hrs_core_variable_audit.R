options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("haven", "dplyr", "purrr", "readr", "tidyselect")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

input_file <- file.path("data", "HRS", "randhrs1992_2022v1.dta")
dictionary_file <- file.path("results", "hrs_all_variables.csv")
output_dir <- "results"

if (!file.exists(input_file)) stop("Missing HRS data file: ", input_file)
if (!file.exists(dictionary_file)) {
  stop("Missing variable inventory. Run code/01_hrs_variable_inventory.R first.")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

waves <- 8:16
years <- seq(2006, 2022, by = 2)

static_spec <- data.frame(
  domain = c(
    "identifier", "sex", "race", "ethnicity", "education", "education",
    "birth", "death", "death"
  ),
  wave = NA_integer_,
  year = NA_integer_,
  variable = c(
    "hhidpn", "ragender", "raracem", "rahispan", "raeduc", "raedyrs",
    "rabyear", "radyear", "radmonth"
  )
)

wave_roots <- data.frame(
  domain = c(
    "interview_status", "interview_date", "age", "weight",
    "marital_status", "social_connection",
    "stroke", "stroke_confirmed",
    "hypertension_current", "hypertension_ever",
    "diabetes_current", "diabetes_ever", "bmi",
    "smoking_current", "smoking_ever",
    "alcohol_any", "alcohol_days", "alcohol_amount",
    "activity_vigorous", "activity_moderate", "activity_light",
    "mood", "mood_missing_items",
    "sleep_falling", "sleep_rested", "sleep_waking_night", "sleep_waking_early",
    "loneliness"
  ),
  suffix = c(
    "iwstat", "iwmid", "agey_m", "wtresp",
    "mstat", "mpart",
    "stroke", "stroke2",
    "hibp", "hibpe",
    "diab", "diabe", "bmi",
    "smoken", "smokev",
    "drink", "drinkd", "drinkn",
    "vgactx", "mdactx", "ltactx",
    "cesd", "cesdm",
    "sleepfal", "sleeprt", "sleepwkn", "sleepwke",
    "lblonely3"
  )
)

wave_spec <- do.call(rbind, lapply(seq_len(nrow(wave_roots)), function(i) {
  data.frame(
    domain = wave_roots$domain[i],
    wave = waves,
    year = years,
    variable = paste0("r", waves, wave_roots$suffix[i])
  )
}))

requested_spec <- rbind(static_spec, wave_spec)
inventory <- readr::read_csv(dictionary_file, show_col_types = FALSE)
available_names <- inventory$variable

requested_spec$available <- requested_spec$variable %in% available_names
missing_spec <- requested_spec[!requested_spec$available, , drop = FALSE]
selected_spec <- requested_spec[requested_spec$available, , drop = FALSE]

readr::write_csv(
  requested_spec,
  file.path(output_dir, "hrs_core_requested_variables.csv"),
  na = ""
)
readr::write_csv(
  missing_spec,
  file.path(output_dir, "hrs_core_missing_variables.csv"),
  na = ""
)

selected_variables <- unique(selected_spec$variable)
cat("Reading", length(selected_variables), "selected variables from the HRS file...\n")

hrs <- haven::read_dta(
  input_file,
  col_select = tidyselect::all_of(selected_variables),
  .name_repair = "minimal"
)

if (nrow(hrs) < 10000) stop("Unexpectedly few HRS records: ", nrow(hrs))
if (!"hhidpn" %in% names(hrs)) stop("hhidpn was not read")
if (anyDuplicated(hrs$hhidpn)) stop("hhidpn is not unique in the wide HRS file")

safe_range_value <- function(x, fun) {
  observed <- x[!is.na(x)]
  if (length(observed) == 0 || !is.numeric(observed)) return("")
  as.character(fun(observed))
}

get_label <- function(x) {
  value <- attr(x, "label", exact = TRUE)
  if (is.null(value) || length(value) == 0) "" else as.character(value[[1]])
}

availability <- purrr::map_dfr(selected_variables, function(variable_name) {
  x <- hrs[[variable_name]]
  spec_row <- selected_spec[selected_spec$variable == variable_name, , drop = FALSE][1, ]
  observed <- sum(!is.na(x))
  data.frame(
    domain = spec_row$domain,
    wave = spec_row$wave,
    year = spec_row$year,
    variable = variable_name,
    label = get_label(x),
    n_total = length(x),
    n_observed = observed,
    missing_pct = round(100 * (1 - observed / length(x)), 2),
    n_unique_observed = length(unique(x[!is.na(x)])),
    minimum = safe_range_value(x, min),
    maximum = safe_range_value(x, max)
  )
})

code_counts <- purrr::map_dfr(selected_variables, function(variable_name) {
  x <- hrs[[variable_name]]
  observed_values <- x[!is.na(x)]
  unique_count <- length(unique(observed_values))
  if (length(observed_values) == 0 || unique_count > 20) {
    return(data.frame(
      domain = character(), wave = integer(), year = integer(),
      variable = character(), value = character(), value_label = character(),
      n = integer(), percent_observed = numeric()
    ))
  }

  tab <- sort(table(as.character(observed_values)), decreasing = TRUE)
  value_labels <- attr(x, "labels", exact = TRUE)
  label_lookup <- if (is.null(value_labels)) {
    setNames(character(), character())
  } else {
    setNames(names(value_labels), as.character(unname(value_labels)))
  }
  spec_row <- selected_spec[selected_spec$variable == variable_name, , drop = FALSE][1, ]
  value_text <- names(tab)
  matched_labels <- unname(label_lookup[value_text])
  matched_labels[is.na(matched_labels)] <- ""

  data.frame(
    domain = spec_row$domain,
    wave = spec_row$wave,
    year = spec_row$year,
    variable = variable_name,
    value = value_text,
    value_label = matched_labels,
    n = as.integer(tab),
    percent_observed = round(100 * as.integer(tab) / sum(tab), 2)
  )
})

domain_summary <- availability |>
  dplyr::group_by(.data$domain) |>
  dplyr::summarise(
    variables_available = dplyr::n(),
    minimum_observed = min(.data$n_observed),
    median_missing_pct = round(stats::median(.data$missing_pct), 2),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$domain)

readr::write_csv(
  availability,
  file.path(output_dir, "hrs_core_availability.csv"),
  na = ""
)
readr::write_csv(
  code_counts,
  file.path(output_dir, "hrs_core_code_counts.csv"),
  na = ""
)
readr::write_csv(
  domain_summary,
  file.path(output_dir, "hrs_core_domain_summary.csv"),
  na = ""
)

cat("\nCore-variable audit completed\n")
cat("HRS records:", nrow(hrs), "\n")
cat("Selected variables:", ncol(hrs), "\n")
cat("Requested but unavailable variables:", nrow(missing_spec), "\n\n")
print(as.data.frame(domain_summary), row.names = FALSE)
cat("\nOutputs:\n")
cat(" - results/hrs_core_requested_variables.csv\n")
cat(" - results/hrs_core_missing_variables.csv\n")
cat(" - results/hrs_core_availability.csv\n")
cat(" - results/hrs_core_code_counts.csv\n")
cat(" - results/hrs_core_domain_summary.csv\n")

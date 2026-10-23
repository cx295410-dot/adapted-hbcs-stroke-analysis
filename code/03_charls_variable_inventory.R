options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("haven", "dplyr", "purrr", "readr", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

input_files <- c(
  regular = file.path("data", "CHARLS", "Harmonized", "H_CHARLS_D_Data.dta"),
  end_of_life = file.path("data", "CHARLS", "End_of_Life", "H_CHARLS_EOL_a.dta")
)
output_dir <- "results"

missing_files <- input_files[!file.exists(input_files)]
if (length(missing_files) > 0) {
  stop("Missing CHARLS files: ", paste(missing_files, collapse = ", "))
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

get_label <- function(x) {
  value <- attr(x, "label", exact = TRUE)
  if (is.null(value) || length(value) == 0) "" else as.character(value[[1]])
}

make_dictionary <- function(file_path, dataset_name) {
  cat("Reading one row from", dataset_name, "...\n")
  metadata <- haven::read_dta(file_path, n_max = 1, .name_repair = "minimal")
  if (ncol(metadata) < 10) {
    stop(dataset_name, " contains unexpectedly few variables: ", ncol(metadata))
  }
  if (anyDuplicated(names(metadata))) {
    stop(dataset_name, " contains duplicate variable names")
  }
  data.frame(
    dataset = dataset_name,
    variable = names(metadata),
    label = vapply(metadata, get_label, character(1)),
    wave_prefix = stringr::str_match(names(metadata), "^[rs]([0-9]{1,2})")[, 2],
    stringsAsFactors = FALSE
  )
}

variable_dictionary <- purrr::imap_dfr(input_files, make_dictionary)

search_text <- stringr::str_to_lower(
  paste(variable_dictionary$variable, variable_dictionary$label)
)

domain_patterns <- c(
  identifier = "hhidpn|person id|respondent id|household.*person",
  death = "death|deceased|dmonth|dyear|ddate|mortality|exit interview|end of life",
  stroke = "stroke|cerebrovascular|cva",
  hypertension = "hibp|hypertension|high blood pressure|blood pressure",
  diabetes = "diab|diabetes|blood sugar|glyc|a1c",
  bmi = "bmi|body mass|body weight|height|weight",
  smoking = "smok|cigarette|tobacco",
  alcohol = "alcohol|drink|drinking",
  physical_activity = "physical activ|phys activ|vgact|mdact|ltact|exercise|walking",
  sleep = "sleep|insomnia|night rest|nap",
  mood = "cesd|depress|sad|mood|hopeless|mental health",
  social_connection = "social|lonely|loneliness|friend|relative|marital|partner|cohabit",
  covariate = "age|gender|sex|educ|wealth|income|rural|urban|region|sampling weight|stratum|cluster"
)

candidate_variables <- purrr::imap_dfr(domain_patterns, function(pattern, domain) {
  keep <- stringr::str_detect(search_text, stringr::regex(pattern, ignore_case = TRUE))
  if (!any(keep)) {
    return(data.frame(
      dataset = character(), domain = character(), variable = character(),
      label = character(), wave_prefix = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    dataset = variable_dictionary$dataset[keep],
    domain = domain,
    variable = variable_dictionary$variable[keep],
    label = variable_dictionary$label[keep],
    wave_prefix = variable_dictionary$wave_prefix[keep],
    stringsAsFactors = FALSE
  )
})

candidate_summary <- candidate_variables |>
  dplyr::count(.data$dataset, .data$domain, name = "candidate_count") |>
  dplyr::arrange(.data$dataset, .data$domain)

readr::write_csv(
  variable_dictionary,
  file.path(output_dir, "charls_all_variables.csv"),
  na = ""
)
readr::write_csv(
  candidate_variables,
  file.path(output_dir, "charls_candidate_variables.csv"),
  na = ""
)
readr::write_csv(
  candidate_summary,
  file.path(output_dir, "charls_candidate_summary.csv"),
  na = ""
)

session_lines <- capture.output(sessionInfo())
writeLines(session_lines, file.path(output_dir, "charls_inventory_sessionInfo.txt"))

dataset_sizes <- variable_dictionary |>
  dplyr::count(.data$dataset, name = "variables")

cat("\nCHARLS inventory completed\n")
print(as.data.frame(dataset_sizes), row.names = FALSE)
cat("Candidate rows:", nrow(candidate_variables), "\n\n")
print(as.data.frame(candidate_summary), row.names = FALSE)
cat("\nOutputs:\n")
cat(" - results/charls_all_variables.csv\n")
cat(" - results/charls_candidate_variables.csv\n")
cat(" - results/charls_candidate_summary.csv\n")
cat(" - results/charls_inventory_sessionInfo.txt\n")

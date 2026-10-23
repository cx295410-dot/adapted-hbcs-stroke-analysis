options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("haven", "dplyr", "purrr", "readr", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    ". Run the package-install command in 从这里开始.md first."
  )
}

input_file <- file.path("data", "HRS", "randhrs1992_2022v1.dta")
output_dir <- "results"

if (!file.exists(input_file)) {
  stop("HRS file not found: ", input_file)
}

file_size <- file.info(input_file)$size
if (is.na(file_size) || file_size < 1e9) {
  stop("HRS file is unexpectedly small: ", file_size, " bytes")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Input:", normalizePath(input_file), "\n")
cat("File size:", format(file_size, big.mark = ","), "bytes\n")
cat("Reading one row to obtain variable names and labels...\n")

metadata <- haven::read_dta(
  input_file,
  n_max = 1,
  .name_repair = "minimal"
)

if (ncol(metadata) < 1000) {
  stop("Unexpectedly few variables: ", ncol(metadata))
}
if (anyDuplicated(names(metadata))) {
  stop("Duplicate variable names found in the HRS file")
}

get_label <- function(x) {
  value <- attr(x, "label", exact = TRUE)
  if (is.null(value) || length(value) == 0 || is.na(value)) "" else as.character(value)
}

variable_dictionary <- data.frame(
  variable = names(metadata),
  label = vapply(metadata, get_label, character(1)),
  stringsAsFactors = FALSE
)

variable_dictionary$wave_prefix <- stringr::str_match(
  variable_dictionary$variable,
  "^[rs]([0-9]{1,2})"
)[, 2]

search_text <- stringr::str_to_lower(
  paste(variable_dictionary$variable, variable_dictionary$label)
)

domain_patterns <- c(
  identifier = "hhidpn|person identifier|household.*person",
  death = "death|deceased|dmonth|dyear|ddate|dage|mortality|exit interview",
  stroke = "stroke|cerebrovascular|cva",
  hypertension = "hibp|hypertension|high blood pressure|blood pressure",
  diabetes = "diab|diabetes|blood sugar|glyc|a1c",
  bmi = "bmi|body mass|body weight|height|weight",
  smoking = "smok|cigarette|tobacco",
  alcohol = "alcohol|drink|drinking",
  physical_activity = "physical activ|vigorous|moderate activ|exercise|walking",
  sleep = "sleep|insomnia|restless",
  mood = "cesd|depress|sad|mood|hopeless|mental health",
  social_connection = "social|lonely|loneliness|friend|relative|marital|partner|cohabit",
  covariate = "age|gender|sex|educ|wealth|income|race|hispan|sampling weight|stratum|cluster"
)

candidate_variables <- purrr::imap_dfr(domain_patterns, function(pattern, domain) {
  keep <- stringr::str_detect(search_text, stringr::regex(pattern, ignore_case = TRUE))
  if (!any(keep)) {
    return(data.frame(
      domain = character(), variable = character(), label = character(),
      wave_prefix = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    domain = domain,
    variable_dictionary[keep, c("variable", "label", "wave_prefix")],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
})

candidate_summary <- candidate_variables |>
  dplyr::count(.data$domain, name = "candidate_count") |>
  dplyr::arrange(.data$domain)

required_domains <- names(domain_patterns)
missing_domains <- setdiff(required_domains, candidate_summary$domain)
if (length(missing_domains) > 0) {
  warning("No candidates found for: ", paste(missing_domains, collapse = ", "))
}

readr::write_csv(
  variable_dictionary,
  file.path(output_dir, "hrs_all_variables.csv"),
  na = ""
)
readr::write_csv(
  candidate_variables,
  file.path(output_dir, "hrs_candidate_variables.csv"),
  na = ""
)
readr::write_csv(
  candidate_summary,
  file.path(output_dir, "hrs_candidate_summary.csv"),
  na = ""
)

session_lines <- capture.output(sessionInfo())
writeLines(session_lines, file.path(output_dir, "hrs_inventory_sessionInfo.txt"))

cat("\nInventory completed\n")
cat("Variables found:", nrow(variable_dictionary), "\n")
cat("Candidate rows:", nrow(candidate_variables), "\n\n")
print(as.data.frame(candidate_summary), row.names = FALSE)
cat("\nOutputs:\n")
cat(" - results/hrs_all_variables.csv\n")
cat(" - results/hrs_candidate_variables.csv\n")
cat(" - results/hrs_candidate_summary.csv\n")
cat(" - results/hrs_inventory_sessionInfo.txt\n")

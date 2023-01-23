pacta_log_dir <- "00_Log_Files"
parameter_file_dir <- "10_Parameter_File"
raw_input_dir <- "20_Raw_Inputs"
processed_input_dir <- "30_Processed_Inputs"
result_dir <- "40_Results"
output_dir <- "50_Outputs"

minimal_pacta_dirs <- c(
  parameter_file_dir,
  raw_input_dir
)

essential_pacta_dirs <- c(
  minimal_pacta_dirs,
  processed_input_dir,
  result_dir,
  output_dir
)

pacta_dirs <- c(pacta_log_dir, essential_pacta_dirs)

portfolio_parameter_file_regex <- "_PortfolioParameters.yml$"

#' @export
find_portfolio_parameter_files <- function(
  search_path,
  searcher = searcher_simple,
  pattern = portfolio_parameter_regex,
  ...
  ) {
  log_info("Searching for parameter files in: {search_path}")
  log_trace("using searcher {as.character(substitute(searcher))}")
  matches <- searcher(
    search_path = search_path,
    pattern = pattern,
    ...
  )
  for (x in matches){
    log_debug("Processing candidate parameter file: {x}")
    has_portfolio <- verify_parameter_file(file.path(search_path, x))
    if (has_portfolio) {
      add_to_queue(queue = portfolio_queue, title = "portfolio", message = x)
    } else {
      log_warn("Candidate parameter file rejected: {x}")
    }
  }
}

verify_parameter_file <- function(filepath) {
  log_debug("Verifying parameter file: {filepath}")
  is_valid_parameter_file <- TRUE
  log_trace("Checking {filepath} exists")
  if (!file.exists(filepath)) {
    log_warn("Parameter file {filepath} does not exist")
    is_valid_parameter_file <- FALSE
  }
  log_trace("Checking {filepath} is located in {parameter_file_dir}")
  if (basename(dirname(filepath)) != parameter_file_dir) {
    log_warn("Parameter file {filepath} is not located in {parameter_file_dir}")
    is_valid_parameter_file <- FALSE
  }
  portfolio_name <- port_name_from_parameter_file(filepath)
  # Need to go 2 levels up (from file):
  # foo/10_Parameter_File/bar_PortfolioParameters.yml
  # foo/10_Parameter_File
  # foo/
  candidate_working_dir <- dirname(dirname(filepath))
  log_trace("Checking that matched portfolio CSV exists for {filepath}")
  csv_exists <- file.exists(
    file.path(
      candidate_working_dir,
      raw_input_dir,
      paste0(portfolio_name, ".csv")
    )
  )
  if (!csv_exists){
    log_warn("No matched portfolio CSV for {filepath}")
    is_valid_parameter_file <- FALSE
  }
  return(is_valid_parameter_file)
}

port_name_from_parameter_file <- function(file) {
  log_trace("extracting portfolio name from {file}")
  bn <- basename(file)
  portname <- gsub(
    x = bn,
    pattern = portfolio_parameter_file_regex,
    replacement = ""
  )
  return(portname)
}

# This works fine for small directories, but it's slow for large
# directories (thousands of files), espescially with many nested sub
# directories
#' @export
searcher_simple <- function(
  search_path,
  pattern
  ) {
  list.files(
    path = search_path,
    recursive = TRUE,
    pattern = portfolio_parameter_file_regex,
    full.names = FALSE
  )
}

#' @export
searcher_distributed <- function(
  search_path,
  pattern
  ) {
  log_fatal("Distributed searching not implemented yet.")
  stop()
}

verify_pacta_directory <- function(
  working_dir,
  portfolio_name,
  complete = TRUE
  ) {
  log_trace()

}

ensure_pacta_directory <- function(working_dir) {

}

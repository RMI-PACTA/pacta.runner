pacta_log_dir <- "00_Log_Files"
parameter_file_dir <- "10_Parameter_File"
raw_input_dir <- "20_Raw_Inputs"
processed_input_dir <- "30_Processed_Inputs"
results_dir <- "40_Results"
output_dir <- "50_Outputs"

minimal_pacta_dirs <- c(
  parameter_file_dir,
  raw_input_dir
)

essential_pacta_dirs <- c(
  minimal_pacta_dirs,
  processed_input_dir,
  results_dir,
  output_dir
)

pacta_dirs <- c(pacta_log_dir, essential_pacta_dirs)

portfolio_parameter_file_regex <- "_PortfolioParameters.yml$"

find_portfolios <- function(
  search_path,
  reject_done_processed_inputs = TRUE,
  reject_done_results = TRUE,
  reject_done_outputs = TRUE,
  ...
  ) {
  valid_portfolio_parameter_files <- find_portfolio_parameter_files(
    search_path = search_path,
    ...
  )
  portfolios <- c()
  for (x in valid_portfolio_parameter_files) {
    working_dir <- working_dir_from_parameter_file(
      filepath = file.path(search_path, x),
      expected_directories = minimal_pacta_dirs
    )
    processed_inputs <- file.path(working_dir, processed_input_dir)
    results <- file.path(working_dir, results_dir)
    outputs <- file.path(working_dir, output_dir)
    is_rejected <- FALSE
    if (reject_done_processed_inputs && dir.exists(processed_inputs)) {
      log_trace("Checking for processed inputs in: {processed_inputs}")
      if (!is_directory_empty(processed_inputs)) {
        log_warn("Processed input directory not empty: {processed_inputs}")
        is_rejected = TRUE
      }
    }
    if (reject_done_outputs && dir.exists(results)) {
      log_trace("Checking for results in: {results}")
      if (!is_directory_empty(results)) {
        log_warn("Results directory not empty: {results}")
        is_rejected = TRUE
      }
    }
    if (reject_done_outputs && dir.exists(outputs)) {
      log_trace("Checking for outputs in: {outputs}")
      if (!is_directory_empty(outputs)) {
        log_warn("Outputs directory not empty: {outputs}")
        is_rejected = TRUE
      }
    }
    if (!is_rejected){
      portfolios <- c(portfolios, x)
      add_portfolio_to_queue(portfolio = x, queue = portfolio_queue)
    }
  }
  return(portfolios)
}

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
  out <- c()
  for (x in matches){
    log_debug("Processing candidate parameter file: {x}")
    has_portfolio <- verify_parameter_file(file.path(search_path, x))
    if (!has_portfolio) {
      log_warn("Candidate parameter file rejected: {x}")
    } else {
      log_debug("Valid Parameter file found: {x}")
      out <- c(out, x)
    }
  }
  return(out)
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
  candidate_working_dir <- working_dir_from_parameter_file(
    filepath = filepath,
    expected_directories = minimal_pacta_dirs
  )
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

working_dir_from_parameter_file <- function(
  filepath,
  ...
  ) {
  # Need to go 2 levels up (from file):
  # foo/10_Parameter_File/bar_PortfolioParameters.yml
  # foo/10_Parameter_File
  # foo/
  log_trace("Identifying working dir for parameter file: {filepath}")
  working_dir <- dirname(dirname(filepath))
  log_trace("Working dir is {working_dir}")
  validate_working_dir(working_dir, ...)
  return(working_dir)
}

validate_working_dir <- function(
  working_dir,
  expected_directories = pacta_dirs
  ) {
  is_valid_working_dir <- TRUE
  log_trace("Checking directory exists: {working_dir}")
  if (!dir.exists(working_dir)) {
    log_error("Candidate working_dir does not exist: {working_dir}")
    is_valid_working_dir <- FALSE
  }
  log_trace("Checking for PACTA subdirectories: {expected_directories}")
  subdirectories <- file.path(working_dir, expected_directories)
  if (!all(dir.exists(subdirectories))){
    missing_subs <- subdirectories[!dir.exists(subdirectories)]
    log_warn("Missing subdirectories: {missing_subs}")
    is_valid_working_dir <- FALSE
  }
  return(is_valid_working_dir)
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

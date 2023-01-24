#' @export
run_pacta <- function(
  portfolio_parameter_file,
  user_dir = tempfile(),
  user_id = 4L,
  ...,
  copy_files = TRUE,
  pacta_runner = run_pacta_fake
  ) {
  log_info("Running PACTA from parameters file: {portfolio_parameter_file}")
  working_dir <- working_dir_from_parameter_file(
    filepath = portfolio_parameter_file,
    expected_directories = essential_pacta_dirs,
    create = TRUE
  )
  log_debug("Working dir: {working_dir}")

  if (!dir.exists(user_dir)) {
    dir.create(file.path(user_dir, user_id), recursive = TRUE)
  }

  # Copy files to isolate fallout if needed.
  if (copy_files) {
    t <- tempfile()
    u <- tempfile()
    log_debug("Copying PACTA directory to {t}")
    working_dir_is_copied <- copy_dir_contents(from = working_dir, to = t)
    if (working_dir_is_copied) {
      target_working_dir <- t
    }
    log_debug("Copying user directory to {u}")
    user_dir_is_copied <- copy_dir_contents(from = user_dir, to = u)
    if (user_dir_is_copied) {
      target_user_dir <- u
    }
  } else {
    target_working_dir <- working_dir
    target_user_dir <- user_dir
  }
  validate_working_dir(
    working_dir = target_working_dir,
    expected_directories = essential_pacta_dirs,
    create = TRUE
  )

  log_info("Running PACTA via {as.character(substitute(pacta_runner))}")
  exit_code <- pacta_runner(
    working_dir = target_working_dir,
    user_dir = target_user_dir,
    ...
  )

  # copy files back
  if (exit_code == 0L) {
    if (copy_files) {
      log_info('Copying results back into {working_dir}')
      copy_dir_contents(from = target_working_dir, to = working_dir)
      if (dir.exists(target_working_dir)) {
        unlink(target_working_dir)
      }
      if (dir.exists(target_user_dir)) {
        unlink(target_user_dir)
      }
    }
    log_success("Successfully ran PACTA. Results are available at {working_dir}")
  } else {
    log_error("Failed to run PACTA.")
  }
}

process_unknown <- function(message, ...) {
  log_error("Unknown message type: {message$title}")
  log_error("Message contents: {message$message}")
}

process_portfolio <- function(
  portfolio_parameter_file,
  stage,
  ...
  ) {
  port_name <- port_name_from_parameter_file(portfolio_parameter_file)
  run_pacta(
    portfolio_parameter_file = portfolio_parameter_file,
    script_to_run = stage,
    ...
  )
}

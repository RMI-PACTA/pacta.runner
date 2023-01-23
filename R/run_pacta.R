#' @export
run_pacta <- function(
  working_dir,
  user_dir = tempfile(),
  user_id = 4L,
  ...,
  copy_files = TRUE,
  pacta_runner = run_pacta_docker
  ) {

  if (!dir.exists(user_dir)) {
    dir.create(file.path(user_dir, user_id), recursive = TRUE)
  }

  # Copy files to isolate fallout if needed.
  if (copy_files) {
    t <- tempfile()
    u <- tempfile()
    log_info("Copying PACTA directory to {t}")
    working_dir_is_copied <- copy_dir_contents(from = working_dir, to = t)
    if (working_dir_is_copied) {
      target_working_dir <- t
    }
    user_dir_is_copied <- copy_dir_contents(from = user_dir, to = u)
    if (user_dir_is_copied) {
      target_user_dir <- u
    }
  } else {
    target_working_dir <- working_dir
    target_user_dir <- user_dir
  }

  log_info("Running PACTA via {as.character(substitute(pacta_runner))}")
  exit_code <- pacta_runner(
    working_dir = working_dir,
    user_dir = user_dir,
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


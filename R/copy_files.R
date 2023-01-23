#' @export
copy_dir_contents <- function(
  from,
  to,
  validate = TRUE
  ) {
  if (!dir.exists(from)) {
    log_error("Cannot copy directory {from}, because it is not an existing directory.")
  }
  if (!dir.exists(to)) {
    log_debug("Creating directory {to}, because it is not an existing directory.")
    dir.create(to)
  }

  contents <- c(
    list.files(
      path = from,
      recursive = TRUE,
      all.files = TRUE,
      include.dirs = FALSE,
      full.names = FALSE
    )
  )

  # create dirs to place contents
  content_dirs <- setdiff(unique(dirname(contents)), c("."))
  for (dir in file.path(to, content_dirs)) {
    if (!dir.exists(dir)) {
      log_trace("Creating directory {dir} to copy contents into.")
      dir.create(dir, recursive = TRUE)
    }
  }
  
  log_debug("Copying files from {from} to {to}.")
  # paths are tricky with base::file.copy, but needed because
  # fs::dir_copy doesn't allow for ignoring permissions or timestamps
  # (needed on some remote file shares, such as Azure File Share)
  is_copied <- base::file.copy(
    from = file.path(from, contents),
    to = file.path(to, contents),
    recursive = FALSE,
    overwrite = TRUE,
    copy.mode = FALSE,
    copy.date = FALSE
  )

  if (validate) {
    files_from <- list.files(
        path = from,
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = FALSE
      )
    files_to <- list.files(
        path = to,
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = FALSE
      )
    digest_from <- digest_dir(file.path(from, files_from))
    digest_to <- digest_dir(file.path(to, files_to))
    missing_files <- setdiff(files_from, files_to)
    if ( length(missing_files) > 0 ) {
      log_error("Missing files: {missing_files}")
      stop()
    }
    missing_hashes <- setdiff(digest_from, digest_to)
    if (length(missing_hashes) > 0) {
      bad_copies <- digest_from[which(digest_from %in% missing_hashes)]
      bad_copies_names <- names(bad_copies)
      for (i in seq_along(bad_copies)) {
        log_error("Improperly copied file: {bad_copies_names[i]}, md5: {bad_copies[i]}")
      }
      stop()
    }
  }

  log_debug("Sucessfully copied files from {from} to {to}.")
  return(all(is_copied))
}

digest_dir <- function(files) {
  vapply(
    X = files,
    FUN = digest,
    FUN.VALUE = character(1),
    algo = "md5",
    file = TRUE
  )
}

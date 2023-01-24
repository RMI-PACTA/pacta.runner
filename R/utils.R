# use_system makes it go fast.
# see https://stackoverflow.com/q/21576944
is_directory_empty <- function(
  path,
  all.files = TRUE,
  include.dirs = FALSE,
  recursive = TRUE,
  use_system = all(Sys.which(c("find", "head")) != "")
  ) {
  log_trace("Checking if directory is empty: {path}")
  to_ignore <- c(path, ".", "..")
  if (use_system) {
    flags <- paste(
      ifelse(all.files, "", "-not -path '*/.*'"),
      ifelse(recursive, "", "-maxdepth 1"),
      ifelse(include.dirs, "", "-not -type d")
    )
    command <- paste("find", path, flags, "| head -n 4")
    log_trace("running {command}")
    listing <- system(command, intern = TRUE)
  } else {
    listing <- list.files(
      path = path,
      all.files = all.files,
      include.dirs = include.dirs,
      recursive = recursive,
    )
  }
  return(length(setdiff(listing, to_ignore)) == 0L)
}

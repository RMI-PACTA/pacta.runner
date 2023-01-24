# adding a lot of abstractions here, so that we can use something other
# than {txtq} in the future if we want.

#' @export
add_to_queue <- function(queue, title, message) {
  log_trace("Adding to queue: {queue$path()}, title: {title}, message: {message}")
  queue$push(title = title, message = message)
}

queue_pop <- function(queue) {
  popped <- queue$pop()
  log_trace("Popped from queue: {queue$path()}")
  if (nrow(popped) == 0L) {
    log_warn("Empty message recieved from queue: {queue$path()}")
    popped <- NULL
  } else {
    log_trace("title: {popped$title}, message: {popped$message}")
  }
  return(popped)
}

process_queue_message <- function(message, search_path, ...){
  portfolio_pattern <- "^portfolio\\|"
  if (grepl(x = message$title, pattern = portfolio_pattern)) {
    stage <- as.integer(
      gsub(
        x = message$title,
        pattern = portfolio_pattern,
        replacement = ""
      )
    )
    process_portfolio(
      portfolio_parameter_file = file.path(search_path, message$message),
      stage = stage,
      ...
    )
  } else {
    process_unknown(message)
  }
}

queue_count <- function(queue){
  return(queue$count())
}

add_portfolio_to_queue <- function(portfolio, stage, queue) {
  log_debug("Addding portfolio at stage {stage} to queue: {portfolio}")
  add_to_queue(
    queue = queue,
    title = paste("portfolio", stage, sep = "|"),
    message = portfolio
  )
}

queue_stats <- function(
  queue,
  logging_level = logger::INFO,
  detailed_logging_level = logger::DEBUG
  ) {
  log_level(detailed_logging_level, "Queue located at: {queue$path()}")
  log_level(logging_level, "Entries in queue: {queue$count()}")
}

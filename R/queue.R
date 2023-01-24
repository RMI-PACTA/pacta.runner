portfolio_queue <- "p.txt"

#' @export
add_to_queue <- function(queue, title, message) {
  log_trace("Adding to queue: {queue}, title: {title}, message: {message}")
  cat(file = queue, append = TRUE, paste(title, message, "\n", sep = "|"))
}

add_portfolio_to_queue <- function(portfolio, queue) {
  log_debug("Addding portfolio to queue: {portfolio}")
  add_to_queue(
    queue = queue,
    title = "portfolio",
    message = portfolio
  )
}

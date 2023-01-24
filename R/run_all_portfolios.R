#' @export
run_all_portfolios <- function(
  search_path,
  queue = txtq(file.path(search_path, "queue")),
  portfolio_stages = c(
    "/bound/bin/run-r-scripts-results-only",
    "/bound/bin/run-r-scripts-outputs-only"
  ),
  ...
  ) {

  # cold start
  if (queue_count(queue) == 0L) {
    # adding random sleep, so not all runners try to cold start at the
    # same time
    Sys.sleep(runif(1) * 10)
    portfolios <- find_portfolios(search_path, ...)
    add_portfolio_to_queue(portfolio = portfolios, stage = 1, queue = queue)
  }

  message <- queue_pop(queue)
  while (!is.null(message)) {
    process_queue_message(
      message = message,
      search_path = search_path
    )
    message <- queue_pop(queue)
  }

}

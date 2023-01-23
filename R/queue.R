portfolio_queue <- "p.txt"

#' @export
add_to_queue <- function(queue, title, message) {
  cat(file = queue, append = TRUE, paste(title, message, "\n", sep = "|"))
}

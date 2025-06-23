#' A transparent element line
#'
#' @param ... Other arguments passed to [ggplot2::element_line()].
#'
#' @return A element_line.
#' @export
element_line_transparent <- function(...) {
  ggplot2::element_line(colour = "transparent", ...)
}

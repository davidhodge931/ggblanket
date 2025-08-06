#' A transparent element line
#'
#' @param ... Arguments passed to [ggplot2::element_line()].
#'
#' @return A element_line.
#' @noRd
element_line_transparent <- function(...) {
  ggplot2::element_line(colour = "transparent", ...)
}

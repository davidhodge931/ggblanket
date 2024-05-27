#' #' Unlabel a axis or legend title
#' #'
#' #' @description
#' #' Remove a x or y axis title, or remove the legend title.
#' #'
#' #'
#' #' @param x TRUE or FALSE of whether to remove the x axis title.
#' #' @param y TRUE or FALSE of whether to remove the y axis title.
#' #' @param col TRUE or FALSE of whether to remove the legend.title
#' #' @param ... Provided to support trailing commas only.
#' #'
#' #' @return ggplot2 theme elements
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #'
#' #' set_blanket()
#' #'
#' #' penguins |>
#' #'   gg_bar(
#' #'     y = species,
#' #'     col = island,
#' #'     width = 0.75,
#' #'   ) +
#' #'   unlabel(x = TRUE, y = TRUE, col = TRUE)
#' #'
#' unlabel <- function(x = FALSE, y = FALSE, col = FALSE, ...) {
#'
#'   if (x & !y & !col) {
#'     ggplot2::theme(
#'       axis.title.x.bottom = ggplot2::element_blank(),
#'       axis.title.x.top = ggplot2::element_blank()
#'     )
#'   }
#'   else if (!x & y & !col) {
#'     ggplot2::theme(
#'       axis.title.y.left = ggplot2::element_blank(),
#'       axis.title.y.right = ggplot2::element_blank()
#'     )
#'   }
#'   else if (!x & !y & col) {
#'     ggplot2::theme(
#'       legend.title = ggplot2::element_blank()
#'     )
#'   }
#'   else if (x & y & !col) {
#'     ggplot2::theme(
#'       axis.title.x.bottom = ggplot2::element_blank(),
#'       axis.title.x.top = ggplot2::element_blank(),
#'       axis.title.y.left = ggplot2::element_blank(),
#'       axis.title.y.right = ggplot2::element_blank()
#'     )
#'   }
#'   else if (x & !y & col) {
#'     ggplot2::theme(
#'       axis.title.x.bottom = ggplot2::element_blank(),
#'       axis.title.x.top = ggplot2::element_blank(),
#'       legend.title = ggplot2::element_blank()
#'     )
#'   }
#'   else if (!x & y & col) {
#'     ggplot2::theme(
#'       axis.title.y.left = ggplot2::element_blank(),
#'       axis.title.y.right = ggplot2::element_blank(),
#'       legend.title = ggplot2::element_blank()
#'     )
#'   }
#'   else if (x & y & col) {
#'     ggplot2::theme(
#'       axis.title.x.bottom = ggplot2::element_blank(),
#'       axis.title.x.top = ggplot2::element_blank(),
#'       axis.title.y.left = ggplot2::element_blank(),
#'       axis.title.y.right = ggplot2::element_blank(),
#'       legend.title = ggplot2::element_blank()
#'     )
#'   }
#' }

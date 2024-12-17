#' #' Replace a axis line with an annotated segment.
#' #'
#' #' @param axis The axis. Either "x" or "y"
#' #' @param x_position The position of the "x" axis, if applicable. Either "bottom" or "top".
#' #' @param y_position The position of the "y" axis, if applicable. Either "left" or "right".
#' #' @param colour The colour of the annotated segment.
#' #' @param linewidth The linewidth of the annotated segment.
#' #' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' #'
#' #' @return A list of a annotate layer and theme elements.
#' #' @export
#' #'
#' #' @examples
#' #' library(dplyr)
#' #' library(ggplot2)
#' #' library(ggblanket)
#' #' library(palmerpenguins)
#' #'
#' #' set_blanket()
#' #'
#' #' penguins |>
#' #'   add_row(
#' #'     flipper_length_mm = 175,
#' #'     body_mass_g = 2500,
#' #'     species = "Adelie",
#' #'   ) |>
#' #'   gg_blanket(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'   ) +
#' #'   annotate_axis_line() +
#' #'   geom_point(size = 2.5)
#' #'
#' annotate_axis_line <- function(
#'     axis = "x",
#'     x_position = "bottom",
#'     y_position = "left",
#'     colour = NULL,
#'     linewidth = NULL,
#'     ...
#' ) {
#'
#'   if (axis == "x") {
#'     if (rlang::is_null(colour)) colour <- ggplot2::GeomHline$default_aes$colour
#'     if (rlang::is_null(linewidth)) linewidth <- ggplot2::GeomHline$default_aes$linewidth
#'
#'     if (x_position == "bottom") {
#'       stamp <- rlang::list2(
#'         ggplot2::annotate(
#'           "segment",
#'           x = -Inf, xend = Inf, y = -Inf, yend = -Inf,
#'           colour = colour,
#'           linewidth = linewidth,
#'           ...
#'         ),
#'         ggplot2::theme(axis.line.x.bottom = ggplot2::element_blank())
#'       )
#'     }
#'     else if (x_position == "top")  {
#'       stamp <- rlang::list2(
#'         ggplot2::annotate(
#'           "segment",
#'           x = -Inf, xend = Inf, y = Inf, yend = Inf,
#'           colour = colour,
#'           linewidth = linewidth,
#'           ...
#'         ),
#'         ggplot2::theme(axis.line.x.top = ggplot2::element_blank())
#'       )
#'     }
#'   }
#'   else if (axis == "y") {
#'     if (rlang::is_null(colour)) colour <- ggplot2::GeomVline$default_aes$colour
#'     if (rlang::is_null(linewidth)) linewidth <- ggplot2::GeomVline$default_aes$linewidth
#'
#'     if (y_position == "left") {
#'       stamp <- rlang::list2(
#'         ggplot2::annotate(
#'           "segment",
#'           x = -Inf, xend = -Inf, y = -Inf, yend = Inf,
#'           colour = colour,
#'           linewidth = linewidth,
#'           ...
#'         ),
#'         ggplot2::theme(axis.line.y.left = ggplot2::element_blank())
#'       )
#'     }
#'     else if (y_position == "right") {
#'       stamp <- rlang::list2(
#'         ggplot2::annotate(
#'           "segment",
#'           x = Inf, xend = Inf, y = -Inf, yend = Inf,
#'           colour = colour,
#'           linewidth = linewidth,
#'           ...
#'         ),
#'         ggplot2::theme(axis.line.y.right = ggplot2::element_blank())
#'       )
#'     }
#'   }
#'   return(stamp)
#' }

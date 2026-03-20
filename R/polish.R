#' #' Modern theme polish
#' #'
#' #' Removes gridlines and axis line/tick elements from the non-focused dimension.
#' #' Also removes ticks on discrete axes.
#' #'
#' #' @param ... Additional arguments (currently unused)
#' #' @param focus Character. The primary axis of interest: "x" or "y". Gridlines
#' #'   and axis elements are removed from the opposite axis.
#' #' @param x_type Character. Type of x-axis: "continuous", "binned", or "discrete"
#' #' @param y_type Character. Type of y-axis: "continuous", "binned", or "discrete"
#' #' @param geom Character. Geom type (currently unused)
#' #'
#' #' @return A ggplot2 theme object
#' #' @export
#' polish_modern <- function(
#'     ...,
#'     focus = c("x", "y"),
#'     x_type = c("continuous", "binned", "discrete"),
#'     y_type = c("continuous", "binned", "discrete"),
#'     geom = NULL
#' ) {
#'
#'   theme <- ggplot2::theme()
#'
#'   if (focus == "x") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.line.y.left = ggplot2::element_line(linetype = 0),
#'         axis.line.y.right = ggplot2::element_line(linetype = 0),
#'         axis.ticks.y.left = ggplot2::element_line(linetype = 0),
#'         axis.ticks.y.right = ggplot2::element_line(linetype = 0),
#'         axis.minor.ticks.y.left = ggplot2::element_line(linetype = 0),
#'         axis.minor.ticks.y.right = ggplot2::element_line(linetype = 0),
#'         panel.grid.major.x = ggplot2::element_line(linetype = 0),
#'         panel.grid.minor.x = ggplot2::element_line(linetype = 0)
#'       )
#'   }
#'   if (focus == "y") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.line.x.bottom = ggplot2::element_line(linetype = 0),
#'         axis.line.x.top = ggplot2::element_line(linetype = 0),
#'         axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'         axis.ticks.x.top = ggplot2::element_line(linetype = 0),
#'         axis.minor.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'         axis.minor.ticks.x.top = ggplot2::element_line(linetype = 0),
#'         panel.grid.major.y = ggplot2::element_line(linetype = 0),
#'         panel.grid.minor.y = ggplot2::element_line(linetype = 0)
#'       )
#'   }
#'
#'   if (x_type == "discrete") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'         axis.ticks.x.top = ggplot2::element_line(linetype = 0),
#'       )
#'   }
#'   if (y_type == "discrete") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.ticks.y.left = ggplot2::element_line(linetype = 0),
#'         axis.ticks.y.right = ggplot2::element_line(linetype = 0),
#'       )
#'   }
#'
#'   return(theme)
#' }
#'
#' #' Classic polish
#' #'
#' #' Removes gridlines from the non-focused dimension. Also removes ticks on
#' #' discrete axes.
#' #'
#' #' @inheritParams polish_modern
#' #'
#' #' @return A ggplot2 theme object
#' #' @export
#' polish_classic <- function(
#'     ...,
#'     focus = c("x", "y"),
#'     x_type = c("continuous", "binned", "discrete"),
#'     y_type = c("continuous", "binned", "discrete"),
#'     geom = NULL
#' ) {
#'
#'   theme <- ggplot2::theme()
#'
#'   if (focus == "x") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         panel.grid.major.x = ggplot2::element_line(linetype = 0),
#'         panel.grid.minor.x = ggplot2::element_line(linetype = 0)
#'       )
#'   }
#'
#'   if (focus == "y") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         panel.grid.major.y = ggplot2::element_line(linetype = 0),
#'         panel.grid.minor.y = ggplot2::element_line(linetype = 0)
#'       )
#'   }
#'
#'   if (x_type == "discrete") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'         axis.ticks.x.top = ggplot2::element_line(linetype = 0),
#'       )
#'   }
#'   if (y_type == "discrete") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.ticks.y.left = ggplot2::element_line(linetype = 0),
#'         axis.ticks.y.right = ggplot2::element_line(linetype = 0),
#'       )
#'   }
#'
#'   return(theme)
#' }
#'
#' #' Science polish
#' #'
#' #' Removes gridlines and ticks from discrete axes.
#' #'
#' #' @inheritParams polish_modern
#' #'
#' #' @return A ggplot2 theme object
#' #' @export
#' polish_science <- function(
#'     ...,
#'     focus = c("x", "y"),
#'     x_type = c("continuous", "binned", "discrete"),
#'     y_type = c("continuous", "binned", "discrete"),
#'     geom = NULL
#' ) {
#'
#'   theme <- ggplot2::theme()
#'
#'   if (x_type == "discrete") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'         axis.ticks.x.top = ggplot2::element_line(linetype = 0),
#'         panel.grid.major.x = ggplot2::element_line(linetype = 0),
#'         panel.grid.minor.x = ggplot2::element_line(linetype = 0)
#'       )
#'   }
#'
#'   if (y_type == "discrete") {
#'     theme <- theme +
#'       ggplot2::theme(
#'         axis.ticks.y.left = ggplot2::element_line(linetype = 0),
#'         axis.ticks.y.right = ggplot2::element_line(linetype = 0),
#'         panel.grid.major.y = ggplot2::element_line(linetype = 0),
#'         panel.grid.minor.y = ggplot2::element_line(linetype = 0)
#'       )
#'   }
#'
#'   return(theme)
#' }
#'
#' #' Void polish
#' #'
#' #' Removes axes and gridlines.
#' #'
#' #' @inheritParams polish_modern
#' #'
#' #' @return A ggplot2 theme object
#' #' @export
#' polish_void <- function(
#'     ...,
#'     focus = c("x", "y"),
#'     x_type = c("continuous", "binned", "discrete"),
#'     y_type = c("continuous", "binned", "discrete"),
#'     geom = NULL
#' ) {
#'
#'   theme <- ggplot2::theme()
#'
#'   theme <- theme +
#'     ggplot2::theme(
#'       axis.line.x.bottom = ggplot2::element_line(linetype = 0),
#'       axis.line.x.top = ggplot2::element_line(linetype = 0),
#'       axis.line.y.left = ggplot2::element_line(linetype = 0),
#'       axis.line.y.right = ggplot2::element_line(linetype = 0),
#'       axis.ticks.x.top = ggplot2::element_line(linetype = 0),
#'       axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'       axis.ticks.y.left = ggplot2::element_line(linetype = 0),
#'       axis.ticks.y.right = ggplot2::element_line(linetype = 0),
#'       axis.minor.ticks.x.bottom = ggplot2::element_line(linetype = 0),
#'       axis.minor.ticks.x.top = ggplot2::element_line(linetype = 0),
#'       axis.minor.ticks.y.left = ggplot2::element_line(linetype = 0),
#'       axis.minor.ticks.y.right = ggplot2::element_line(linetype = 0),
#'
#'       axis.text.x.top = ggplot2::element_blank(),
#'       axis.text.x.bottom = ggplot2::element_blank(),
#'       axis.text.y.left = ggplot2::element_blank(),
#'       axis.text.y.right = ggplot2::element_blank(),
#'
#'       axis.title.x.top = ggplot2::element_blank(),
#'       axis.title.x.bottom = ggplot2::element_blank(),
#'       axis.title.y.left = ggplot2::element_blank(),
#'       axis.title.y.right = ggplot2::element_blank(),
#'
#'       panel.grid = ggplot2::element_line(linetype = 0),
#'       panel.grid.major = ggplot2::element_line(linetype = 0),
#'       panel.grid.minor = ggplot2::element_line(linetype = 0),
#'       panel.grid.major.x = ggplot2::element_line(linetype = 0),
#'       panel.grid.minor.x = ggplot2::element_line(linetype = 0),
#'       panel.grid.major.y = ggplot2::element_line(linetype = 0),
#'       panel.grid.minor.y = ggplot2::element_line(linetype = 0)
#'     )
#'
#'   return(theme)
#' }
#'
#' #' No polish
#' #'
#' #' Leaves the theme unchanged.
#' #'
#' #' @inheritParams polish_modern
#' #'
#' #' @return An empty ggplot2 theme object
#' #' @export
#' polish_none <- function(
#'     focus = c("x", "y"),
#'     x_type = c("continuous", "binned", "discrete"),
#'     y_type = c("continuous", "binned", "discrete"),
#'     geom = NULL
#' ) {
#'
#'   theme <- ggplot2::theme()
#'
#'   return(theme)
#' }

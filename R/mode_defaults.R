#' Mode colour and linewidth defaults
#'
#' @description
#' `lightness` and `darkness` are vectors of 3 colours used in the `*_theme_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' `linewidthness` is a vector of 2 integers used in the `*_theme_*` themes for the linewidth of the axis.line (axis.ticks and legend.ticks) and panel.grid theme elements.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(c(lightness, darkness), ncol = 3)
lightness <- c("#121B24FF", "#F6F8FAFF", "#FFFFFFFF")

#' @rdname lightness
#' @format NULL
#' @export
darkness <- c("#C8D7DFFF", "#00040AFF", "#050D1BFF")

#' @rdname lightness
#' @format NULL
#' @export
linewidthness <- c(0.33, 1.33)

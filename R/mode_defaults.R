#' Mode colour and linewidth defaults
#'
#' @description
#' `lightness`, `greyness` and `darkness` are vectors of 3 colours used in the `*_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' `linewidthness` is a vector of 2 integers used in the `*_mode_*` themes for the linewidth of the axis.line (axis.ticks and legend.ticks) and panel.grid theme elements.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(c(lightness, greyness, darkness), ncol = 3)
lightness <- c("#121b24ff", "#f6f8faff", "#ffffffff")

#' @rdname lightness
#' @format NULL
#' @export
greyness <- c("#121b24ff", "#f6f8faff", "#fcfdfeff")

#' @rdname lightness
#' @format NULL
#' @export
darkness <- c("#c8d7dfff", "#00040aff", "#050d1bff")

#' @rdname lightness
#' @format NULL
#' @export
linewidthness <- c(0.33, 1.33)

#' A blue colour
#'
#' @description A blue colour derived from `viridisLite::mako(9)[5]`
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(blue)
blue <- "#357ba2ff"

#' A teal colour
#'
#' @description A teal colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(teal)
teal <- "#0095a8ff"

#' A orange colour
#'
#' @description A orange colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(orange)
orange <- "#ffa600ff"

#' A navy colour
#'
#' @description A navy colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(navy)
# navy <- "#26506C"
navy <- "#003f5cff"

#' A red colour
#'
#' @description A red colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(red)
red <- "#da3c39ff"

#' A pink colour
#'
#' @description A pink colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(pink)
pink <- "#ec9ecbff"

#' A purple colour
#'
#' @description A purple colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(purple)
purple <- "#67609cff"

#' A categorical colour palette
#'
#' @description A categorical colour palette with 6 colours.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(jumble)
jumble <- c(teal, orange, navy, red, pink, purple)

#' The `light_mode_*` theme colours
#'
#' @description A vector of 3 colours used in the `light_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @return A character vector.
#' @noRd
#'
#' @examples
#' scales::show_col(lightness)
lightness <- c("#121b24ff", "#f6f8faff", "#ffffffff")

#' The `grey_mode_*` theme colours
#'
#' @description A vector of 3 colours used in the `grey_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @return A character vector.
#' @noRd
#'
#' @examples
#' scales::show_col(greyness)
greyness <- c("#121b24ff", "#f6f8faff", "#fcfdfeff")

#' The `dark_mode_*` theme colours
#'
#' @description A vector of 3 colours used in the `dark_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @return A character vector.
#' @noRd
#'
#' @examples
#' scales::show_col(darkness)
darkness <- c("#c8d7dfff", "#00040aff", "#050d1bff")

#' The `*_mode_*` linewidth values
#'
#' @description A vector of 2 integers used in the `*_mode_*` themes for the for the linewidth of the axis.line (axis.ticks and legend.ticks) and panel.grid theme elements.
#'
#' @return A numeric vector.
#' @noRd
#'
linewidthness <- c(0.33, 1.33)

#' A blue colour
#'
#' @description A blue colour derived from `viridisLite::mako(9)[5]`
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(blue)
blue <- "#357ba2"

#' A teal colour
#'
#' @description A teal colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(teal)
teal <- "#0095a8"

#' A orange colour
#'
#' @description A orange colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(orange)
orange <- "#ffa600"

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
navy <- "#003f5c"

#' A red colour
#'
#' @description A red colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(red)
red <- "#da3c39"

#' A pink colour
#'
#' @description A pink colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(pink)
pink <- "#ec9ecb"

#' A grey colour
#'
#' @description A grey colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(grey)
grey <- "#8fa8b4"

#' @description A categorical colour palette with 5 colours.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(vivid)
vivid <- c(teal, orange, navy, red, pink)

#' The `light_mode_*` theme colours
#'
#' @description A vector of 5 colours used in the `light_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(lightness)
lightness <- c(
  "#121b24",
  "#121b24",
  "#f6f8fa",
  "#ffffff",
  "#ffffff"
)

#' The `grey_mode_*` theme colours
#'
#' @description A vector of 5 colours used in the `grey_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(greyness)
greyness <- c(
  "#121b24",
  "#121b24",
  "#f6f8fa",
  "#fcfdfe",
  "#f6f8fa"
)

#' The `dark_mode_*` theme colours
#'
#' @description A vector of 5 colours used in the `dark_mode_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(darkness)
darkness <- c(
  "#c8d7df",
  "#c8d7df",
  "#00040a",
  "#050d1b",
  "#00040a"
)

#' The `*_mode_*` linewidth values
#'
#' @description A vector of 3 integers used in the `*_mode_*` themes for the for the linewidth of the axis.line (and axis.ticks), panel.grid and legend.ticks theme elements.
#'
#' @return A numeric vector.
#' @export
#'
linewidthness <- c(0.33, 1.33, 0.33)

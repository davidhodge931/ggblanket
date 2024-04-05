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
navy <- "#003f5c"

#' A pink colour
#'
#' @description A pink colour.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(pink)
pink <- "#bc5090"

#' The `light_mode_*` theme colours
#'
#' @description A vector of 5 colours used in the `light_mode_*` themes for the for the text, line, panel.grid, panel.background and plot.background.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(lightness)
lightness <- c(
  "#121b24", #text
  "#121b24", #line
  "#f6f8fa", #panel.grid
  "#ffffff", #panel.background
  "#ffffff" #plot.background
)

#' The `grey_mode_*` theme colours
#'
#' @description A vector of 5 colours used in the `grey_mode_*` themes for the for the text, line, panel.grid, panel.background and plot.background.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(greyness)
greyness <- c(
  "#121b24", #text
  "#121b24", #line
  "#f6f8fa", #panel.grid
  "#fcfdfe", #panel.background
  "#f6f8fa" #plot.background
)

#' The `dark_mode_*` theme colours
#'
#' @description A vector of 5 colours used in the `dark_mode_*` themes for the for the text, line, panel.grid, panel.background and plot.background.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(darkness)
darkness <- c(
  "#c8d7df", #text
  "#c8d7df", #line
  "#00040a", #panel.grid
  "#050d1b", #panel.background
  "#00040a" #plot.background
)

#' The `*_mode_*` linewidth values
#'
#' @description A vector of 2 integers used in the `*_mode_*` themes for the for the linewidth of the line/rect and panel.grid theme elements.
#'
#' @return A numeric vector.
#' @export
#'
linewidthness <- c(0.33, 1.33)

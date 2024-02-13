#' A blue colour
#'
#' @description A blue colour derived from `viridisLite::mako(9)[5]`
#'
#' @return A character vector.
#'
#' @export
blue <- "#357BA2"

#' A teal colour
#'
#' @description A teal colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
teal <- "#0095A8"

#' A orange colour
#'
#' @description A orange colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
orange <- "#FF7043"

#' A navy colour
#'
#' @description A navy colour derived from the US Census Bureau's Data Visualisation Standards. Note this colour is not accessible with dark modes.
#'
#' @return A character vector.
#'
#' @export
navy <- "#112E51"

#' A plum colour
#'
#' @description A plum colour inspired by the NZ tree fuchsia (kōtukutuku)
#'
#' @return A character vector.
#'
#' @export
plum <- "#901752"

#' Grey colours
#'
#' @description A vector of grey colours derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
greys <- c("#E8EFF2", "#C8D7DF", "#A7C0CD", "#78909C", "#4B636E", "#364850", "#222C31")

#' The `grey_mode_*` mode colours
#'
#' @noRd
greyness <- c(
  "text" = "#121B24",
  "axis_line" = "#121B24",
  "panel_background" = "#fCFDFE",
  "plot_background" = "#F6F8FA",
  "panel_grid" = "#F6F8FA"
)

#' The `light_mode_*` mode colours
#'
#' @description A vector of colours used in the `light_mode_*` modes.
#'
#' @noRd
lightness <- c(
  "text" = "#121B24",
  "axis_line" = "#121B24",
  "panel_background" = "#FFFFFF",
  "plot_background" = "#FFFFFF",
  "panel_grid" = "#F6F8FA"
)

#' The `dark_mode_*` mode colours
#'
#' @description A vector of colours used in the `dark_mode_*` modes.
#'
#' @noRd
darkness <- c(
  "text" = "#C8D7DF",
  "axis_line" = "#C8D7DF",
  "panel_background" = "#050D1B",
  "plot_background" = "#00040a",
  "panel_grid" = "#00040a"
)

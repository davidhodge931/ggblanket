#' A discrete palette inspired by the Guardian newspaper.
#'
#' @description A discrete palette of four colour-blind safe colours inspired by the Guardian website.
#'
#' @return A character vector.
#'
#' @export
#' @references Colours inspired by the Guardian website. scales::pal_hue otherwise.
pal_guardian <-c("#2596be", "#fc7c24", "#9c1e74", "#6b5840")

#' A 5 colour vector for use with the light_mode_* themes to colour the (1) plot background (2) panel background, (3) axis lines (and ticks) (4) panel grid and (5) text.
#'
#' @description A 5 colour vector for use with the light_mode_* themes.
#'
#' @return A character vector.
#'
#' @export
pal_light_mode <- c(
  "plot_background" = "#e6ecf2",
  "panel_background" = "#fcfdfe",
  "axis_line" = "#121b24",
  "gridlines" = "#dbe1e7",
  "text" = "#121b24"
)

#' A 5 colour vector for use with the dark_mode_* themes to colour the (1) plot background (2) panel background, (3) axis lines (and ticks) (4) panel grid and (5) text.
#'
#' @description A 5 colour vector for use with the dark_mode_* themes.
#'
#' @return A character vector.
#'
#' @export
pal_dark_mode <- c(
  "plot_background" = "#15202b",
  "panel_background" = "#1f2f3e",
  "axis_line" = "#bbccdd",
  "gridlines" = "#2c3a48",
  "text" = "#bbccdd"
)

#' Default colours used to colour a discrete variable.
#'
#' @description Default colours used to colour a discrete variable. Uses a colour blind safe palette inspired by the Guardian newspaper for 4 or less colours. For 5 or more colours, uses scales::pal_hue.
#'
#' @return A character vector.
#'
#' @keywords internal
#' @references Colours inspired by the Guardian website. scales::pal_hue otherwise.
pal_discrete <- function(n = 4) {
  if (n <= 4) c("#2596be", "#fc7c24", "#9c1e74", "#6b5840")[1:n]
  else scales::pal_hue()(n)
}

#' Default colours used to colour a continuous variable.
#'
#' @description Default colours used to colour a continuous variable.
#'
#' @return A character vector.
#'
#' @keywords internal
#' @references The mako colour palette from viridisLite reversed
pal_continuous <- function(n = 20) {
  viridisLite::mako(n = n, direction = -1, end = )
}

#' Default colour for no col aesthetic
#'
#' @description Default colour for no col aesthetic i.e. `viridisLite::mako(9)[5]`
#'
#' @return A character vector.
#'
#' @keywords internal
pal_none <- function() {
  "#357BA2"
}

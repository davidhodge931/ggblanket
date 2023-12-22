#' A blue colour
#'
#' @description A blue colour derived from `viridisLite::mako(9)[5]`
#'
#' @return A character vector.
#'
#' @export
pal_blue <- "#357BA2"

#' A teal colour
#'
#' @description A teal colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
pal_teal <- "#0095A8"

#' A orange colour
#'
#' @description A orange colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
pal_orange <- "#FF7043"

#' A navy colour
#'
#' @description A navy colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
pal_navy <- "#112E51"

#' A plum colour
#'
#' @description A plum colour inspired by the NZ tree fuchsia (kōtukutuku)
#'
#' @return A character vector.
#'
#' @export
pal_plum <- "#901752"

#' A grey colour
#'
#' @description A grey colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
pal_grey <- "#78909C"

#' The light_mode_* theme colours
#'
#' @description A 5 colour vector for use with the light_mode_* themes to colour the (1) text (2) plot background etc (3) panel background etc (4) axis line/ticks (5) and gridlines.
#'
#' @return A character vector.
#'
#' @export
pal_light_mode <- c(
  "text" = "#121b24",
  "plot" = "#e6ecf2",
  "panel" = "#fcfdfe",
  "axislines" = "#121b24",
  "gridlines" = "#dbe1e7"
)

#' The dark_mode_* theme colours
#'
#' @description A 5 colour vector for use with the dark_mode_* themes to colour the (1) text (2) plot background etc (3) panel background etc (4) axis line/ticks (5) and gridlines.
#'
#' @return A character vector.
#'
#' @export
pal_dark_mode <- c(
  "text" = "#bbccdd",
  "plot" = "#15202b",
  "panel" = "#1f2f3e",
  "axislines" = "#bbccdd",
  "gridlines" = "#2c3a48"
)

#' Default colours used to colour a discrete variable
#'
#' @description Default colours used to colour a discrete variable. Uses a colour blind safe palette derived from the US Census Bureau's Data Visualisation Standards for 3 or less colours. For 4 or more colours, uses scales::pal_hue.
#'
#' @return A character vector.
#'
#' @keywords internal
pal_discrete <- function(n = 3) {
  # if (n <= 4) c("#2596be", "#fc7c24", "#9c1e74", "#6b5840")[1:n]
  if (n <= 3) c(pal_teal, pal_orange, pal_plum)[1:n]
  else scales::pal_hue()(n)
}

#' Default colours used to colour a continuous variable
#'
#' @description Default colours used to colour a continuous variable.
#'
#' @return A character vector.
#'
#' @keywords internal
#' @references The mako colour palette from viridisLite reversed
pal_continuous <- function(n = 18) {
  viridisLite::mako(n = n, direction = -1)
}

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
#' @description A navy colour derived from the US Census Bureau's Data Visualisation Standards.
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

#' A grey colour
#'
#' @description A grey colour derived from the US Census Bureau's Data Visualisation Standards.
#'
#' @return A character vector.
#'
#' @export
grey <- "#78909C"

#' The `light_mode_*` theme colours
#'
#' @description A 5 colour vector for use with the `light_mode_*` themes to colour the (1) plot background etc (2) panel background etc, (3) gridlines, (4) axisline/ticks.(5) and text.
#'
#' @return A character vector.
#'
#' @export
pal_light_mode <- c(
  "plot" = "#e6ecf2",
  "panel" = "#fcfdfe",
  "gridlines" = "#e6ecf2",
  "axisline" = "#121b24",
  "text" = "#121b24"
)

#' The `dark_mode_*` theme colours
#'
#' @description A 5 colour vector for use with the `dark_mode_*` themes to colour the (1) plot background etc (2) panel background etc, (3) gridlines, (4) axisline/ticks.(5) and text.
#'
#' @return A character vector.
#'_
#' @export
pal_dark_mode <- c(
  "plot" = "#15202b",
  "panel" = "#1f2f3e",
  "gridlines" = "#15202b",
  "axisline" = "#bbccdd",
  "text" = "#bbccdd"
)

#' The `light_mode_*` theme colours
#'
#' @description A 3 colour vector used in the `light_mode_*` themes.
#'
#' @return A character vector.
#'_
#' @export
lightness <- c("#121b24", "#fcfdfe", "#e6ecf2")

#' The `dark_mode_*` theme colours
#'
#' @description A 3 colour vector used in the `dark_mode_*` themes.
#'
#' @return A character vector.
#'_
#' @export
darkness <- c("#bbccdd", "#1f2f3e", "#15202b")

#' Default colours used to colour a discrete variable
#'
#' @description Default colours used to colour a discrete variable. Uses a colour blind safe palette derived from the US Census Bureau's Data Visualisation Standards for 3 or less colours. For 4 or more colours, uses scales::pal_hue.
#'
#' @return A character vector.
#'
#' @noRd
col_pal_discrete <- function(n = 3) {
  if (n == 1) blue
  else if (n <= 3) c(teal, orange, plum)[1:n]
  else scales::pal_hue()(n)
}

#' Default colours used to colour a continuous variable
#'
#' @description Default colours used to colour a continuous variable.
#'
#' @return A character vector.
#'
#' @noRd
#' @references The mako colour palette from viridisLite reversed
col_pal_continuous <- function(n = 18) {
  viridisLite::mako(n = n, direction = -1)
}

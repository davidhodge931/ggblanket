#' A discrete palette inspired by the Guardian newspaper.
#'
#' @description A discrete palette of four colour-blind safe colours inspired by the Guardian website.
#'
#' @return A character vector.
#'
#' @export
#' @references Colours inspired by the Guardian website. scales::pal_hue otherwise.
pal_guardian <-c("#2596be", "#fc7c24", "#9c1e74", "#6b5840")

#' Default colours used in the light_mode theme.
#'
#' @description Default colours used in the light_mode theme for the (1) base text and axis, (2) plot background, (3) panel background and (4) gridlines.
#'
#' @return A character vector.
#'
#' @keywords internal
pal_light_mode <- c("#121b24", "#e6ecf2", "#fcfdfe", "#dbe1e7")

#' Default colours used in the dark_mode theme.
#'
#' @description Default colours used in the dark_mode theme for the (1) base text and axis, (2) plot background, (3) panel background and (4) gridlines.
#'
#' @return A character vector.
#'
#' @keywords internal
pal_dark_mode <- c("#bbccdd", "#15202b", "#1f2f3e", "#2c3a48")

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
#' @description Default colour for no col aesthetic i.e. viridisLite::mako(9)[5]
#'
#' @return A character vector.
#'
#' @keywords internal
pal_none <- function() {
  "#357BA2"
}

#' Colour blind safe categorical palette with 4 colours
#'
#' @description A colour blind safe categorical palette inspired by the Guardian newspaper.
#'
#' @param n The number of colours to return. Defaults to the maximum of 4.
#'
#' @return A character vector of hex codes.
#'
#' @export
#'
#' @examples
#' scales::show_col(guardian())
#'
guardian <- function(n = 4) {
  if (n <= 4) c("#2596be", "#fc7c24", "#6b5840", "#9c1e74")[1:n]
  else rlang::abort("guardian provides a maximum of 4 colours")
}

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

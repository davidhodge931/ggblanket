#' Colour blind safe categorical palette with 5 colours
#'
#' @description A default colour blind safe 5 colour palette used to colour a categorical col variable.
#'
#' @param n The number of colours to return. Defaults to the maximum of 5.
#'
#' @return A character vector of hex codes.
#'
#' @export
#'
#' @references Derived from a Datawrapper blog dated 30/03/2022 by Lisa Charlotte Muth.
jumble <- function(n = 5) {
  if (n > 6) rlang::abort("jumble provides a maximum of 6 colours")
  else {
    c("#2596be", "#fc7c24", "#125162", "#b82e2e", "#009e73")[1:n]
  }
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

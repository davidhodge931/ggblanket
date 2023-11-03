#' Default colour blind safe categorical palette with 5 colours
#'
#' @description A default colour blind safe 5 colour palette used to colour a categorical col variable.
#'
#' @return A character vector of hex codes.
#'
#' @export
#'
#' @references Derived from a Datawrapper blog dated 30/03/2022 by Lisa Charlotte Muth.
jumble <- function(n = 5) {
  if (n == 1) pal <- pal_discrete[4]
  else if (n < 6) pal <- pal_discrete
  else if (n == 6) pal <- c(pal_discrete, "#0c2052")
  else if (n > 6) rlang::abort("jumble provides a maximum of 6 colours")

  # if (direction == -1) pal <- rev(pal)
  pal <- pal[1:n]

  return(pal)
}

#' Default colour blind safe categorical palette with 5 colours
#'
#' @description The default colour blind safe 5 colour palette used to colour a categorical col variable.
#'
#' @return A character vector of hex codes.
#'
#' @keywords internal
#'
#' @references Derived from a Datawrapper blog dated 30/03/2022 by Lisa Charlotte Muth.
pal_discrete <- c("#69c2c0", "#941111", "#b2c615", "#2b6999", "#d57236", "#0c2052")

#' Default blue colour
#'
#' @description A default blue colour used to colour when there is no col variable.
#'
#' @return A character vector.
#'
#' @keywords internal
pal_blue <- "#2b6999"

#' Default grey colour
#'
#' @description The default grey colour used to colour NA values.
#'
#' @return A character vector.
#'
#' @keywords internal
pal_grey <- "#7f7f7f"

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

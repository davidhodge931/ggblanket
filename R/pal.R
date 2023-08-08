#' Default blue colour.
#'
#' @description A default blue colour used to colour when there is no col variable.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_blue)
pal_blue <- "#2B6999"

#' Default categorical palette.
#'
#' @description A default colour blind safe 5 colour palette used to colour a categorical col variable.
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_discrete)
#'
#' @references Derived from a Datawrapper blog dated 30/03/2022 by Lisa Charlotte Muth.
pal_discrete <- c("#69C2C0", "#941111", "#B2C615", "#2B6999", "#D57236")

#' Default grey colour.
#'
#' @description A default grey colour used to colour NA values.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_grey)
pal_grey <- "#7f7f7f"

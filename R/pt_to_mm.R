#' Convert pt to mm
#'
#' @description
#' Convert pt to mm. Intended for use with the size argument in weave_geom_text, gg_text, or geom_text - and label equivalents.
#' The conversion uses the standard typographic conversion factor where 1 point = 25.4 / 72 ≈ 0.353 mm.
#'
#' @param pt Numeric vector of font sizes in points.
#'
#' @return Numeric vector in mm.
#'
#' @examples
#' pt_to_mm(11)  # Convert 11 pt to mm
#' pt_to_mm(15)  # Convert 15 pt to mm
#'
#' @export
#'
pt_to_mm <- function(pt) {
  pt * 25.4 / 72
}

#' Convert pt to mm
#'
#' @param pt Numeric vector of font sizes in points.
#'
#' @return Numeric vector of font sizes in millimeters.
#'
#' @details
#' The conversion uses the standard typographic conversion factor where
#' 1 point = 25.4/72 millimeters ≈ 0.353 mm.
#'
#' @examples
#' pt_to_mm(11)  # Convert 11 pt to mm
#' pt_to_mm(15)  # Convert 15 pt to mm
#'
#' # Use with weave geom functions, or in gg_text (or geom_text)
#' weave_geom_label(size = pt_to_mm(12))
#'
#' @export
pt_to_mm <- function(pt) {
  pt * 25.4 / 72
}

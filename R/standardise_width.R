#' Standardise width
#'
#' Calculate consistent bar widths across plots with different numbers of categories.
#' This ensures bars have the same physical width regardless of how many bars are in the plot.
#' Save plots with uniform panel dimensions.
#'
#' Note: intended for polygons.
#' Does not work with:
#' * panel.sizes of different dimensions
#' * geoms that are dodged etc.
#'
#' @param n Number of categories.
#' @param n_dodge Number of groups dodged.
#' @param reference_n Number of categories in the reference plot. Defaults to 3.
#' @param reference_n_dodge Number of groups dodged.in the reference plot. Defaults to 1.
#' @param reference_width Width value in the reference plot. Defaults to 0.2.
#'
#' @returns A numeric value
#' @noRd
#'
#' @examples
#' # For a plot with 2 bars, standardised to a 3-bar reference with 0.2 width
#' standardise_width(2)
standardise_width <- function(n,
                              n_dodge = 1,
                              reference_n = 3,
                              reference_n_dodge = 1,
                              reference_width = 0.5) {

  # Base category scaling
  base_width <- (n / reference_n) * reference_width

  # Dodge scaling: maintain consistent individual bar width
  if (n_dodge > 1 || reference_n_dodge > 1) {
    width <- base_width * (n_dodge / reference_n_dodge)
  } else {
    width <- base_width
  }

  if (width >= 1)
    rlang::abort("width cannot be greater than or equal to 1")
  return(width)
}

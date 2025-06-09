#' Standardise width
#'
#' Calculate consistent bar widths across plots with different numbers of categories.
#' This ensures bars have the same physical width regardless of how many bars are in the plot.
#' Save plots with uniform panel dimensions.
#'
#' Note: intended for polygons.
#' Works with position_dodge and position_dodge2.
#'
#' @param n Number of categories.
#' @param n_dodge Number of groups dodged.
#' @param padding Padding for position_dodge2. Defaults to 0.
#' @param reference_n Number of categories in the reference plot. Defaults to 3.
#' @param reference_n_dodge Number of groups dodged in the reference plot. Defaults to 1.
#' @param reference_width Width value in the reference plot. Defaults to 0.5.
#' @param reference_padding Padding for position_dodge2 in the reference plot. Defaults to 0.
#'
#' @returns A numeric value
#' @noRd
#'
#' @examples
#' # For a plot with 2 bars, standardised to a 3-bar reference with 0.5 width
#' standardise_width(2)
#'
#' # For dodged bars
#' standardise_width(n = 3, n_dodge = 2)
#'
#' # For position_dodge2 with padding
#' standardise_width(n = 3, n_dodge = 2, padding = 0.1)
standardise_width <- function(n,
                              n_dodge = 1,
                              padding = 0,
                              reference_n = 3,
                              reference_n_dodge = 1,
                              reference_width = 0.5,
                              reference_padding = 0) {
  # Base category scaling
  base_width <- (n / reference_n) * reference_width

  # Dodge scaling: maintain consistent individual bar width
  if (n_dodge > 1 || reference_n_dodge > 1) {
    width <- base_width * (n_dodge / reference_n_dodge)
  } else {
    width <- base_width
  }

  # Padding adjustment for position_dodge2
  if (padding > 0 || reference_padding > 0) {
    # Adjust for difference in padding - more padding requires wider total width
    padding_factor <- (1 + padding) / (1 + reference_padding)
    width <- width * padding_factor
  }

  if (width >= 1)
    rlang::abort("width cannot be greater than or equal to 1")
  return(width)
}

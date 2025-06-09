#' Standardise width
#'
#' Calculate widths that are standardised.
#'
#' @param n Number of x aesthetic groups.
#' @param dodge_n Number of fill aesthetic etc groups dodged.
#' @param dodge_padding Amount of padding (in position_dodge2). Defaults to 0.
#' @param reference_width In the reference, width value. Defaults to 0.5.
#' @param reference_n In the reference, number of x aesthetic groups. Defaults to 3.
#' @param reference_dodge_n In the reference, number of fill aesthetic etc groups dodged. Defaults to 1.
#' @param reference_dodge_padding In the reference, amount of padding (in position_dodge2). Defaults to 0.
#'
#' @returns A numeric value
#' @noRd
standardise_width <- function(n,
                              dodge_n = 1,
                              dodge_padding = 0,
                              reference_width = 0.5,
                              reference_n = 3,
                              reference_dodge_n = 1,
                              reference_dodge_padding = 0) {
  # Base category scaling
  base_width <- (n / reference_n) * reference_width

  # Dodge scaling: maintain consistent individual bar width
  if (dodge_n > 1 || reference_dodge_n > 1) {
    width <- base_width * (dodge_n / reference_dodge_n)
  } else {
    width <- base_width
  }

  # dodge_padding adjustment for position_dodge2
  if (dodge_padding > 0 || reference_dodge_padding > 0) {
    # Adjust for difference in dodge_padding - more dodge_padding requires wider total width
    dodge_padding_factor <- (1 + dodge_padding) / (1 + reference_dodge_padding)
    width <- width * dodge_padding_factor
  }

  if (width >= 1)
    rlang::abort("width cannot be greater than or equal to 1")
  return(width)
}

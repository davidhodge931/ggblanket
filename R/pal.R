#' Default ggblanket palette.
#'
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#'
#' @param n The number of colours (excluding an NA colour).
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_default(1))
#' scales::show_col(pal_default())
#' @references
#' https://blog.datawrapper.de/colors-for-data-vis-style-guides/
pal_default <- function(n = 7) {

  if (n == 1) {
    "#2B6999"
  }
  else {
    c("#53B0AE", "#A31414", "#B2C615", "#E37000", "#2B6999", "#ff4676", "#14a35c")[1:n]
  }
}

#' NA palette.
#'
#' @description A function to retreive a hex code for a colour to use for NA values.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_na_default())
#' @references
#' https://blog.datawrapper.de/colors-for-data-vis-style-guides/
pal_na_default <- function() {
  "#88837D"
}

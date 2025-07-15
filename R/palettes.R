#' Blue
#'
#' @description A blue colour.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(blue)
blue <- "#357BA2FF"

#' Silver
#'
#' @description A silver colour.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(silver)
silver <- "#8991A1FF"

#' Grey
#'
#' @description A grey colour.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(grey)
grey <- "#A6A6A6FF"

#' The jumble palette
#'
#' @description
#' A discrete palette that is relatively colour-blind safe.
#'
#' For use on a lighter background.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' colorspace::swatchplot(jumble, cvd = TRUE)
jumble <- c(
  "#0095A8FF",
  "#FFA600FF",
  "#003F5CFF",
  "#DA3C39FF",
  "#EC9ECBFF",
  "#67609CFF",
  "#CDC5BFFF"
)

#' Mode colour and linewidth defaults
#'
#' @description
#' `lightness` and `darkness` are vectors of 3 colours used in the `*_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @format NULL
#' @return A character vector.
#' @noRd
#'
#' @examples
#' scales::show_col(c(lightness, darkness), ncol = 3)
lightness <- c("#121B24FF", "#F6F8FAFF", "#FFFFFFFF")

#' @rdname lightness
#' @format NULL
#' @noRd
darkness <- c("#C8D7DFFF", "#00040AFF", "#050D1BFF")

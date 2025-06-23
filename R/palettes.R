#' A blue colour
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

#' A grey colour
#'
#' @description A grey colour.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(grey)
grey <- "#CDC5BFFF"

#' The jumble palette
#'
#' @description
#' A discrete palette that is relatively colour-blind safe.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' colorspace::swatchplot(c(jumble, grey), cvd = TRUE)
jumble <- c(
  "#0095A8FF",
  "#FFA600FF",
  "#003F5CFF",
  "#DA3C39FF",
  "#EC9ECBFF",
  "#67609CFF"
)

#' @rdname jumble
#' @format NULL
#' @export
teal <- "#0095A8FF"

#' @rdname jumble
#' @format NULL
#' @export
orange <- "#FFA600FF"

#' @rdname jumble
#' @format NULL
#' @export
navy <- "#003F5CFF"

#' @rdname jumble
#' @format NULL
#' @export
red <- "#DA3C39FF"

#' @rdname jumble
#' @format NULL
#' @export
pink <- "#EC9ECBFF"

#' @rdname jumble
#' @format NULL
#' @export
purple <- "#67609CFF"

#' Mode colour and linewidth defaults
#'
#' @description
#' `lightness` and `darkness` are vectors of 3 colours used in the `*_*` themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.
#'
#' @format NULL
#' @return A character vector.
#' @export
#'
#' @examples
#' scales::show_col(c(lightness, darkness), ncol = 3)
lightness <- c("#121B24FF", "#F6F8FAFF", "#FFFFFFFF")

#' @rdname lightness
#' @format NULL
#' @export
darkness <- c("#C8D7DFFF", "#00040AFF", "#050D1BFF")

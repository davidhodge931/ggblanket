#' Label every nth element
#'
#' @description
#' Label every nth element in a vector, and replace the rest with "".
#'
#' @param ... If numeric, arguments passed to the `scales::comma` function. Otherwise, arguments passed to `format`. Require named arguments (and support trailing commas).
#' @param n The increment of elements to hold as is. Defaults to `2`.
#' @param offset An offset for which element to first hold. Defaults to `0`. Possible values are `-1` to (`n - 2`)
#'
#' @return A labelling function
#' @export
#'
#' @examples
#' set_blanket()
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     facet = species,
#'     y_breaks_n = 20,
#'     y_labels = label_every_nth(),
#'   )
#
label_every_nth <- function(..., n = 2, offset = 0) {
  function(x) {
    i <- which(is.finite(x) | is.character(x) | is.factor(x) | is.logical(x))
    i <- i[seq_along(i) %% n == (offset + 1)]

    if (is.numeric(x)) {
      x <- scales::comma(x, ...)
    } else {
      x <- format(x, ...)
    }

    x[-i] <- ""
    x
  }
}

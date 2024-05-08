#' Hold by every nth element
#'
#' @description
#' Hold by every nth element in a vector, and replace the rest with "".
#'
#' @param nth The increment of elements to hold as is. Defaults to `2`.
#' @param offset An offset for which element to first hold. Defaults to `0`. Possible values are `-1` to (`nth - 2`)
#' @param ... If numeric, other arguments passed to the `scales::comma` function.
#'
#' @return A character vector
#' @noRd
#'
#' @examples
#' hold_by_nth()scales::comma(seq(1000, 5000, 1000))
#' hold_by_nth()format(lubridate::ymd(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")))
#' hold_by_nth()LETTERS[1:12]
#'
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'     y_labels = hold_by_nth(),
#'   )
#
hold_by_nth <- function(nth = 2, offset = 0, ...) {
  force(nth)
  force(offset)
  function(x) {
    i <- which(is.finite(x) | is.character(x) | is.factor(x) | is.logical(x))
    i <- i[seq_along(i) %% nth == (offset + 1)]
    if (is.numeric(x)) x <- scales::comma(x, ...)
    else x <- as.character(x)
    x[-i] <- ""
    x
  }
}

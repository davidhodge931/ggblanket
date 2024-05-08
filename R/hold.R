#' Hold every nth element
#'
#' @description
#' Hold every nth element in a vector, and replace the rest with "".
#'
#' @param x A vector.
#' @param nth The increment of elements to hold as is. Defaults to `2`.
#' @param offset An offset for which element to first hold. Defaults to `0`. Possible values are `-1` to (`nth - 2`)
#'
#' @return A character vector
#' @export
#'
#' @examples
#' hold_nth(scales::comma(seq(1000, 5000, 1000)))
#' hold_nth(format(lubridate::ymd(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"))))
#' hold_nth(LETTERS[1:12])
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
#'     y_labels = \(x) hold_nth(scales::comma(x)),
#'   )
#'
hold_nth <- function(x,
                     nth = 2,
                     offset = 0) {

  replace(x, seq_along(x) %% nth != (offset + 1), "")
}

#' #' Hold every nth element
#' #'
#' #' @description
#' #' Hold every nth element in a vector, and replace the rest with "".
#' #'
#' #' @param n The increment of elements to hold as is. Defaults to `2`.
#' #' @param offset An offset for which element to first hold. Defaults to `0`. Possible values are `-1` to (`n - 2`)
#' #' @param ... If numeric, arguments passed to the `scales::comma` function. Otherwise, arguments passed to `format`.
#' #'
#' #' @return A labelling function
#' #' @export
#' #'
#' #' @examples
#' #'  label_every_nth()(scales::comma(seq(1000, 5000, 1000)))
#' #'  label_every_nth()(lubridate::ymd(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")))
#' #'  label_every_nth()(LETTERS[1:12])
#' #'
#' #'  library(dplyr)
#' #'  library(palmerpenguins)
#' #'
#' #'  set_blanket()
#' #'
#' #'  penguins |>
#' #'    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
#' #'    gg_point(
#' #'      x = flipper_length_mm,
#' #'      y = body_mass_g,
#' #'      col = sex,
#' #'      x_labels = label_every_nth(),
#' #'      y_labels = label_every_nth(),
#' #'    )
#' #
#' label_every_nth <- function(n = 2, offset = 0, ...) {
#'   function(x) {
#'     i <- which(is.finite(x) | is.character(x) | is.factor(x) | is.logical(x))
#'     i <- i[seq_along(i) %% n == (offset + 1)]
#'
#'     if (is.numeric(x)) x <- scales::comma(x, ...)
#'     else x <- format(x, ...)
#'
#'     x[-i] <- ""
#'     x
#'   }
#' }

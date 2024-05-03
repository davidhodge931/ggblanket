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
#' @noRd
#'
#' @examples
#' hold_nth(scales::comma(seq(1000, 5000, 1000)))
#' hold_nth(format(lubridate::ymd(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"))))
#' hold_nth(LETTERS[1:12])
hold_nth <- function(x,
                     nth = 2,
                     offset = 0) {

  replace(x, seq_along(x) %% nth != (offset + 1), "")
}

#' Hold the range of elements
#'
#' @description
#' Hold the range of elements in a vector, and replace the rest with "".
#'
#' @param x A vector.
#'
#' @return A character vector
#' @noRd
#'
#' @examples
#' hold_range(scales::comma(seq(1000, 5000, 1000)))
#' hold_range(format(lubridate::ymd(c("2021-01-01", "2022-01-01", "2023-01-01"))))
#' hold_range(LETTERS[1:12])
hold_range <- function(x) {

  c(as.character(x[1]),
    rep("", times = length(x) - 2),
    as.character(x[length(x)])
  )
}

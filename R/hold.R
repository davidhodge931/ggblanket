#' Hold every nth element
#'
#' @description
#' Hold every nth element in a vector, and replace the rest with a value such as "".
#'
#' @param x A vector.
#' @param ... If numeric, other arguments passed to the `scales::comma` function.
#' @param nth The increment of elements to hold as is. Defaults to `2`.
#' @param offset An offset for which element to first hold. Defaults to `0`. Possible values are `-1` to (`nth - 2`)
#' @param replacement The replacement value to replace non-held elements with. Defaults to `""`.
#'
#' @return A character vector
#' @noRd
#'
#' @examples
#' hold_nth(seq(1000, 7000, 1000))
#' hold_nth(LETTERS[1:12])
#'
hold_nth <- function(x,
                     ...,
                     nth = 2,
                     offset = 0,
                     replacement = "") {

  if (is.numeric(x)) {
    replace(scales::comma(x, ...),
            seq_along(scales::comma(x, ...))
            %% nth != (offset + 1), replacement)
  }
  else if (lubridate::is.Date(x) | lubridate::is.POSIXct(x) | hms::is_hms(x)) {
    replace(as.character(x), seq_along(as.character(x)) %% nth != (offset + 1), replacement)
  }
  else {
    replace(x, seq_along(x) %% nth != (offset + 1), replacement)
  }
}

#' Hold the range of elements
#'
#' @description
#' Hold the range of elements in a vector, and replace the rest with a value such as "".
#'
#' @param x A vector.
#' @param ... If numeric, other arguments passed to the `scales::comma` function.
#' @param replacement The replacement value to replace non-held elements with. Defaults to `""`.
#'
#' @return A character vector
#' @noRd
#'
#' @examples
#' hold_range(seq(1000, 7000, 1000))
#' hold_range(LETTERS[1:12])
#'
hold_range <- function(x,
                       ...,
                       replacement = "") {

  if (is.numeric(x)) {
    c(as.character(sort(scales::comma(x, ...))[1]),
      rep(replacement, times = length(x) - 2),
      as.character(sort(scales::comma(x, ...))[length(x)])
    )
  }
  else {
    c(as.character(sort(x)[1]),
      rep(replacement, times = length(x) - 2),
      as.character(sort(x)[length(x)])
    )
  }
}

#' Replace a sequence of elements in a vector
#'
#' @description Keep every nth element in a vector, and replace the rest with a value such as `""`.
#'
#' @param x A vector.
#' @param ... If numeric, other arguments passed to the `scales::comma` function.
#' @param keep_nth The increment of elements to keep as is. Defaults to `2`.
#' @param offset An offset to start at the intended offset. Defaults to `0`. Possible replaces are `-1` to (`keep_nth - 2`)
#' @param replacement The replacement value to replace non-kept elements with. Defaults to `""`.
#'
#' @return A vector.
#' @export
#'
#' @examples
#' x <- LETTERS[1:12]
#' replace_seq(x)
#' replace_seq(x, offset = -1)
#' replace_seq(x, keep_nth = 3)
#'
replace_seq <- function(x,
                        ...,
                        keep_nth = 2,
                        offset = 0,
                        replacement = "") {

  if (is.numeric(x)) {
    replace(scales::comma(x, ...),
            seq_along(scales::comma(x, ...))
            %% keep_nth != (offset + 1), replacement)
  }
  else if (lubridate::is.Date(x) | lubridate::is.POSIXct(x) | hms::is_hms(x)) {
    replace(as.character(x), seq_along(as.character(x)) %% keep_nth != (offset + 1), replacement)
  }
  else {
    replace(x, seq_along(x) %% keep_nth != (offset + 1), replacement)
  }
}

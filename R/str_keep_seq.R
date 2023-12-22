#' Keep elements in a vector sequence, and replace the rest with ""
#'
#' @param x A vector.
#' @param ... If numeric, other arguments passed to the scales::comma function.
#' @param by The increment of elements to keep. Defaults to 2.
#' @param offset An offset to start at the intended offset. Defaults to 0. Possible values are -1 to (`by` - 2)
#'
#' @return A vector.
#' @export
#'
#' @examples
#' x <- LETTERS[1:12]
#' str_keep_seq(x)
#' str_keep_seq(x, offset = -1)
#' str_keep_seq(x, by = 3)
#'
str_keep_seq <- function(x,
                         ...,
                         by = 2,
                         offset = 0) {
  if (is.numeric(x)) {
    replace(scales::comma(x, ...),
            seq_along(scales::comma(x, ...))
            %% by != (offset + 1), "")
  }
  else if (lubridate::is.Date(x) | lubridate::is.POSIXct(x) | hms::is_hms(x)) {
    replace(as.character(x), seq_along(as.character(x)) %% by != (offset + 1), "")
  }
  else {
    replace(x, seq_along(x) %% by != (offset + 1), "")
  }
}

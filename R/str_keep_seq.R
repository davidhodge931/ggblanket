#' Keep elements in a vector sequence, and replace the rest with ""
#'
#' @param x A vector.
#' @param by The increment of elements to keep. Defaults to 2.
#' @param offset An offset to start at the intended offset. Defaults to 0. Possible values are -1 to (`by` - 2)
#' @param ... If numeric, other arguments passed to the scales::comma function.
#'
#' @return A vector.
#' @export
#'
#' @examples
#' x <- LETTERS[1:12]
#' str_keep_seq(x)
#' str_keep_seq(x, offset = -1)
#' str_keep_seq(x, by = 3)
#' str_keep_seq(x, by = 3, offset = -1)
#' str_keep_seq(x, by = 3, offset = 1)
#'
#' y <- c(1000, 2000, 3000)
#' str_keep_seq(y)
#' str_keep_seq(y, big.mark = " ")
#'
#' z <- c(0.0, 0.25, 0.5, 0.75, 1)
#' str_keep_seq(z)
#' str_keep_seq(z, drop0trailing = TRUE)
#'
#' str_keep_seq(format(ggplot2::economics$date[1:6], "%Y-%m"))
#'
#' library(palmerpenguins)
#' penguins |>
#'  gg_jitter(x = species,
#'            y = body_mass_g,
#'            col = flipper_length_mm,
#'            facet = island,
#'            facet2 = species,
#'            y_breaks = scales::breaks_pretty(n = 10),
#'            y_labels = \(x) str_keep_seq(x, by = 2),
#'            x_labels = \(x) str_keep_seq(x, by = 2),
#'            col_labels = \(x) str_keep_seq(x, by = 2, offset = -1))
#'
str_keep_seq <- function(x,
                         by = 2,
                         offset = 0,
                         ...
) {
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

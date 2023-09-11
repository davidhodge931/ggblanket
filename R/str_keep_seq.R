#' Keep elements of a vector in a sequence, and replace the rest with ""
#'
#' @param x A vector.
#' @param by The increment of elements to keep. Defaults to 2.
#' @param offset An offset to start at the intended offset. Defaults to 0. Possible values are -1 to (`by` - 2)
#' @param big.mark If numeric, the character used between every 3 digits to seperate thousands. Defaults to ",".
#' @param ... If numeric, other arguments passed to the scales::number function.
#'
#' @return A vector.
#' @export
#'
#' @examples
#' x <- LETTERS[1:12]
#' str_keep_seq(x, by = 2, offset = 0)
#' str_keep_seq(x, by = 2, offset = -1)
#' str_keep_seq(x, by = 3, offset = 0)
#' str_keep_seq(x, by = 3, offset = -1)
#' str_keep_seq(x, by = 3, offset = 1)
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
                         offset = 0, #possible numbers -1 to (by - 2)
                         big.mark = ",",
                         ...) {
  if (is.numeric(x)) {
    replace(scales::number(x, big.mark = ",", ...),
            seq_along(scales::number(x, big.mark = big.mark, ...))
            %% by != (offset + 1), "")
  }
  else {
    replace(x, seq_along(x) %% by != (offset + 1), "")
  }
}

x <- LETTERS[1:12]
str_keep_seq(x, by = 2, offset = 0)
str_keep_seq(x, by = 2, offset = -1)
str_keep_seq(x, by = 3, offset = 0)
str_keep_seq(x, by = 3, offset = -1)
str_keep_seq(x, by = 3, offset = 1)

# global defaults
ggblanket_global$titles_case <- NULL

#' Set the titles case behaviour
#'
#' @description Set the titles case behaviour in `gg_*` functions.
#'
#' @param titles_case A function to apply to unspecified/unlabelled titles. Defaults to `snakecase::to_sentence_case`.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_titles_case <- function(titles_case = snakecase::to_sentence_case, ...) {
  old <- ggblanket_global$titles_case
  ggblanket_global$titles_case <- titles_case
  invisible(old)
}


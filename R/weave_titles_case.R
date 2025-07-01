# global defaults
ggblanket_global$titles_case <- NULL

#' Set a titles case function
#'
#' @description Set a function to format the titles of unlabelled variables.
#'
#' @param titles_case A function to format the title of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_titles_case <- function(titles_case = snakecase::to_sentence_case, ...) {
  old <- ggblanket_global$titles_case
  ggblanket_global$titles_case <- titles_case
  invisible(old)
}


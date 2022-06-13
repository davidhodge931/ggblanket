#' Add a tooltip text column of united variable names and values.
#'
#' @param data A data frame or tibble.
#' @param ... Arguments passed to select (i.e unquoted variables, tidyselect helpers etc). If no arguments provided, uses all columns.
#' @param rename_with A function to rename all variables in the text column.
#'
#' @return A data frame or tibble with a column of text
#' @export
#'
#' @examples
#' iris %>%
#'   add_tooltip_text() %>%
#'   head(1)
#'
#' iris %>%
#'   add_tooltip_text(Species, tidyselect::contains("Sepal")) %>%
#'   head(1)
add_tooltip_text <- function(data,
                             ...,
                             rename_with = ~ snakecase::to_sentence_case(.x)) {

  if (class(data)[1] == "sf") {
    temp_data <- data %>%
      dplyr::ungroup() %>%
      sf::st_drop_geometry()
  } else {
    temp_data <- data %>%
      dplyr::ungroup()
  }

  if (...length() != 0) {
    temp_data <- temp_data %>%
      dplyr::select(...)
  }

  temp_data <- temp_data %>%
    dplyr::rename_with(rename_with) %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ paste0(dplyr::cur_column(), ": ", .x),
      .names = ".add_{.col}"
    )) %>%
    tidyr::unite("text", tidyselect::starts_with(".add_"), sep = "<br>") %>%
    dplyr::select("text")

  data %>%
    dplyr::bind_cols(temp_data)
}

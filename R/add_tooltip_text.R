#' Add a tooltip text column of united variable names and values.
#'
#' @param data A data frame or tibble.
#' @param ... Arguments passed to select (i.e unquoted variables, tidyselect helpers etc). If no arguments provided, uses all columns.
#' @param titles A function to format the variable names, including in rlang lambda format.
#'
#' @return A data frame or tibble with a column of text
#' @export
#'
#' @examples
#' iris %>%
#'   add_tooltip_text() %>%
#'   head(1)
#'
#'  iris %>%
#'   add_tooltip_text(Species, tidyselect::contains("Sepal")) %>%
#'   head(1)
#'
#'   library(snakecase)
#'
#'  iris %>%
#'   add_tooltip_text(titles = ~ to_sentence_case(.x)) %>%
#'   head(1)
#'
#'  iris %>%
#'    add_tooltip_text() %>%
#'    gg_point(x = Sepal.Width,
#'             y = Sepal.Length,
#'             col = Species,
#'             text = text,
#'             theme = gg_theme("helvetica", x_grid = TRUE, y_grid = TRUE)) %>%
#'     plotly::ggplotly(tooltip = "text")
add_tooltip_text <- function(data,
                             ...,
                             titles = NULL) {

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

  if (!rlang::is_null(titles)) {
    temp_data <- temp_data %>%
      dplyr::titles(titles)
  }

  temp_data <- temp_data %>%
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

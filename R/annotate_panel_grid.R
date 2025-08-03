#' Annotated panel grid lines
#'
#' @description Replace panel grid lines with annotated segments.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' @param x_breaks,y_breaks A vector of axis breaks for the panel grid.
#' @param colour The colour of grid lines. Inherits from current theme panel.grid.major etc.
#' @param linewidth The linewidth of grid lines. Inherits from current theme panel.grid.major etc.
#' @param linetype The linetype of grid lines. Inherits from current theme panel.grid.major etc.
#' @param theme_elements What to do with theme panel grid elements. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of annotate layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_panel_grid(
#'     x_breaks = seq(190, 230, 15),
#'     y_breaks = seq(3000, 6500, 250),
#'   ) +
#'   geom_point()
#'
annotate_panel_grid <- function(
    ...,
    x_breaks = NULL,
    y_breaks = NULL,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    theme_elements = "transparent"
) {
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )
  # Validate theme_elements parameter
  if (!theme_elements %in% c("transparent", "keep", "blank")) {
    rlang::abort("theme_elements must be one of 'transparent', 'keep', or 'blank'")
  }
  # Return early if no breaks provided
  if (is.null(x_breaks) && is.null(y_breaks)) {
    return(list())
  }
  # Get current theme
  current_theme <- ggplot2::get_theme()
  # Extract theme properties for grid lines
  if (rlang::is_null(colour)) {
    colour <- current_theme$panel.grid.major$colour %||%
      current_theme$panel.grid$colour %||%
      "#E6E9EFFF"
  }
  if (rlang::is_null(linewidth)) {
    linewidth <- current_theme$panel.grid.major$linewidth %||%
      current_theme$panel.grid$linewidth %||%
      0.5
  }
  if (rlang::is_null(linetype)) {
    linetype <- current_theme$panel.grid.major$linetype %||%
      current_theme$panel.grid$linetype %||%
      "solid"
  }
  stamp <- list()
  # Add theme modifications if requested - target most specific elements
  if (theme_elements == "transparent") {
    theme_list <- list()

    # Only modify x-direction grid elements if x_breaks provided
    if (!is.null(x_breaks)) {
      theme_list$panel.grid.major.x <- ggplot2::element_line(colour = "transparent")
      theme_list$panel.grid.minor.x <- ggplot2::element_line(colour = "transparent")
    }

    # Only modify y-direction grid elements if y_breaks provided
    if (!is.null(y_breaks)) {
      theme_list$panel.grid.major.y <- ggplot2::element_line(colour = "transparent")
      theme_list$panel.grid.minor.y <- ggplot2::element_line(colour = "transparent")
    }

    if (length(theme_list) > 0) {
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_list)))
    }

  } else if (theme_elements == "blank") {
    theme_list <- list()

    # Only modify x-direction grid elements if x_breaks provided
    if (!is.null(x_breaks)) {
      theme_list$panel.grid.major.x <- ggplot2::element_blank()
      theme_list$panel.grid.minor.x <- ggplot2::element_blank()
    }

    # Only modify y-direction grid elements if y_breaks provided
    if (!is.null(y_breaks)) {
      theme_list$panel.grid.major.y <- ggplot2::element_blank()
      theme_list$panel.grid.minor.y <- ggplot2::element_blank()
    }

    if (length(theme_list) > 0) {
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_list)))
    }
  }
  # Add vertical grid lines (x_breaks)
  if (!is.null(x_breaks)) {
    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotate,
          "segment",
          x = x_breaks,
          xend = x_breaks,
          y = -Inf,
          yend = Inf,
          colour = colour,
          linewidth = linewidth,
          linetype = linetype,
          !!!list(...)
        )
      )
    )
  }
  # Add horizontal grid lines (y_breaks)
  if (!is.null(y_breaks)) {
    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotate,
          "segment",
          x = -Inf,
          xend = Inf,
          y = y_breaks,
          yend = y_breaks,
          colour = colour,
          linewidth = linewidth,
          linetype = linetype,
          !!!list(...)
        )
      )
    )
  }
  return(stamp)
}

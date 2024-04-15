#' Light mode theme with right legend
#'
#' @description Light mode theme with right legend using `lightness` and `linewidthness`.
#'
#' @param text_size The size of the text theme element.
#' @param text_family The family of the text theme element.
#' @param text_face The face of the text theme element.
#' @param text_colour The colour of the text theme element.
#' @param title_size The size of the plot.title theme element.
#' @param title_family The family of the plot.title theme element.
#' @param title_face The face of the plot.title theme element.
#' @param title_colour The colour of the plot.title theme element.
#' @param subtitle_size The size of the plot.subtitle theme element.
#' @param subtitle_family The family of the plot.subtitle theme element.
#' @param subtitle_face The face of the plot.subtitle theme element.
#' @param subtitle_colour The colour of the plot.subtitle theme element.
#' @param caption_size The size of the plot.caption theme element.
#' @param caption_family The family of the plot.caption theme element.
#' @param caption_face The face of the plot.caption theme element.
#' @param caption_colour The colour of the plot.caption theme element.
#' @param caption_hjust The horizontal adjustment of the plot.caption theme element.
#' @param ... Provided to support trailing commas only.
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     mode = light_mode_r()
#'   )
#'
light_mode_r <- function (
    text_size = 11,
    text_family = "",
    text_face = "plain",
    text_colour = lightness[1],
    title_size = ggplot2::rel(1.1),
    title_family = text_family,
    title_face = "bold",
    title_colour = text_colour,
    subtitle_size = ggplot2::rel(1),
    subtitle_family = text_family,
    subtitle_face = text_face,
    subtitle_colour = text_colour,
    caption_size = ggplot2::rel(0.85),
    caption_family = text_family,
    caption_face = text_face,
    caption_colour = scales::alpha(text_colour, 0.75),
    caption_hjust = 0,
    ...
  ) {

  flex_mode_r(
    text_size = text_size,
    text_family = text_family,
    text_face = text_face,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_face = title_face,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_face = subtitle_face,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_face = caption_face,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,

    axis_line_colour = lightness[2],
    panel_grid_colour = lightness[3],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    ...
  )
}

#' Light mode theme with top legend
#'
#' @description Light mode theme with top legend using `lightness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     mode = light_mode_t()
#'   )
#'
light_mode_t <- function (
    text_size = 11,
    text_family = "",
    text_face = "plain",
    text_colour = lightness[1],
    title_size = ggplot2::rel(1.1),
    title_family = text_family,
    title_face = "bold",
    title_colour = text_colour,
    subtitle_size = ggplot2::rel(1),
    subtitle_family = text_family,
    subtitle_face = text_face,
    subtitle_colour = text_colour,
    caption_size = ggplot2::rel(0.85),
    caption_family = text_family,
    caption_face = text_face,
    caption_colour = scales::alpha(text_colour, 0.75),
    caption_hjust = 0,
    ...
) {

  flex_mode_t(
    text_size = text_size,
    text_family = text_family,
    text_face = text_face,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_face = title_face,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_face = subtitle_face,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_face = caption_face,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,

    axis_line_colour = lightness[2],
    panel_grid_colour = lightness[3],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    ...
  )
}

#' Light mode theme with bottom legend
#'
#' @description Light mode theme with bottom legend using `lightness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     mode = light_mode_b()
#'   )
#'
light_mode_b <- function (
    text_size = 11,
    text_family = "",
    text_face = "plain",
    text_colour = lightness[1],
    title_size = ggplot2::rel(1.1),
    title_family = text_family,
    title_face = "bold",
    title_colour = text_colour,
    subtitle_size = ggplot2::rel(1),
    subtitle_family = text_family,
    subtitle_face = text_face,
    subtitle_colour = text_colour,
    caption_size = ggplot2::rel(0.85),
    caption_family = text_family,
    caption_face = text_face,
    caption_colour = scales::alpha(text_colour, 0.75),
    caption_hjust = 0,
    ...
) {

  flex_mode_b(
    text_size = text_size,
    text_family = text_family,
    text_face = text_face,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_face = title_face,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_face = subtitle_face,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_face = caption_face,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,

    axis_line_colour = lightness[2],
    panel_grid_colour = lightness[3],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    ...
  )
}

#' Light mode theme with no legend
#'
#' @description Light mode theme with no legend using `lightness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' penguins |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = species,
#'     mode = light_mode_n()
#'   )
#'
light_mode_n <- function (
    text_size = 11,
    text_family = "",
    text_face = "plain",
    text_colour = lightness[1],
    title_size = ggplot2::rel(1.1),
    title_family = text_family,
    title_face = "bold",
    title_colour = text_colour,
    subtitle_size = ggplot2::rel(1),
    subtitle_family = text_family,
    subtitle_face = text_face,
    subtitle_colour = text_colour,
    caption_size = ggplot2::rel(0.85),
    caption_family = text_family,
    caption_face = text_face,
    caption_colour = scales::alpha(text_colour, 0.75),
    caption_hjust = 0,
    ...
) {

  flex_mode_n(
    text_size = text_size,
    text_family = text_family,
    text_face = text_face,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_face = title_face,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_face = subtitle_face,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_face = caption_face,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,

    axis_line_colour = lightness[2],
    panel_grid_colour = lightness[3],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    ...
  )
}

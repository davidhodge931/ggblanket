#' #' @title Dark ggplot theme with right top legend
#' #'
#' #' @description Dark theme for a ggplot visualisation with legend at right top.
#' #'
#' #' @inheritParams light_mode_rt
#' #'
#' #' @return A ggplot theme.
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #' library(ggplot2)
#' #'
#' #' #set for a plot
#' #' penguins |>
#' #'   gg_point(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'     theme = dark_mode_b()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(dark_mode_b())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' dark_mode_rt <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = darkness[1],
#'     col_pal_axis_line = darkness[1],
#'     col_pal_background_i = darkness[2],
#'     col_pal_background_o = darkness[3],
#'     col_pal_gridlines = darkness[3],
#'     title_family = NULL,
#'     title_face = "plain",
#'     title_pal = NULL,
#'     title_size = ggplot2::rel(1.1),
#'     title_vjust = 0.5,
#'     title_margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0),
#'     subtitle_family = NULL,
#'     subtitle_face = NULL,
#'     subtitle_pal = NULL,
#'     subtitle_size = NULL,
#'     subtitle_vjust = 0.5,
#'     subtitle_margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0),
#'     caption_family = NULL,
#'     caption_face = NULL,
#'     caption_alpha = 0.33,
#'     caption_pal = col_pal_text,
#'     caption_size = ggplot2::rel(0.9),
#'     caption_hjust = 0,
#'     caption_vjust = 0.5,
#'     caption_margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)
#' ) {
#'
#'   light_mode_rt(
#'     base_size = base_size,
#'     base_family = base_family,
#'     base_face = base_face,
#'     col_pal_text = col_pal_text,
#'     col_pal_axis_line = col_pal_axis_line,
#'     col_pal_background_i = col_pal_background_i,
#'     col_pal_background_o = col_pal_background_o,
#'     col_pal_gridlines = col_pal_gridlines,
#'     title_family = title_family,
#'     title_face = title_face,
#'     title_pal = title_pal,
#'     title_size = title_size,
#'     title_vjust = title_vjust,
#'     title_margin = title_margin,
#'     subtitle_family = subtitle_family,
#'     subtitle_face = subtitle_face,
#'     subtitle_pal = subtitle_pal,
#'     subtitle_size = subtitle_size,
#'     subtitle_vjust = subtitle_vjust,
#'     subtitle_margin = subtitle_margin,
#'     caption_family = caption_family,
#'     caption_face = caption_face,
#'     caption_alpha = caption_alpha,
#'     caption_pal = col_pal_text,
#'     caption_size = caption_size,
#'     caption_hjust = caption_hjust,
#'     caption_vjust = caption_vjust,
#'     caption_margin = caption_margin
#'   )
#' }
#'
#' #' @title Dark ggplot theme with right legend
#' #'
#' #' @description Dark theme for a ggplot visualisation with right centre legend.
#' #'
#' #' @inheritParams light_mode_rt
#' #'
#' #' @return A ggplot theme.
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #' library(ggplot2)
#' #'
#' #' #set for a plot
#' #' penguins |>
#' #'   gg_point(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'     theme = dark_mode_r()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(dark_mode_r())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' dark_mode_r <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = darkness[1],
#'     col_pal_axis_line = darkness[1],
#'     col_pal_background_i = darkness[2],
#'     col_pal_background_o = darkness[3],
#'     col_pal_gridlines = darkness[3],
#'     title_family = NULL,
#'     title_face = "plain",
#'     title_pal = NULL,
#'     title_size = ggplot2::rel(1.1),
#'     title_vjust = 0.5,
#'     title_margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0),
#'     subtitle_family = NULL,
#'     subtitle_face = NULL,
#'     subtitle_pal = NULL,
#'     subtitle_size = NULL,
#'     subtitle_vjust = 0.5,
#'     subtitle_margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0),
#'     caption_family = NULL,
#'     caption_face = NULL,
#'     caption_alpha = 0.33,
#'     caption_pal = col_pal_text,
#'     caption_size = ggplot2::rel(0.9),
#'     caption_hjust = 0,
#'     caption_vjust = 0.5,
#'     caption_margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)
#' ) {
#'
#'   light_mode_r(
#'     base_size = base_size,
#'     base_family = base_family,
#'     base_face = base_face,
#'     col_pal_text = col_pal_text,
#'     col_pal_axis_line = col_pal_axis_line,
#'     col_pal_background_i = col_pal_background_i,
#'     col_pal_background_o = col_pal_background_o,
#'     col_pal_gridlines = col_pal_gridlines,
#'     title_family = title_family,
#'     title_face = title_face,
#'     title_pal = title_pal,
#'     title_size = title_size,
#'     title_vjust = title_vjust,
#'     title_margin = title_margin,
#'     subtitle_family = subtitle_family,
#'     subtitle_face = subtitle_face,
#'     subtitle_pal = subtitle_pal,
#'     subtitle_size = subtitle_size,
#'     subtitle_vjust = subtitle_vjust,
#'     subtitle_margin = subtitle_margin,
#'     caption_family = caption_family,
#'     caption_face = caption_face,
#'     caption_alpha = caption_alpha,
#'     caption_pal = col_pal_text,
#'     caption_size = caption_size,
#'     caption_hjust = caption_hjust,
#'     caption_vjust = caption_vjust,
#'     caption_margin = caption_margin
#'   )
#'
#' }
#'
#' #' @title Dark ggplot theme with bottom legend
#' #'
#' #' @description Dark theme for a ggplot visualisation with bottom legend.
#' #'
#' #' @inheritParams light_mode_rt
#' #'
#' #' @return A ggplot theme.
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #' library(ggplot2)
#' #'
#' #' #set for a plot
#' #' penguins |>
#' #'   gg_point(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'     theme = dark_mode_b()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(dark_mode_b())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' dark_mode_b <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = darkness[1],
#'     col_pal_axis_line = darkness[1],
#'     col_pal_background_i = darkness[2],
#'     col_pal_background_o = darkness[3],
#'     col_pal_gridlines = darkness[3],
#'     title_family = NULL,
#'     title_face = "plain",
#'     title_pal = NULL,
#'     title_size = ggplot2::rel(1.1),
#'     title_vjust = 0.5,
#'     title_margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0),
#'     subtitle_family = NULL,
#'     subtitle_face = NULL,
#'     subtitle_pal = NULL,
#'     subtitle_size = NULL,
#'     subtitle_vjust = 0.5,
#'     subtitle_margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0),
#'     caption_family = NULL,
#'     caption_face = NULL,
#'     caption_alpha = 0.33,
#'     caption_pal = col_pal_text,
#'     caption_size = ggplot2::rel(0.9),
#'     caption_hjust = 0,
#'     caption_vjust = 0.5,
#'     caption_margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)
#' ) {
#'
#'   light_mode_b(
#'     base_size = base_size,
#'     base_family = base_family,
#'     base_face = base_face,
#'     col_pal_text = col_pal_text,
#'     col_pal_axis_line = col_pal_axis_line,
#'     col_pal_background_i = col_pal_background_i,
#'     col_pal_background_o = col_pal_background_o,
#'     col_pal_gridlines = col_pal_gridlines,
#'     title_family = title_family,
#'     title_face = title_face,
#'     title_pal = title_pal,
#'     title_size = title_size,
#'     title_vjust = title_vjust,
#'     title_margin = title_margin,
#'     subtitle_family = subtitle_family,
#'     subtitle_face = subtitle_face,
#'     subtitle_pal = subtitle_pal,
#'     subtitle_size = subtitle_size,
#'     subtitle_vjust = subtitle_vjust,
#'     subtitle_margin = subtitle_margin,
#'     caption_family = caption_family,
#'     caption_face = caption_face,
#'     caption_alpha = caption_alpha,
#'     caption_pal = col_pal_text,
#'     caption_size = caption_size,
#'     caption_hjust = caption_hjust,
#'     caption_vjust = caption_vjust,
#'     caption_margin = caption_margin
#'   )
#' }
#'
#' #' @title Dark ggplot theme with top legend
#' #'
#' #' @description Dark theme for a ggplot visualisation with top legend.
#' #'
#' #' @inheritParams light_mode_rt
#' #'
#' #' @return A ggplot theme.
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #' library(ggplot2)
#' #'
#' #' #set for a plot
#' #' penguins |>
#' #'   gg_point(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'     theme = dark_mode_t()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(dark_mode_t())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' dark_mode_t <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = darkness[1],
#'     col_pal_axis_line = darkness[1],
#'     col_pal_background_i = darkness[2],
#'     col_pal_background_o = darkness[3],
#'     col_pal_gridlines = darkness[3],
#'     title_family = NULL,
#'     title_face = "plain",
#'     title_pal = NULL,
#'     title_size = ggplot2::rel(1.1),
#'     title_vjust = 0.5,
#'     title_margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0),
#'     subtitle_family = NULL,
#'     subtitle_face = NULL,
#'     subtitle_pal = NULL,
#'     subtitle_size = NULL,
#'     subtitle_vjust = 0.5,
#'     subtitle_margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0),
#'     caption_family = NULL,
#'     caption_face = NULL,
#'     caption_alpha = 0.33,
#'     caption_pal = col_pal_text,
#'     caption_size = ggplot2::rel(0.9),
#'     caption_hjust = 0,
#'     caption_vjust = 0.5,
#'     caption_margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)
#' ) {
#'
#'   light_mode_t(
#'     base_size = base_size,
#'     base_family = base_family,
#'     base_face = base_face,
#'     col_pal_text = col_pal_text,
#'     col_pal_axis_line = col_pal_axis_line,
#'     col_pal_background_i = col_pal_background_i,
#'     col_pal_background_o = col_pal_background_o,
#'     col_pal_gridlines = col_pal_gridlines,
#'     title_family = title_family,
#'     title_face = title_face,
#'     title_pal = title_pal,
#'     title_size = title_size,
#'     title_vjust = title_vjust,
#'     title_margin = title_margin,
#'     subtitle_family = subtitle_family,
#'     subtitle_face = subtitle_face,
#'     subtitle_pal = subtitle_pal,
#'     subtitle_size = subtitle_size,
#'     subtitle_vjust = subtitle_vjust,
#'     subtitle_margin = subtitle_margin,
#'     caption_family = caption_family,
#'     caption_face = caption_face,
#'     caption_alpha = caption_alpha,
#'     caption_pal = col_pal_text,
#'     caption_size = caption_size,
#'     caption_hjust = caption_hjust,
#'     caption_vjust = caption_vjust,
#'     caption_margin = caption_margin
#'   )
#' }
#'
#' #' @title Dark ggplot theme with inside legend
#' #'
#' #' @description Dark theme for a ggplot visualisation with legend inside the panel.
#' #'
#' #' @inheritParams light_mode_rt
#' #' @param legend_position_inside The placement of legends inside panels. A numeric vector of length two.
#' #'
#' #' @return A ggplot theme.
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #' library(ggplot2)
#' #'
#' #' #set for a plot
#' #' penguins |>
#' #'   gg_point(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'     theme = dark_mode_i(legend_position_inside = c(0.15, 0.7))
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(dark_mode_i())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' dark_mode_i <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = darkness[1],
#'     col_pal_axis_line = darkness[1],
#'     col_pal_background_i = darkness[2],
#'     col_pal_background_o = darkness[3],
#'     col_pal_gridlines = darkness[3],
#'     title_family = NULL,
#'     title_face = "plain",
#'     title_pal = NULL,
#'     title_size = ggplot2::rel(1.1),
#'     title_vjust = 0.5,
#'     title_margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0),
#'     subtitle_family = NULL,
#'     subtitle_face = NULL,
#'     subtitle_pal = NULL,
#'     subtitle_size = NULL,
#'     subtitle_vjust = 0.5,
#'     subtitle_margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0),
#'     caption_family = NULL,
#'     caption_face = NULL,
#'     caption_alpha = 0.33,
#'     caption_pal = col_pal_text,
#'     caption_size = ggplot2::rel(0.9),
#'     caption_hjust = 0,
#'     caption_vjust = 0.5,
#'     caption_margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0),
#'     legend_position_inside = c(0.5, 0.5)
#' ) {
#'
#'   light_mode_i(
#'     base_size = base_size,
#'     base_family = base_family,
#'     base_face = base_face,
#'     col_pal_text = col_pal_text,
#'     col_pal_axis_line = col_pal_axis_line,
#'     col_pal_background_i = col_pal_background_i,
#'     col_pal_background_o = col_pal_background_o,
#'     col_pal_gridlines = col_pal_gridlines,
#'     title_family = title_family,
#'     title_face = title_face,
#'     title_pal = title_pal,
#'     title_size = title_size,
#'     title_vjust = title_vjust,
#'     title_margin = title_margin,
#'     subtitle_family = subtitle_family,
#'     subtitle_face = subtitle_face,
#'     subtitle_pal = subtitle_pal,
#'     subtitle_size = subtitle_size,
#'     subtitle_vjust = subtitle_vjust,
#'     subtitle_margin = subtitle_margin,
#'     caption_family = caption_family,
#'     caption_face = caption_face,
#'     caption_alpha = caption_alpha,
#'     caption_pal = col_pal_text,
#'     caption_size = caption_size,
#'     caption_hjust = caption_hjust,
#'     caption_vjust = caption_vjust,
#'     caption_margin = caption_margin,
#'     legend_position_inside = legend_position_inside
#'   )
#' }

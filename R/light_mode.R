#' #' @title Light ggplot theme with right top legend
#' #'
#' #' @description Light theme for a ggplot visualisation with legend at right top.
#' #' @param base_size The base size of the text. Defaults to 11.
#' #' @param base_family The base family of the text. Defaults to "".
#' #' @param base_face The base face of the text. Defaults to "plain".
#' #' @param col_pal_text The colour of the text.
#' #' @param col_pal_axis_line The colour of the axis line (and ticks).
#' #' @param col_pal_background_i The background colour inside the panel.
#' #' @param col_pal_background_o The background colour outside the panel.
#' #' @param col_pal_gridlines The colour of the panel gridlines.
#' #' @param title_family The font family of the title. Defaults to the base_family.
#' #' @param title_face The font face of the title. Defaults to "plain".
#' #' @param title_pal The colour of the title. Defaults to the col_pal first element.
#' #' @param title_size The size of the title. Defaults to the base_size * 1.1.
#' #' @param title_vjust The vertical adjustment of the title. Defaults to 0.5.
#' #' @param title_margin The margin of the title. A ggplot2::margin function.
#' #' @param subtitle_family The font family of the subtitle. Defaults to the base_family.
#' #' @param subtitle_face The font face of the subtitle. Defaults to the base_face.
#' #' @param subtitle_pal The colour of the subtitle. Defaults to the col_pal first element.
#' #' @param subtitle_size The size of the subtitle. Defaults to the base_size.
#' #' @param subtitle_vjust The vertical adjustment of the subtitle. Defaults to 0.5.
#' #' @param subtitle_margin The margin of the subtitle. A ggplot2::margin function.
#' #' @param caption_family The font family of the caption. Defaults to the base_family.
#' #' @param caption_face The font face of the caption. Defaults to the base_face.
#' #' @param caption_size The size of the caption. Defaults to the base_size * 0.9.
#' #' @param caption_alpha The alpha of the caption pal. Defaults to 0.33. Use 1 for no alpha.
#' #' @param caption_pal The colour of the caption (before caption_alpha is applied). Defaults to the col_pal first element.
#' #' @param caption_hjust The horizontal adjustment of the caption. Defaults to 0.
#' #' @param caption_vjust The vertical adjustment of the caption. Defaults to 0.5.
#' #' @param caption_margin The margin of the caption. A ggplot2::margin function.
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
#' #'     theme = light_mode_b()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(light_mode_b())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' light_mode_rt <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = lightness[1],
#'     col_pal_axis_line = lightness[1],
#'     col_pal_background_i = lightness[2],
#'     col_pal_background_o = lightness[3],
#'     col_pal_gridlines = lightness[3],
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
#'   ggplot2::theme(
#'     line = ggplot2::element_line(colour = col_pal_axis_line, linewidth = base_size/33, linetype = 1, lineend = "square"),
#'     rect = ggplot2::element_rect(fill = col_pal_background_o, colour = col_pal_background_o, linewidth = base_size/33, linetype = 1),
#'     text = ggplot2::element_text(family = base_family, face = base_face, colour = col_pal_text, size = base_size,
#'                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
#'     axis.line = NULL,
#'     axis.line.x = NULL,
#'     axis.line.y = NULL,
#'     axis.text = NULL,
#'     axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
#'     axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
#'     axis.ticks = NULL,
#'     axis.ticks.length.x = grid::unit(base_size/3, "pt"),
#'     axis.ticks.length.x.top = NULL,
#'     axis.ticks.length.x.bottom = NULL,
#'     axis.ticks.length.y = grid::unit(base_size/4, "pt"),
#'     axis.ticks.length.y.left = NULL,
#'     axis.ticks.length.y.right = NULL,
#'     axis.title = NULL,
#'     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
#'     legend.spacing = grid::unit(base_size * 1, "pt"),
#'     legend.spacing.x = NULL,
#'     legend.spacing.y = NULL,
#'     legend.key = ggplot2::element_rect(colour = col_pal_background_o, fill = col_pal_background_o),
#'     legend.key.size = grid::unit(base_size * 1.75, "pt"),
#'     legend.key.height = NULL,
#'     legend.key.width = NULL,
#'     legend.key.spacing = NULL,
#'     legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
#'     legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
#'     legend.byrow = FALSE,
#'     legend.frame = NULL,
#'     legend.axis.line = NULL,
#'     legend.ticks = ggplot2::element_line(colour = col_pal_background_o),
#'     legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
#'     legend.title.position = "top",
#'     legend.box = NULL,
#'     legend.box.background = NULL,
#'     legend.box.spacing = NULL,
#'     panel.background = ggplot2::element_rect(fill = col_pal_background_i, colour = col_pal_background_i),
#'     panel.border = ggplot2::element_blank(),
#'     panel.grid = NULL,
#'     panel.grid.major = ggplot2::element_line(colour = col_pal_gridlines, linewidth = ggplot2::rel(0.5)),
#'     panel.grid.minor = ggplot2::element_blank(),
#'     panel.spacing = grid::unit(base_size * 2, "pt"),
#'     panel.spacing.x = NULL,
#'     panel.spacing.y = NULL,
#'     panel.ontop = FALSE,
#'     strip.background = ggplot2::element_rect(fill = NA, colour = NA),
#'     strip.clip = "inherit",
#'     strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     strip.text.x = NULL,
#'     strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 1, l = 0)),
#'     strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
#'     strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
#'     strip.placement = "outside",
#'     strip.placement.x = NULL,
#'     strip.placement.y = NULL,
#'     strip.switch.pad.grid = grid::unit(0.15, "pt"),
#'     strip.switch.pad.wrap = grid::unit(0.15, "pt"),
#'     plot.background = ggplot2::element_rect(),
#'     plot.title = ggplot2::element_text(family = title_family, face = title_face, colour = title_pal, size = title_size, hjust = 0, vjust = title_vjust, margin = title_margin),
#'     plot.title.position = "plot",
#'     plot.subtitle = ggplot2::element_text(family = subtitle_family, face = subtitle_face, colour = subtitle_pal, size = subtitle_size, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
#'     plot.caption = ggplot2::element_text(family = caption_family, face = caption_face, colour = scales::alpha(caption_pal, caption_alpha), size = caption_size, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
#'     plot.caption.position = "plot",
#'     plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
#'     plot.tag.position = "topleft",
#'     plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),
#'
#'     legend.position = "right",
#'     legend.direction = "vertical",
#'     legend.justification = "top",
#'     legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
#'     legend.margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = base_size * 0.75),
#'     legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5)),
#'     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
#'     axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
#'     axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
#'     axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
#'     axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 1), angle = -90),
#'
#'     complete = TRUE
#'   )
#' }
#'
#' #' @title Light ggplot theme with right legend
#' #'
#' #' @description Light theme for a ggplot visualisation with right centre legend.
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
#' #'     theme = light_mode_r()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(light_mode_r())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' light_mode_r <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = lightness[1],
#'     col_pal_axis_line = lightness[1],
#'     col_pal_background_i = lightness[2],
#'     col_pal_background_o = lightness[3],
#'     col_pal_gridlines = lightness[3],
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
#'   ggplot2::theme(
#'     line = ggplot2::element_line(colour = col_pal_axis_line, linewidth = base_size/33, linetype = 1, lineend = "square"),
#'     rect = ggplot2::element_rect(fill = col_pal_background_o, colour = col_pal_background_o, linewidth = base_size/33, linetype = 1),
#'     text = ggplot2::element_text(family = base_family, face = base_face, colour = col_pal_text, size = base_size,
#'                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
#'     axis.line = NULL,
#'     axis.line.x = NULL,
#'     axis.line.y = NULL,
#'     axis.text = NULL,
#'     axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
#'     axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
#'     axis.ticks = NULL,
#'     axis.ticks.length.x = grid::unit(base_size/3, "pt"),
#'     axis.ticks.length.x.top = NULL,
#'     axis.ticks.length.x.bottom = NULL,
#'     axis.ticks.length.y = grid::unit(base_size/4, "pt"),
#'     axis.ticks.length.y.left = NULL,
#'     axis.ticks.length.y.right = NULL,
#'     axis.title = NULL,
#'     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
#'     legend.spacing = grid::unit(base_size * 1, "pt"),
#'     legend.spacing.x = NULL,
#'     legend.spacing.y = NULL,
#'     legend.key = ggplot2::element_rect(colour = col_pal_background_o, fill = col_pal_background_o),
#'     legend.key.size = grid::unit(base_size * 1.75, "pt"),
#'     legend.key.height = NULL,
#'     legend.key.width = NULL,
#'     legend.key.spacing = NULL,
#'     legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
#'     legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
#'     legend.byrow = FALSE,
#'     legend.frame = NULL,
#'     legend.axis.line = NULL,
#'     legend.ticks = ggplot2::element_line(colour = col_pal_background_o),
#'     legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
#'     legend.title.position = "top",
#'     legend.box = NULL,
#'     legend.box.background = NULL,
#'     legend.box.spacing = NULL,
#'     panel.background = ggplot2::element_rect(fill = col_pal_background_i, colour = col_pal_background_i),
#'     panel.border = ggplot2::element_blank(),
#'     panel.grid = NULL,
#'     panel.grid.major = ggplot2::element_line(colour = col_pal_gridlines, linewidth = ggplot2::rel(0.5)),
#'     panel.grid.minor = ggplot2::element_blank(),
#'     panel.spacing = grid::unit(base_size * 2, "pt"),
#'     panel.spacing.x = NULL,
#'     panel.spacing.y = NULL,
#'     panel.ontop = FALSE,
#'     strip.background = ggplot2::element_rect(fill = NA, colour = NA),
#'     strip.clip = "inherit",
#'     strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     strip.text.x = NULL,
#'     strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
#'     strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
#'     strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
#'     strip.placement = "outside",
#'     strip.placement.x = NULL,
#'     strip.placement.y = NULL,
#'     strip.switch.pad.grid = grid::unit(0.15, "pt"),
#'     strip.switch.pad.wrap = grid::unit(0.15, "pt"),
#'     plot.background = ggplot2::element_rect(),
#'     plot.title = ggplot2::element_text(family = title_family, face = title_face, colour = title_pal, size = title_size, hjust = 0, vjust = title_vjust, margin = title_margin),
#'     plot.title.position = "plot",
#'     plot.subtitle = ggplot2::element_text(family = subtitle_family, face = subtitle_face, colour = subtitle_pal, size = subtitle_size, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
#'     plot.caption = ggplot2::element_text(family = caption_family, face = caption_face, colour = scales::alpha(caption_pal, caption_alpha), size = caption_size, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
#'     plot.caption.position = "plot",
#'     plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
#'     plot.tag.position = "topleft",
#'     plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),
#'
#'     legend.position = "right",
#'     legend.direction = "vertical",
#'     legend.justification = "left",
#'     legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
#'     legend.margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = base_size * 0.75),
#'     legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5)),
#'     axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
#'     axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
#'     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
#'     axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
#'     axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 1), angle = -90),
#'
#'     complete = TRUE
#'   )
#' }
#'
#' #' @title Light ggplot theme with bottom legend
#' #'
#' #' @description Light theme for a ggplot visualisation with bottom legend.
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
#' #'     theme = light_mode_b()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(light_mode_b())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' light_mode_b <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = lightness[1],
#'     col_pal_axis_line = lightness[1],
#'     col_pal_background_i = lightness[2],
#'     col_pal_background_o = lightness[3],
#'     col_pal_gridlines = lightness[3],
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
#'   ggplot2::theme(
#'     line = ggplot2::element_line(colour = col_pal_axis_line, linewidth = base_size/33, linetype = 1, lineend = "square"),
#'     rect = ggplot2::element_rect(fill = col_pal_background_o, colour = col_pal_background_o, linewidth = base_size/33, linetype = 1),
#'     text = ggplot2::element_text(family = base_family, face = base_face, colour = col_pal_text, size = base_size,
#'                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
#'     axis.line = NULL,
#'     axis.line.x = NULL,
#'     axis.line.y = NULL,
#'     axis.text = NULL,
#'     axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
#'     axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
#'     axis.ticks = NULL,
#'     axis.ticks.length.x = grid::unit(base_size/3, "pt"),
#'     axis.ticks.length.x.top = NULL,
#'     axis.ticks.length.x.bottom = NULL,
#'     axis.ticks.length.y = grid::unit(base_size/4, "pt"),
#'     axis.ticks.length.y.left = NULL,
#'     axis.ticks.length.y.right = NULL,
#'     axis.title = NULL,
#'     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
#'     legend.spacing = grid::unit(base_size * 1, "pt"),
#'     legend.spacing.x = NULL,
#'     legend.spacing.y = NULL,
#'     legend.key = ggplot2::element_rect(colour = col_pal_background_o, fill = col_pal_background_o),
#'     legend.key.size = grid::unit(base_size * 1.75, "pt"),
#'     legend.key.height = NULL,
#'     legend.key.width = NULL,
#'     legend.key.spacing = NULL,
#'     legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
#'     legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
#'     legend.byrow = FALSE,
#'     legend.frame = NULL,
#'     legend.axis.line = NULL,
#'     legend.ticks = ggplot2::element_line(colour = col_pal_background_o),
#'     legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
#'     legend.title.position = "top",
#'     legend.box = NULL,
#'     legend.box.background = NULL,
#'     legend.box.spacing = NULL,
#'     panel.background = ggplot2::element_rect(fill = col_pal_background_i, colour = col_pal_background_i),
#'     panel.border = ggplot2::element_blank(),
#'     panel.grid = NULL,
#'     panel.grid.major = ggplot2::element_line(colour = col_pal_gridlines, linewidth = ggplot2::rel(0.5)),
#'     panel.grid.minor = ggplot2::element_blank(),
#'     panel.spacing = grid::unit(base_size * 2, "pt"),
#'     panel.spacing.x = NULL,
#'     panel.spacing.y = NULL,
#'     panel.ontop = FALSE,
#'     strip.background = ggplot2::element_rect(fill = NA, colour = NA),
#'     strip.clip = "inherit",
#'     strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     strip.text.x = NULL,
#'     strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
#'     strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
#'     strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
#'     strip.placement = "outside",
#'     strip.placement.x = NULL,
#'     strip.placement.y = NULL,
#'     strip.switch.pad.grid = grid::unit(0.15, "pt"),
#'     strip.switch.pad.wrap = grid::unit(0.15, "pt"),
#'     plot.background = ggplot2::element_rect(),
#'     plot.title = ggplot2::element_text(family = title_family, face = title_face, colour = title_pal, size = title_size, hjust = 0, vjust = title_vjust, margin = title_margin),
#'     plot.title.position = "plot",
#'     plot.subtitle = ggplot2::element_text(family = subtitle_family, face = subtitle_face, colour = subtitle_pal, size = subtitle_size, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
#'     plot.caption = ggplot2::element_text(family = caption_family, face = caption_face, colour = scales::alpha(caption_pal, caption_alpha), size = caption_size, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
#'     plot.caption.position = "plot",
#'     plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
#'     plot.tag.position = "topleft",
#'     plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),
#'
#'     legend.position = "bottom",
#'     legend.direction = "horizontal",
#'     legend.justification = "left",
#'     legend.box.margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = 0, l = 0),
#'     legend.margin = ggplot2::margin(r = base_size * 2, b = base_size * 0.5),
#'     legend.title = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 0.5, l = 0)),
#'     legend.text = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.5, r = base_size * 1.25, b = base_size * 0.5, l = base_size * 0.5)),
#'     axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
#'     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
#'     axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
#'     axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
#'     axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90),
#'
#'     complete = TRUE
#'   )
#' }
#'
#' #' @title Light ggplot theme with top legend
#' #'
#' #' @description Light theme for a ggplot visualisation with top legend.
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
#' #'     theme = light_mode_t()
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(light_mode_t())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' light_mode_t <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = lightness[1],
#'     col_pal_axis_line = lightness[1],
#'     col_pal_background_i = lightness[2],
#'     col_pal_background_o = lightness[3],
#'     col_pal_gridlines = lightness[3],
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
#'   ggplot2::theme(
#'     line = ggplot2::element_line(colour = col_pal_axis_line, linewidth = base_size/33, linetype = 1, lineend = "square"),
#'     rect = ggplot2::element_rect(fill = col_pal_background_o, colour = col_pal_background_o, linewidth = base_size/33, linetype = 1),
#'     text = ggplot2::element_text(family = base_family, face = base_face, colour = col_pal_text, size = base_size,
#'                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
#'     axis.line = NULL,
#'     axis.line.x = NULL,
#'     axis.line.y = NULL,
#'     axis.text = NULL,
#'     axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
#'     axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
#'     axis.ticks = NULL,
#'     axis.ticks.length.x = grid::unit(base_size/3, "pt"),
#'     axis.ticks.length.x.top = NULL,
#'     axis.ticks.length.x.bottom = NULL,
#'     axis.ticks.length.y = grid::unit(base_size/4, "pt"),
#'     axis.ticks.length.y.left = NULL,
#'     axis.ticks.length.y.right = NULL,
#'     axis.title = NULL,
#'     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
#'     legend.spacing = grid::unit(base_size * 1, "pt"),
#'     legend.spacing.x = NULL,
#'     legend.spacing.y = NULL,
#'     legend.key = ggplot2::element_rect(colour = col_pal_background_o, fill = col_pal_background_o),
#'     legend.key.size = grid::unit(base_size * 1.75, "pt"),
#'     legend.key.height = NULL,
#'     legend.key.width = NULL,
#'     legend.key.spacing = NULL,
#'     legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
#'     legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
#'     legend.byrow = FALSE,
#'     legend.frame = NULL,
#'     legend.axis.line = NULL,
#'     legend.ticks = ggplot2::element_line(colour = col_pal_background_o),
#'     legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
#'     legend.title.position = "top",
#'     legend.box = NULL,
#'     legend.box.background = NULL,
#'     legend.box.spacing = NULL,
#'     panel.background = ggplot2::element_rect(fill = col_pal_background_i, colour = col_pal_background_i),
#'     panel.border = ggplot2::element_blank(),
#'     panel.grid = NULL,
#'     panel.grid.major = ggplot2::element_line(colour = col_pal_gridlines, linewidth = ggplot2::rel(0.5)),
#'     panel.grid.minor = ggplot2::element_blank(),
#'     panel.spacing = grid::unit(base_size * 2, "pt"),
#'     panel.spacing.x = NULL,
#'     panel.spacing.y = NULL,
#'     panel.ontop = FALSE,
#'     strip.background = ggplot2::element_rect(fill = NA, colour = NA),
#'     strip.clip = "inherit",
#'     strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     strip.text.x = NULL,
#'     strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
#'     strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
#'     strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
#'     strip.placement = "outside",
#'     strip.placement.x = NULL,
#'     strip.placement.y = NULL,
#'     strip.switch.pad.grid = grid::unit(0.15, "pt"),
#'     strip.switch.pad.wrap = grid::unit(0.15, "pt"),
#'     plot.background = ggplot2::element_rect(),
#'     plot.title = ggplot2::element_text(family = title_family, face = title_face, colour = title_pal, size = title_size, hjust = 0, vjust = title_vjust, margin = title_margin),
#'     plot.title.position = "plot",
#'     plot.subtitle = ggplot2::element_text(family = subtitle_family, face = subtitle_face, colour = subtitle_pal, size = subtitle_size, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
#'     plot.caption = ggplot2::element_text(family = caption_family, face = caption_face, colour = scales::alpha(caption_pal, caption_alpha), size = caption_size, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
#'     plot.caption.position = "plot",
#'     plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
#'     plot.tag.position = "topleft",
#'     plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),
#'
#'     legend.position = "top",
#'     legend.direction = "horizontal",
#'     legend.justification = "left",
#'     legend.box.margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0),
#'     legend.margin = ggplot2::margin(t = base_size * -1.5, r = base_size * 2, b = base_size * 0.5, l = 0),
#'     legend.title = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 0.5, l = 0)),
#'     legend.text = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.5, r = base_size * 1.25, b = base_size * 0.5, l = base_size * 0.5)),
#'     axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
#'     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
#'     axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * 0, r = 0, b = base_size * 0.2, l = 0)),
#'     axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90),
#'
#'     complete = TRUE
#'   )
#' }
#'
#' #' @title Light ggplot theme with inside legend
#' #'
#' #' @description Light theme for a ggplot visualisation with legend inside the panel.
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
#' #'     theme = light_mode_i(legend_position_inside = c(0.15, 0.7))
#' #'   )
#' #'
#' #' #set globally
#' #' \dontrun{
#' #'   theme_set(light_mode_i())
#' #'
#' #'   penguins |>
#' #'     gg_point(
#' #'       x = flipper_length_mm,
#' #'       y = body_mass_g,
#' #'       col = species
#' #'     )
#' #' }
#' #'
#' light_mode_i <- function (
#'     base_size = 11,
#'     base_family = "",
#'     base_face = "plain",
#'     col_pal_text = lightness[1],
#'     col_pal_axis_line = lightness[1],
#'     col_pal_background_i = lightness[2],
#'     col_pal_background_o = lightness[3],
#'     col_pal_gridlines = lightness[3],
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
#'   ggplot2::theme(
#'     line = ggplot2::element_line(colour = col_pal_axis_line, linewidth = base_size/33, linetype = 1, lineend = "square"),
#'     rect = ggplot2::element_rect(fill = col_pal_background_o, colour = col_pal_background_o, linewidth = base_size/33, linetype = 1),
#'     text = ggplot2::element_text(family = base_family, face = base_face, colour = col_pal_text, size = base_size,
#'                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
#'     axis.line = NULL,
#'     axis.line.x = NULL,
#'     axis.line.y = NULL,
#'     axis.text = NULL,
#'     axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
#'     axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
#'     axis.ticks = NULL,
#'     axis.ticks.length.x = grid::unit(base_size/3, "pt"),
#'     axis.ticks.length.x.top = NULL,
#'     axis.ticks.length.x.bottom = NULL,
#'     axis.ticks.length.y = grid::unit(base_size/4, "pt"),
#'     axis.ticks.length.y.left = NULL,
#'     axis.ticks.length.y.right = NULL,
#'     axis.title = NULL,
#'     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
#'     legend.spacing = grid::unit(base_size * 1, "pt"),
#'     legend.spacing.x = NULL,
#'     legend.spacing.y = NULL,
#'     legend.key = ggplot2::element_rect(colour = col_pal_background_o, fill = col_pal_background_o),
#'     legend.key.size = grid::unit(base_size * 1.75, "pt"),
#'     legend.key.height = NULL,
#'     legend.key.width = NULL,
#'     legend.key.spacing = NULL,
#'     legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
#'     legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
#'     legend.byrow = FALSE,
#'     legend.frame = NULL,
#'     legend.axis.line = NULL,
#'     legend.ticks = ggplot2::element_line(colour = col_pal_background_o),
#'     legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
#'     legend.title.position = "top",
#'     legend.box = NULL,
#'     legend.box.background = NULL,
#'     legend.box.spacing = NULL,
#'     panel.background = ggplot2::element_rect(fill = col_pal_background_i, colour = col_pal_background_i),
#'     panel.border = ggplot2::element_blank(),
#'     panel.grid = NULL,
#'     panel.grid.major = ggplot2::element_line(colour = col_pal_gridlines, linewidth = ggplot2::rel(0.5)),
#'     panel.grid.minor = ggplot2::element_blank(),
#'     panel.spacing = grid::unit(base_size * 2, "pt"),
#'     panel.spacing.x = NULL,
#'     panel.spacing.y = NULL,
#'     panel.ontop = FALSE,
#'     strip.background = ggplot2::element_rect(fill = NA, colour = NA),
#'     strip.clip = "inherit",
#'     strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     strip.text.x = NULL,
#'     strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
#'     strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
#'     strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
#'     strip.placement = "outside",
#'     strip.placement.x = NULL,
#'     strip.placement.y = NULL,
#'     strip.switch.pad.grid = grid::unit(0.15, "pt"),
#'     strip.switch.pad.wrap = grid::unit(0.15, "pt"),
#'     plot.background = ggplot2::element_rect(),
#'     plot.title = ggplot2::element_text(family = title_family, face = title_face, colour = title_pal, size = title_size, hjust = 0, vjust = title_vjust, margin = title_margin),
#'     plot.title.position = "plot",
#'     plot.subtitle = ggplot2::element_text(family = subtitle_family, face = subtitle_face, colour = subtitle_pal, size = subtitle_size, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
#'     plot.caption = ggplot2::element_text(family = caption_family, face = caption_face, colour = scales::alpha(caption_pal, caption_alpha), size = caption_size, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
#'     plot.caption.position = "plot",
#'     plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
#'     plot.tag.position = "topleft",
#'     plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),
#'
#'     legend.position = "inside",
#'     legend.position.inside = legend_position_inside,
#'     legend.direction = "vertical",
#'     legend.justification = NULL,
#'     legend.margin = ggplot2::margin(t = base_size * 0.66, r = base_size * 0.33, b = base_size * 0.66, l = base_size * 0.66),
#'     legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
#'     legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 0.5)),
#'     legend.background = ggplot2::element_rect(colour = col_pal_gridlines, fill = col_pal_background_o),
#'     axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
#'     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
#'     axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
#'     axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
#'     axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90),
#'
#'     complete = TRUE
#'   )
#' }

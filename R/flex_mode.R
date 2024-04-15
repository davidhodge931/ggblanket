#' Mode theme base
#'
#' @description Theme base for `*_mode_*` functions.
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
#' @param axis_line_colour The colour of the axis.line theme element.
#' @param axis_line_linewidth The linewidth of the axis.line theme element.
#' @param axis_ticks_colour The colour of the axis.ticks theme element.
#' @param axis_ticks_linewidth The linewidth of the axis.ticks theme element.
#' @param axis_ticks_length_x_pt The length of the axis.ticks.length.x theme element in points.
#' @param axis_ticks_linewidth_y_pt The length of the axis.ticks.length.y theme element in points.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#' @param legend_background_fill The fill (and colour) of the legend.background theme element.
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#'
#' @return A ggplot theme.
#' @keywords internal
#'
flex_mode_base <- function(
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
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = text_size / 3,
    axis_ticks_length_y_pt = text_size / 4,
    panel_grid_colour = lightness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill
) {

  ggplot2::theme(
    text = ggplot2::element_text(size = text_size, family = text_family, face = text_face, colour = text_colour,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = ggplot2::element_line(colour = axis_line_colour, linewidth = axis_line_linewidth),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = text_size * 0.25), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = text_size * 0.25), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = axis_ticks_colour, linewidth = axis_ticks_linewidth),
    axis.ticks.length.x = grid::unit(axis_ticks_length_x_pt, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(axis_ticks_length_y_pt, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(text_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = legend_key_fill, fill = legend_key_fill),
    legend.key.size = grid::unit(text_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(11, "pt"),
    legend.key.spacing.y = grid::unit(text_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.text = ggplot2::element_text(margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)),
    legend.ticks = NULL,
    legend.ticks.length = grid::unit(text_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    legend.background = ggplot2::element_rect(colour = legend_background_fill, fill = legend_background_fill),
    panel.background = ggplot2::element_rect(colour = panel_background_fill, fill = panel_background_fill),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = panel_grid_colour, linewidth = panel_grid_linewidth),
    panel.grid.major = NULL,
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(text_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = NA, colour = NA),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.66, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = text_size * 0.25, r = 0, b = text_size * 1, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = text_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(colour = plot_background_fill, fill = plot_background_fill),
    plot.title = ggplot2::element_text(size = title_size, family = title_family, face = title_face, colour = title_colour,
                                       hjust = 0, margin = ggplot2::margin(t = text_size * -1, r = 0, b = text_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(size = subtitle_size, family = subtitle_family, face = subtitle_face, colour = subtitle_colour,
                                          hjust = 0, margin = ggplot2::margin(t = text_size * -2, r = 0, b = text_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(size = caption_size, family = caption_family, face = caption_face, colour = caption_colour, hjust = caption_hjust,
                                         margin = ggplot2::margin(t = text_size * 0.5, r = 0, b = text_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = text_size * 2, r = text_size * 2, b = text_size * 0.33, l = text_size * 1),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -0.33, r = 0, b = text_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -1, r = 0, b = text_size * 1, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * -0.5, b = 0, l = text_size * 1), angle = -90),
    legend.margin = ggplot2::margin(t = 0, r = text_size * -1, b = text_size * 0.75, l = text_size * 0.75),
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = c(0, 1),
    legend.location = "panel",
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.5, l = 0)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = text_size * 0.3, r = 0, b = text_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = text_size * 0.3, l = 0)),

    complete = TRUE
  )
}

#' Flexible mode with right legend
#'
#' @description Flexible mode with right legend.
#'
#' @inheritParams flex_mode_base
#' @param ... Provided to support trailing commas only.
#'
#' @return A ggplot theme.
#' @keywords internal
#'
flex_mode_r <- function (
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
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = text_size / 3,
    axis_ticks_length_y_pt = text_size / 4,
    panel_grid_colour = lightness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    ...) {

  flex_mode_base(
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
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill
  )
}

#' Flexible mode with top legend
#'
#' @description Flexible mode with legend at top.
#'
#' @inheritParams flex_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
flex_mode_t <- function (
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
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = text_size / 3,
    axis_ticks_length_y_pt = text_size / 4,
    panel_grid_colour = lightness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    ...) {

  flex_mode_base(
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
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill
  ) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.margin = ggplot2::margin(t = text_size * -1.5, r = text_size * 2, b = text_size * 0.5, l = 0),
      legend.box.margin = ggplot2::margin(t = text_size * 0.5, r = 0, b = text_size * 0.5, l = 0),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = text_size * 0.25, r = 0, b = text_size * 0.5, l = 0)),

      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -0.33, r = 0, b = text_size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.5, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * 0, r = 0, b = text_size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * -0.5, b = 0, l = text_size * 1), angle = -90)
    )
}

#' Flexible mode with bottom legend
#'
#' @description Flexible mode with legend at bottom.
#'
#' @inheritParams flex_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
flex_mode_b <- function (
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
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = text_size / 3,
    axis_ticks_length_y_pt = text_size / 4,
    panel_grid_colour = lightness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    ...) {

  flex_mode_base(
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
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.margin = ggplot2::margin(t = 0, r = text_size * 2, b = text_size * 0.75, l = 0),
      legend.box.margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = 0, l = 0),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = text_size * 0.25, r = 0, b = text_size * 0.5, l = 0)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = text_size * 0.3, r = 0, b = text_size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = text_size * 0.3, l = 0))
    )
}

#' Flexible mode with no legend
#'
#' @description Flexible mode with no legend.
#'
#' @inheritParams flex_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
flex_mode_n <- function (
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
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = text_size / 3,
    axis_ticks_length_y_pt = text_size / 4,
    panel_grid_colour = lightness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = lightness[4],
    plot_background_fill = lightness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    ...) {

  flex_mode_base(
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
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill
  ) +
    ggplot2::theme(legend.position = "none")
}


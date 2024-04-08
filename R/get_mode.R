#' Mode theme base
#'
#' @description Theme base for `*_mode_*` functions.
#'
#' @param text_size The base size of the text. Defaults to 11. The title is 110% of this, caption 85% and tag 120%.
#' @param text_family The family of the text. Defaults to "".
#' @param text_colour A colour of the text theme element. The caption colour is scales::alpha(text_colour, 0.75).
#' @param axis_line_colour The colour of the axis.line and axis.ticks theme elements.
#' @param axis_line_linewidth The linewidth of the axis.line and axis.ticks theme element.
#' @param panel_line_colour The colour of the panel.line theme element.
#' @param panel_background_colour The colour (and fill) of the panel.background theme element.
#' @param plot_background_colour The colour (and fill) of the plot.background, legend.background and legend.key theme elements.
#' @param panel_line_linewidth The linewidth of the panel.line theme element.
#'
#' @return A ggplot theme.
#' @keywords internal
#'
get_mode_base <- function(
    text_size = 11,
    text_family = "",
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = lightness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5]
    ) {

  ggplot2::theme(
    text = ggplot2::element_text(family = text_family, face = "plain", colour = text_colour, size = text_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = ggplot2::element_line(colour = axis_line_colour, linewidth = axis_line_linewidth),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = text_size * 0.25), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = text_size * 0.25), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = axis_line_colour, linewidth = axis_line_linewidth),
    axis.ticks.length.x = grid::unit(text_size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(text_size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(text_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = plot_background_colour, fill = plot_background_colour),
    legend.key.size = grid::unit(text_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(text_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(text_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = NULL,
    legend.ticks.length = grid::unit(text_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    legend.background = ggplot2::element_rect(fill = plot_background_colour, colour = plot_background_colour),
    panel.background = ggplot2::element_rect(fill = panel_background_colour, colour = panel_background_colour),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = panel_line_colour, linewidth = panel_line_linewidth),
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
    plot.background = ggplot2::element_rect(fill = plot_background_colour, colour = plot_background_colour, linewidth = axis_line_linewidth),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = text_size * -1, r = 0, b = text_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = text_size * -2, r = 0, b = text_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = scales::alpha(text_colour, 0.75), size = ggplot2::rel(0.85), hjust = 0, margin = ggplot2::margin(t = text_size * 0.5, r = 0, b = text_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = text_size * 2, r = text_size * 2, b = text_size * 0.25, l = text_size * 0.75),

    complete = TRUE
  )
}

#' Flexible mode with right legend
#'
#' @description Flexible mode with right legend and customisable colour and linewidth.
#'
#' @inheritParams get_mode_base
#' @param ... Provided to support trailing commas only.
#'
#' @return A ggplot theme.
#' @keywords internal
#'
get_mode_r <- function (
    text_size = 11,
    text_family = "",
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    panel_line_colour = lightness[3],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    axis_line_linewidth = linewidthness[1],
    panel_line_linewidth = linewidthness[2],
    ...) {

    get_mode_base(
      text_size = text_size,
      text_family = text_family,
      text_colour = text_colour,
      axis_line_colour = axis_line_colour,
      panel_line_colour = panel_line_colour,
      panel_background_colour = panel_background_colour,
      plot_background_colour = plot_background_colour,
      axis_line_linewidth = axis_line_linewidth,
      panel_line_linewidth = panel_line_linewidth
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = c(0, 1),
      legend.location = "panel",
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.75, l = text_size * 0.75),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.5, l = 0)),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = text_size * 0.5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -0.33, r = 0, b = text_size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -1, r = 0, b = text_size * 1, l = 0)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = text_size * 0.3, r = 0, b = text_size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = text_size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 1, b = 0, l = text_size * 1), angle = -90)
    )
}

#' Flexible mode with top legend
#'
#' @description Flexible mode with legend at top and customisable colour and linewidth.
#'
#' @inheritParams get_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
get_mode_t <- function (
    text_size = 11,
    text_family = "",
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    panel_line_colour = lightness[3],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    axis_line_linewidth = linewidthness[1],
    panel_line_linewidth = linewidthness[2],
    ...) {

  get_mode_base(
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    axis_line_colour = axis_line_colour,
    panel_line_colour = panel_line_colour,
    panel_background_colour = panel_background_colour,
    plot_background_colour = plot_background_colour,
    axis_line_linewidth = axis_line_linewidth,
    panel_line_linewidth = panel_line_linewidth
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = c(0, 0.5),
    legend.location = "plot",
    legend.box.margin = ggplot2::margin(t = text_size * 0.5, r = 0, b = text_size * 0.5, l = 0),
    legend.margin = ggplot2::margin(t = text_size * -1.5, r = text_size * 2, b = text_size * 0.5, l = 0),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = text_size * 0.25, r = 0, b = text_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 1.25, b = 0, l = text_size * 0.5)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = text_size * 0.3, r = 0, b = text_size * 1, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -0.33, r = 0, b = text_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.5, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * 0, r = 0, b = text_size * 0.3, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * -0.5, b = 0, l = text_size * 1), angle = -90)
  )
}

#' Flexible mode with bottom legend
#'
#' @description Flexible mode with legend at bottom and customisable colour and linewidth.
#'
#' @inheritParams get_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
get_mode_b <- function (
    text_size = 11,
    text_family = "",
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    panel_line_colour = lightness[3],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    axis_line_linewidth = linewidthness[1],
    panel_line_linewidth = linewidthness[2],
    ...) {

    get_mode_base(
      text_size = text_size,
      text_family = text_family,
      text_colour = text_colour,
      axis_line_colour = axis_line_colour,
      panel_line_colour = panel_line_colour,
      panel_background_colour = panel_background_colour,
      plot_background_colour = plot_background_colour,
      axis_line_linewidth = axis_line_linewidth,
      panel_line_linewidth = panel_line_linewidth
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.box.margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(r = text_size * 2, b = text_size * 0.5),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = text_size * 0.25, r = 0, b = text_size * 0.5, l = 0)),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 1.25, b = 0, l = text_size * 0.5)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = text_size * 0.3, r = 0, b = text_size * 1, l = 0)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -0.33, r = 0, b = text_size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -1, r = 0, b = text_size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = text_size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * -0.5, b = 0, l = text_size * 1), angle = -90)
    )
}

#' Flexible mode with no legend
#'
#' @description Flexible mode with no legend and customisable colour and linewidth.
#'
#' @inheritParams get_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
get_mode_n <- function (
    text_size = 11,
    text_family = "",
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    panel_line_colour = lightness[3],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    axis_line_linewidth = linewidthness[1],
    panel_line_linewidth = linewidthness[2],
    ...) {

  get_mode_base(
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    axis_line_colour = axis_line_colour,
    panel_line_colour = panel_line_colour,
    panel_background_colour = panel_background_colour,
    plot_background_colour = plot_background_colour,
    axis_line_linewidth = axis_line_linewidth,
    panel_line_linewidth = panel_line_linewidth
  ) +
  ggplot2::theme(
    legend.position = "none",
    legend.direction = "vertical",
    legend.justification = c(0, 1),
    legend.location = "panel",
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.75, l = text_size * 0.75),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = text_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = text_size * 0.5)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -0.33, r = 0, b = text_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = text_size * -1, r = 0, b = text_size * 1, l = 0)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = text_size * 0.3, r = 0, b = text_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = text_size * -0.5, r = 0, b = text_size * 0.3, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = text_size * 1, b = 0, l = text_size * 1), angle = -90)
  )
}

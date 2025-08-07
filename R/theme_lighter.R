#' Lighter theme
#'
#' @description A lighter complete theme.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param base_size The base size of the text theme element. Defaults to 10.
#' @param base_family The base family of the text theme element. Defaults to "".
#' @param base_colour The base colour of the text theme element.
#' @param base_face The base face of the text theme element. Defaults to "plain".
#' @param legend_position The position of the legend. Either "right", "top" or "bottom".
#' @param legend_axis_line_colour The colour of the legend.axis.line theme element.
#' @param legend_axis_line_linewidth The linewidth of the legend.axis.line theme element.
#' @param legend_background_fill The fill (and colour) of the legend.background theme element.
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#' @param legend_ticks_colour The colour of the legend.ticks theme element.
#' @param legend_ticks_linewidth The linewidth of the legend.ticks theme element.
#' @param legend_ticks_length The legend.ticks.length theme element.
#' @param axis_line_colour The colour of the axis.line theme element.
#' @param axis_line_linewidth The linewidth of the axis.line theme element.
#' @param axis_ticks_colour The colour of the axis.ticks theme element.
#' @param axis_ticks_linewidth The linewidth of the axis.ticks theme element.
#' @param axis_ticks_length The length of the axis.ticks.length theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid theme element.
#' @param panel_grid_minor_linetype The linetype of the panel.grid.minor theme element. Defaults to 0.
#' @param panel_grid_minor_linewidth The linewidth of the panel.grid.minor theme element.
#' @param panel_heights The height of the panels.
#' @param panel_widths The width of the panels.
#' @param title_size The size of the plot.title theme element.
#' @param title_family The family of the plot.title theme element.
#' @param title_colour The colour of the plot.title theme element.
#' @param title_face The face of the plot.title theme element.
#' @param subtitle_size The size of the plot.subtitle theme element.
#' @param subtitle_family The family of the plot.subtitle theme element.
#' @param subtitle_colour The colour of the plot.subtitle theme element.
#' @param subtitle_face The face of the plot.subtitle theme element.
#' @param caption_size The size of the plot.caption theme element.
#' @param caption_family The family of the plot.caption theme element.
#' @param caption_colour The colour of the plot.caption theme element.
#' @param caption_face The face of the plot.caption theme element.
#' @param caption_hjust The horizontal adjustment of the plot.caption theme element.
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_darker(legend_position = "top"))
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
theme_lighter <- function(
  ...,
  base_size = 10,
  base_family = "",
  base_colour = "#121B24FF",
  base_face = "plain",
  legend_position = "right",
  legend_axis_line_colour = plot_background_fill,
  legend_axis_line_linewidth = axis_line_linewidth,
  legend_background_fill = plot_background_fill,
  legend_key_fill = plot_background_fill,
  legend_ticks_colour = legend_axis_line_colour,
  legend_ticks_linewidth = legend_axis_line_linewidth,
  legend_ticks_length = ggplot2::rel(c(0.175, 0)),
  axis_line_colour = "#121B24FF",
  axis_line_linewidth = 0.25,
  axis_ticks_colour = axis_line_colour,
  axis_ticks_linewidth = axis_line_linewidth,
  axis_ticks_length = grid::unit(11 / 3, "pt"),
  panel_background_fill = "#FFFFFFFF",
  plot_background_fill = "#FFFFFFFF",
  panel_grid_colour = "#F6F8FAFF",
  panel_grid_linewidth = 1.33,
  panel_grid_minor_linetype = 0,
  panel_grid_minor_linewidth = ggplot2::rel(0.5),
  panel_heights = NULL,
  panel_widths = NULL,
  title_size = ggplot2::rel(1.1),
  title_family = base_family,
  title_colour = base_colour,
  title_face = "bold",
  subtitle_size = ggplot2::rel(1),
  subtitle_family = base_family,
  subtitle_colour = base_colour,
  subtitle_face = "plain",
  caption_size = ggplot2::rel(0.85),
  caption_family = base_family,
  caption_colour = ifelse(
    is_panel_light(),
    col_screen(base_colour),
    col_multiply(base_colour)
  ),
  caption_face = "plain",
  caption_hjust = 0
) {
  # Base theme (same for all legend positions)
  theme <- ggplot2::theme(
    text = ggplot2::element_text(
      size = base_size,
      family = base_family,
      face = "plain",
      colour = base_colour,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.line = ggplot2::element_line(
      colour = axis_line_colour,
      linewidth = axis_line_linewidth,
      lineend = "square"
    ),
    axis.line.x = NULL,
    axis.line.x.bottom = NULL,
    axis.line.x.top = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,
    axis.ticks = ggplot2::element_line(
      colour = axis_ticks_colour,
      linewidth = axis_ticks_linewidth
    ),
    axis.minor.ticks.x.bottom = ggplot2::element_line(
      colour = axis_ticks_colour
    ),
    axis.minor.ticks.x.top = ggplot2::element_line(colour = axis_ticks_colour),
    axis.minor.ticks.y.left = ggplot2::element_line(colour = axis_ticks_colour),
    axis.minor.ticks.y.right = ggplot2::element_line(
      colour = axis_ticks_colour
    ),

    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = axis_ticks_length,
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = ggplot2::rel(0.66),
    axis.title = ggplot2::element_text(),
    axis.text = ggplot2::element_text(),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 11 * 0.25),
      hjust = 1
    ),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 11 * 0.25),
      hjust = 0
    ),
    legend.spacing = grid::unit(11 * 1, "pt"),
    legend.spacing.y = grid::unit(11 * 1.5, "pt"),
    legend.spacing.x = NULL,
    legend.key = ggplot2::element_rect(
      colour = legend_key_fill,
      fill = legend_key_fill
    ),
    legend.key.height = ggplot2::rel(1),
    legend.key.width = ggplot2::rel(0.6),
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(11, "pt"),
    legend.key.spacing.y = grid::unit(11 * 0.33, "pt"),
    legend.frame = NULL,
    legend.text = ggplot2::element_text(
      margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    ),
    legend.axis.line = ggplot2::element_line(
      colour = legend_axis_line_colour,
      linewidth = legend_axis_line_linewidth
    ),
    legend.ticks = ggplot2::element_line(
      colour = legend_ticks_colour,
      linewidth = legend_ticks_linewidth
    ),
    legend.ticks.length = legend_ticks_length,
    legend.box = "vertical",
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    legend.box.just = "left",
    legend.background = ggplot2::element_rect(
      colour = legend_background_fill,
      fill = legend_background_fill
    ),
    panel.background = ggplot2::element_rect(
      colour = panel_background_fill,
      fill = panel_background_fill
    ),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(
      colour = panel_grid_colour,
      linewidth = panel_grid_linewidth
    ),
    panel.grid.major = NULL,
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor = ggplot2::element_line(
      linetype = panel_grid_minor_linetype,
      linewidth = panel_grid_minor_linewidth
    ),
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.spacing = grid::unit(11 * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    panel.heights = panel_heights,
    panel.widths = panel_widths,
    strip.background = ggplot2::element_rect(fill = NA, colour = NA),
    strip.clip = "off",
    strip.text = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.66, l = 0)
    ),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(
      margin = ggplot2::margin(t = 11 * 0.25, r = 0, b = 11 * 1, l = 0)
    ),
    strip.text.y = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 11 * 2 / 3),
      angle = -90
    ),
    strip.text.y.left = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 11 * 2 / 3, b = 0, l = 0),
      angle = 90
    ),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(
      colour = plot_background_fill,
      fill = plot_background_fill
    ),
    plot.title = ggplot2::element_text(
      size = title_size,
      family = title_family,
      face = title_face,
      colour = title_colour,
      hjust = 0,
      margin = ggplot2::margin(t = 11 * -1, r = 0, b = 11 * 2.5, l = 0)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      size = subtitle_size,
      family = subtitle_family,
      face = subtitle_face,
      colour = subtitle_colour,
      hjust = 0,
      margin = ggplot2::margin(t = 11 * -2, r = 0, b = 11 * 2, l = 0)
    ),
    plot.caption = ggplot2::element_text(
      size = caption_size,
      family = caption_family,
      face = caption_face,
      colour = caption_colour,
      hjust = caption_hjust,
      margin = ggplot2::margin(t = 11 * 0.5, r = 0, b = 11 * 0.5, l = 0)
    ),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(
      t = 11 * 2,
      r = 11 * 2,
      b = 11 * 0.33,
      l = 11 * 1
    ),
    complete = TRUE
  )

  # Apply legend position specific settings
  theme + theme_lighter_move_legend(legend_position)
}

#' Move legend position
#'
#' @description Helper function to move the legend position in theme_lighter.
#'
#' @param legend_position The position of the legend. Either "right", "top" or "bottom".
#'
#' @return A ggplot theme object with legend position settings.
#' @noRd
#'
theme_lighter_move_legend <- function(legend_position = "right") {
  if (legend_position == "right") {
    ggplot2::theme(
      # Legend-specific settings for right position
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.location = "panel",
      legend.title.position = "top",
      legend.margin = ggplot2::margin(
        t = 0,
        r = 11 * -1,
        b = 0,
        l = 11 * 0.75
      ),
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.5, l = 0)
      ),
      legend.byrow = FALSE,

      # Axis settings for right legend
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -0.33, r = 0, b = 11 * 0.75, l = 0)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -1, r = 0, b = 11 * 1, l = 0)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * 1, b = 0, l = 0),
        angle = 90
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * -0.5, b = 0, l = 11 * 1),
        angle = -90
      ),
      axis.text.x = ggplot2::element_text(
        vjust = 1,
        margin = ggplot2::margin(t = 11 * 0.3, r = 0, b = 11 * 1, l = 0)
      ),
      axis.text.x.top = ggplot2::element_text(
        vjust = 0,
        margin = ggplot2::margin(t = 11 * -0.5, r = 0, b = 11 * 0.3, l = 0)
      )
    )
  } else if (legend_position == "top") {
    ggplot2::theme(
      # Legend-specific settings for top position
      legend.position = "top",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.title.position = "top",
      legend.margin = ggplot2::margin(
        t = 11 * -1.5,
        r = 11 * 2,
        b = 11 * 0.5,
        l = 0
      ),
      legend.box.margin = ggplot2::margin(
        t = 11 * 0.5,
        r = 0,
        b = 11 * 0.5,
        l = 0
      ),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * 0.25, r = 0, b = 11 * 0.5, l = 0)
      ),
      legend.byrow = TRUE,

      # Axis settings for top legend
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -0.33, r = 0, b = 11 * 0.75, l = 0)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.5, l = 0)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * 1, b = 0, l = 0),
        angle = 90
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * -0.5, b = 0, l = 11 * 1),
        angle = -90
      ),
      axis.text.x = ggplot2::element_text(
        vjust = 1,
        margin = ggplot2::margin(t = 11 * 0.3, r = 0, b = 11 * 1, l = 0)
      ),
      axis.text.x.top = ggplot2::element_text(
        vjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.3, l = 0)
      )
    )
  } else if (legend_position == "bottom") {
    ggplot2::theme(
      # Legend-specific settings for bottom position
      legend.position = "bottom",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.title.position = "top",
      legend.margin = ggplot2::margin(
        t = 0,
        r = 11 * 2,
        b = 11 * 0.75,
        l = 0
      ),
      legend.box.margin = ggplot2::margin(
        t = 11 * -0.5,
        r = 0,
        b = 0,
        l = 0
      ),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * 0.25, r = 0, b = 11 * 0.5, l = 0)
      ),
      legend.byrow = TRUE,

      # Axis settings for bottom legend
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -0.33, r = 0, b = 11 * 0.75, l = 0)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -1, r = 0, b = 11 * 1, l = 0)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * 1, b = 0, l = 0),
        angle = 90
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * -0.5, b = 0, l = 11 * 1),
        angle = -90
      ),
      axis.text.x = ggplot2::element_text(
        vjust = 1,
        margin = ggplot2::margin(t = 11 * 0.3, r = 0, b = 11 * 1, l = 0)
      ),
      axis.text.x.top = ggplot2::element_text(
        vjust = 0,
        margin = ggplot2::margin(t = 11 * -0.5, r = 0, b = 11 * 0.3, l = 0)
      )
    )
  }
}

# theme_ggrefine (internal helper, not exported) --------------------------------
#
# The shared theme-building engine used by theme_lights(), theme_greys(), and
# theme_darks(). Every one of those three public functions resolves its own
# defaults (colours, dependent parameters, etc.) and then forwards everything
# here. Keeping the actual `ggplot2::theme()` construction in one position means
# the three public functions stay small, symmetric, and easy to compare against
# each other, and any change to the shared plot structure only needs to be made
# once.
theme_ggrefine <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = "#100F0F", #flexoki::flexoki$base["black"],
    title_size = NULL,
    title_family = NULL,
    title_colour = NULL,
    subtitle_size = NULL,
    subtitle_family = NULL,
    subtitle_colour = NULL,
    caption_size = NULL,
    caption_family = NULL,
    caption_colour = NULL,
    caption_hjust = 0,
    legend_position = "right",
    legend_axis_line_colour = NULL,
    legend_axis_line_linewidth = NULL,
    legend_background_fill = NULL,
    legend_key_fill = NULL,
    legend_ticks_colour = NULL,
    legend_ticks_linewidth = NULL,
    legend_ticks_linetype = 1,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = NULL,
    axis_line_linewidth = 0.2,
    axis_ticks_colour = NULL,
    axis_ticks_linewidth = NULL,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = "white",
    panel_grid_colour = "#F2F0E5", #flexoki::flexoki$base["base50"],
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "white"
) {
  legend_position <- match.arg(legend_position, c("right", "top", "bottom"))

  # Resolve dependent defaults inside the function body to avoid lazy
  # evaluation surprises when arguments inherit from other arguments.
  if (is.null(title_size)) {
    title_size <- text_size
  }
  if (is.null(title_family)) {
    title_family <- text_family
  }
  if (is.null(title_colour)) {
    title_colour <- text_colour
  }

  if (is.null(subtitle_size)) {
    subtitle_size <- text_size
  }
  if (is.null(subtitle_family)) {
    subtitle_family <- text_family
  }
  if (is.null(subtitle_colour)) {
    subtitle_colour <- text_colour
  }

  if (is.null(caption_size)) {
    caption_size <- text_size
  }
  if (is.null(caption_family)) {
    caption_family <- text_family
  }
  if (is.null(caption_colour)) {
    caption_colour <- text_colour
  }

  if (is.null(axis_line_colour)) {
    axis_line_colour <- text_colour
  }

  # Convert all colour arguments to plain character strings
  text_colour <- as.character(text_colour)
  title_colour <- as.character(title_colour)
  subtitle_colour <- as.character(subtitle_colour)
  caption_colour <- as.character(caption_colour)
  axis_line_colour <- as.character(axis_line_colour)
  panel_background_fill <- as.character(panel_background_fill)
  panel_grid_colour <- as.character(panel_grid_colour)
  plot_background_fill <- as.character(plot_background_fill)

  # Set defaults for dependent parameters
  if (is.null(axis_ticks_colour)) {
    axis_ticks_colour <- axis_line_colour
  }
  if (is.null(axis_ticks_linewidth)) {
    axis_ticks_linewidth <- axis_line_linewidth
  }
  if (is.null(legend_axis_line_colour)) {
    legend_axis_line_colour <- plot_background_fill
  }
  if (is.null(legend_axis_line_linewidth)) {
    legend_axis_line_linewidth <- axis_line_linewidth
  }
  if (is.null(legend_background_fill)) {
    legend_background_fill <- plot_background_fill
  }
  if (is.null(legend_key_fill)) {
    legend_key_fill <- plot_background_fill
  }
  if (is.null(legend_ticks_colour)) {
    legend_ticks_colour <- legend_axis_line_colour
  }
  if (is.null(legend_ticks_linewidth)) {
    legend_ticks_linewidth <- legend_axis_line_linewidth
  }

  # Convert dependent colour arguments to plain character strings
  axis_ticks_colour <- as.character(axis_ticks_colour)
  legend_axis_line_colour <- as.character(legend_axis_line_colour)
  legend_background_fill <- as.character(legend_background_fill)
  legend_key_fill <- as.character(legend_key_fill)
  legend_ticks_colour <- as.character(legend_ticks_colour)

  # Base theme (same for all legend positions)
  theme <-
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size,
        family = text_family,
        colour = text_colour,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = ggplot2::margin()
      ),
      line = ggplot2::element_line(
        colour = axis_line_colour,
        linewidth = axis_line_linewidth,
        lineend = "butt"
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
        linewidth = axis_ticks_linewidth,
        lineend = "butt"
      ),
      axis.minor.ticks.x.bottom = ggplot2::element_line(
        colour = axis_ticks_colour
      ),
      axis.minor.ticks.x.top = ggplot2::element_line(
        colour = axis_ticks_colour
      ),
      axis.minor.ticks.y.left = ggplot2::element_line(
        colour = axis_ticks_colour
      ),
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
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5.5)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = 5.5)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 11),
        angle = 90
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 11),
        angle = -90
      ),
      axis.text = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(
        vjust = 1,
        margin = ggplot2::margin(t = 3.33)
      ),
      axis.text.x.top = ggplot2::element_text(
        vjust = 0,
        margin = ggplot2::margin(b = 3.33)
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 3.33),
        hjust = 1
      ),
      axis.text.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 3.33),
        hjust = 0
      ),

      legend.key = ggplot2::element_rect(
        colour = legend_key_fill,
        fill = legend_key_fill
      ),
      legend.key.height = ggplot2::rel(1),
      legend.key.width = ggplot2::rel(0.6),

      legend.key.spacing = NULL,
      legend.key.spacing.x = grid::unit(11, "pt"),
      legend.key.spacing.y = grid::unit(3.33, "pt"),
      legend.frame = NULL,
      legend.spacing = ggplot2::unit(5.5, "pt"),
      legend.spacing.y = ggplot2::unit(0, "pt"),
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(l = 5.5)
      ),
      legend.axis.line = ggplot2::element_line(
        colour = legend_axis_line_colour,
        linewidth = legend_axis_line_linewidth
      ),
      legend.ticks = ggplot2::element_line(
        colour = legend_ticks_colour,
        linewidth = legend_ticks_linewidth,
        linetype = legend_ticks_linetype
      ),
      legend.ticks.length = legend_ticks_length,
      legend.box.background = NULL,
      legend.box.spacing = NULL,
      legend.box.just = "left",
      legend.background = ggplot2::element_rect(
        colour = legend_background_fill,
        fill = legend_background_fill
      ),

      legend.box = "vertical",
      legend.justification.right = "top",
      legend.justification.top = "left",
      legend.justification.bottom = "left",
      legend.title.position = "top",
      panel.background = ggplot2::element_rect(
        colour = panel_background_fill,
        fill = panel_background_fill
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(
        colour = panel_grid_colour,
        lineend = "butt"
      ),
      panel.grid.major = ggplot2::element_line(
        linewidth = panel_grid_linewidth,
        linetype = panel_grid_linetype
      ),
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor = ggplot2::element_line(
        linetype = panel_grid_minor_linetype,
        linewidth = panel_grid_minor_linewidth
      ),
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing = grid::unit(20, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      spacing = grid::unit(5.5, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.clip = "off",
      strip.text = ggplot2::element_text(
        margin = ggplot2::margin(b = 5.5)
      ),
      strip.text.x = NULL,
      strip.text.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(t = 5.5)
      ),
      strip.text.y = ggplot2::element_text(
        margin = ggplot2::margin(l = 5.5),
        angle = -90
      ),
      strip.text.y.left = ggplot2::element_text(
        margin = ggplot2::margin(r = 5.5),
        angle = 90
      ),
      strip.placement = "outside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = grid::unit(2.75, "pt"),
      strip.switch.pad.wrap = grid::unit(2.75, "pt"),
      plot.background = ggplot2::element_rect(
        colour = plot_background_fill,
        fill = plot_background_fill
      ),
      plot.title = ggplot2::element_text(
        size = title_size,
        family = title_family,
        colour = title_colour,
        hjust = 0,
        margin = ggplot2::margin(b = 5.5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = subtitle_size,
        family = subtitle_family,
        colour = subtitle_colour,
        hjust = 0,
        margin = ggplot2::margin(b = 5.5)
      ),
      plot.caption = ggplot2::element_text(
        size = caption_size,
        family = caption_family,
        colour = caption_colour,
        hjust = caption_hjust,
        margin = ggplot2::margin(t = 5.5)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.tag = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        hjust = 0,
        vjust = 0.5
      ),
      plot.tag.position = "topleft",
      plot.margin = ggplot2::margin(8.25, 12.375, 8.25, 8.25),
      complete = FALSE
    ) +
    ggplot2::theme(
      geom = ggplot2::element_geom(
        fill = "steelblue",
        colour = "steelblue",
        pointshape = 21,
        linewidth = 0.66,
        borderwidth = 0.33,
        pointsize = 1.5,
        fontsize = text_size,
        family = text_family
      ),
      # Border geoms — have both fill and colour
      geom.area = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.bar = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.boxplot = ggplot2::element_geom(
        linewidth = 0.33,
        borderwidth = 0.33
      ),
      geom.col = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.crossbar = ggplot2::element_geom(
        linewidth = 0.33,
        borderwidth = 0.33
      ),
      geom.density = ggplot2::element_geom(
        linewidth = 0.33,
        borderwidth = 0.33
      ),
      geom.dotplot = ggplot2::element_geom(
        linewidth = 0.33,
        borderwidth = 0.33
      ),
      geom.hex = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.map = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.point = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.pointrange = ggplot2::element_geom(
        linewidth = 0.33,
        borderwidth = 0.33
      ),
      geom.polygon = ggplot2::element_geom(
        linewidth = 0.33,
        borderwidth = 0.33
      ),
      geom.rect = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.ribbon = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.smooth = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.sf = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.tile = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),
      geom.violin = ggplot2::element_geom(linewidth = 0.33, borderwidth = 0.33),

      # Line geoms — colour only
      geom.abline = ggplot2::element_geom(linewidth = 0.66),
      geom.contour = ggplot2::element_geom(linewidth = 0.66),
      geom.curve = ggplot2::element_geom(linewidth = 0.66),
      geom.density_2d = ggplot2::element_geom(linewidth = 0.66),
      geom.errorbar = ggplot2::element_geom(linewidth = 0.66),
      geom.hline = ggplot2::element_geom(linewidth = 0.66),
      geom.line = ggplot2::element_geom(linewidth = 0.66),
      geom.linerange = ggplot2::element_geom(linewidth = 0.66),
      geom.path = ggplot2::element_geom(linewidth = 0.66),
      geom.quantile = ggplot2::element_geom(linewidth = 0.66),
      geom.rug = ggplot2::element_geom(linewidth = 0.66),
      geom.segment = ggplot2::element_geom(linewidth = 0.66),
      geom.spoke = ggplot2::element_geom(linewidth = 0.66),
      geom.step = ggplot2::element_geom(linewidth = 0.66),
      geom.vline = ggplot2::element_geom(linewidth = 0.66),

      geom.text = ggplot2::element_geom(fontsize = 9, colour = text_colour),
      geom.label = ggplot2::element_geom(
        fontsize = 9,
        colour = text_colour,
        fill = panel_background_fill
      ),

      palette.colour.discrete = jumble::jumble,
      palette.fill.discrete = jumble::jumble,

      palette.colour.continuous = viridis::turbo(n = 256),
      palette.fill.continuous = viridis::turbo(n = 256),

      palette.shape.discrete = scales::pal_manual(c(21, 24, 22, 23, 25)),

      palette.linetype.discrete = scales::pal_manual(1:6)
    )

  theme +
    refine_legend(position = legend_position)
}

#' theme_lights------------------------------------------------------------------
#'
#' Lights theme
#'
#' @description A complete theme with a white panel background and a light panel
#' grid colour. Uses flexoki inspired colours.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param text_size The base size of the text theme element. Defaults to 10.
#' @param text_family The base family of the text theme element. Defaults to "".
#' @param text_colour The base colour of the text theme element.
#' @param title_size The size of the `plot.title` theme element. Defaults to `text_size`.
#' @param title_family The family of the `plot.title` theme element. Defaults to `text_family`.
#' @param title_colour The colour of the `plot.title` theme element. Defaults to `text_colour`.
#' @param subtitle_size The size of the `plot.subtitle` theme element. Defaults to `text_size`.
#' @param subtitle_family The family of the `plot.subtitle` theme element. Defaults to `text_family`.
#' @param subtitle_colour The colour of the `plot.subtitle` theme element. Defaults to `text_colour`.
#' @param caption_size The size of the `plot.caption` theme element. Defaults to `text_size`.
#' @param caption_family The family of the `plot.caption` theme element. Defaults to `text_family`.
#' @param caption_colour The colour of the `plot.caption` theme element. Defaults to `text_colour`.
#' @param caption_hjust The horizontal justification of the `plot.caption` theme element. Defaults to 0.
#' @param legend_position The position of the legend. Either "right", "top" or "bottom".
#' @param legend_axis_line_colour The colour of the legend.axis.line theme element.
#' @param legend_axis_line_linewidth The linewidth of the legend.axis.line theme element.
#' @param legend_background_fill The fill (and colour) of the `legend.background` theme element.
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#' @param legend_ticks_colour The colour of the legend.ticks theme element.
#' @param legend_ticks_linewidth The linewidth of the legend.ticks theme element.
#' @param legend_ticks_linetype The linetype of the legend.ticks theme element. Defaults to 1.
#' @param legend_ticks_length The length of the legend.ticks.length theme element.
#' @param axis_line_colour The colour of the axis.line theme element. Defaults to `text_colour`.
#' @param axis_line_linewidth The linewidth of the axis.line theme element.
#' @param axis_ticks_colour The colour of the axis.ticks theme element.
#' @param axis_ticks_linewidth The linewidth of the axis.ticks theme element.
#' @param axis_ticks_length The length of the axis.ticks.length theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linetype The linetype of the panel.grid.major theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid.major theme element.
#' @param panel_grid_minor_linetype The linetype of the panel.grid.minor theme element.
#' @param panel_grid_minor_linewidth The linewidth of the panel.grid.minor theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#'
#' @return A ggplot theme.
#' @noRd
theme_lights <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = "#100F0F", #flexoki::flexoki$base["black"],
    title_size = NULL,
    title_family = NULL,
    title_colour = NULL,
    subtitle_size = NULL,
    subtitle_family = NULL,
    subtitle_colour = NULL,
    caption_size = NULL,
    caption_family = NULL,
    caption_colour = NULL,
    caption_hjust = 0,
    legend_position = "right",
    legend_axis_line_colour = NULL,
    legend_axis_line_linewidth = NULL,
    legend_background_fill = NULL,
    legend_key_fill = NULL,
    legend_ticks_colour = NULL,
    legend_ticks_linewidth = NULL,
    legend_ticks_linetype = 1,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = NULL,
    axis_line_linewidth = 0.2,
    axis_ticks_colour = NULL,
    axis_ticks_linewidth = NULL,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = "white",
    panel_grid_colour = "#F2F0E5", #flexoki::flexoki$base["base50"],
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "white"
) {
  theme_ggrefine(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,
    legend_position = legend_position,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_linetype = legend_ticks_linetype,
    legend_ticks_length = legend_ticks_length,
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length = axis_ticks_length,
    plot_background_fill = plot_background_fill,
    panel_background_fill = panel_background_fill,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linetype = panel_grid_linetype,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_grid_minor_linetype = panel_grid_minor_linetype,
    panel_grid_minor_linewidth = panel_grid_minor_linewidth
  )
}

#' theme_greys------------------------------------------------------------------
#'
#' Grey theme
#'
#' @description A complete theme with a light grey panel background and white
#' panel grid colour. Uses flexoki inspired colours.
#'
#' @inheritParams theme_lights
#'
#' @return A ggplot theme.
#' @noRd
theme_greys <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = "#100F0F", #flexoki::flexoki$base["black"],
    title_size = NULL,
    title_family = NULL,
    title_colour = NULL,
    subtitle_size = NULL,
    subtitle_family = NULL,
    subtitle_colour = NULL,
    caption_size = NULL,
    caption_family = NULL,
    caption_colour = NULL,
    caption_hjust = 0,
    legend_position = "right",
    legend_axis_line_colour = NULL,
    legend_axis_line_linewidth = NULL,
    legend_background_fill = NULL,
    legend_key_fill = NULL,
    legend_ticks_colour = NULL,
    legend_ticks_linewidth = NULL,
    legend_ticks_linetype = 1,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = NULL,
    axis_line_linewidth = 0.2,
    axis_ticks_colour = NULL,
    axis_ticks_linewidth = NULL,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = "#F2F0E5FF", # flexoki::flexoki$base["base50"]
    panel_grid_colour = "white",
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "white"
) {
  theme_ggrefine(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,
    legend_position = legend_position,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_linetype = legend_ticks_linetype,
    legend_ticks_length = legend_ticks_length,
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length = axis_ticks_length,
    plot_background_fill = plot_background_fill,
    panel_background_fill = panel_background_fill,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linetype = panel_grid_linetype,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_grid_minor_linetype = panel_grid_minor_linetype,
    panel_grid_minor_linewidth = panel_grid_minor_linewidth
  )
}

#' theme_darks------------------------------------------------------------------
#'
#' Dark theme
#'
#' @description A complete theme with a dark plot and panel background. Uses
#' flexoki inspired colours.
#'
#' @inheritParams theme_lights
#'
#' @return A ggplot theme.
#' @noRd
theme_darks <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = "#CECDC3",
    title_size = NULL,
    title_family = NULL,
    title_colour = NULL,
    subtitle_size = NULL,
    subtitle_family = NULL,
    subtitle_colour = NULL,
    caption_size = NULL,
    caption_family = NULL,
    caption_colour = NULL,
    caption_hjust = 0,
    legend_position = "right",
    legend_axis_line_colour = NULL,
    legend_axis_line_linewidth = NULL,
    legend_background_fill = NULL,
    legend_key_fill = NULL,
    legend_ticks_colour = NULL,
    legend_ticks_linewidth = NULL,
    legend_ticks_linetype = 1,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = NULL,
    axis_line_linewidth = 0.2,
    axis_ticks_colour = NULL,
    axis_ticks_linewidth = NULL,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = "#282726", #flexoki::flexoki$base["base900"]
    panel_grid_colour = "#100F0F", #flexoki::flexoki$base["black"],
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "#100F0F" #flexoki::flexoki$base["black"],
) {
  theme_ggrefine(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    title_size = title_size,
    title_family = title_family,
    title_colour = title_colour,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_colour = subtitle_colour,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_colour = caption_colour,
    caption_hjust = caption_hjust,
    legend_position = legend_position,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_linetype = legend_ticks_linetype,
    legend_ticks_length = legend_ticks_length,
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length = axis_ticks_length,
    plot_background_fill = plot_background_fill,
    panel_background_fill = panel_background_fill,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linetype = panel_grid_linetype,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_grid_minor_linetype = panel_grid_minor_linetype,
    panel_grid_minor_linewidth = panel_grid_minor_linewidth
  )
}

#' Move Legend
#'
#' Helper shortcut to set legend position, location, and margins.
#'
#' @param position Character. Where to place the legend ("right", "top", or "bottom").
#' @return A ggplot2 theme object.
#' @noRd
refine_legend <- function(position = "right") {

  if (position == "right") {
    return(
      ggplot2::theme(
        legend.position = "right",
        legend.location = "panel",
        legend.margin   = ggplot2::margin(l = 5.5, b = 11),
        legend.justification = "top",
        legend.title.position = "top",
        legend.byrow = FALSE
      )
    )
  }

  if (position == "top") {
    return(
      ggplot2::theme(
        legend.position = "top",
        legend.location = "plot",
        legend.margin   = ggplot2::margin(r = 5.5, b = 11),
        legend.justification = "left",
        legend.title.position = "top",
        legend.byrow = TRUE
      )
    )
  }

  if (position == "bottom") {
    return(
      ggplot2::theme(
        legend.position = "bottom",
        legend.location = "plot",
        legend.margin   = ggplot2::margin(r = 5.5, b = 11),
        legend.justification = "left",
        legend.title.position = "top",
        legend.byrow = TRUE
      )
    )
  }
}

#' Lighter theme
#'
#' @description A complete theme for a white panel background.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param text_size The base size of the text theme element. Defaults to 10.
#' @param text_family The base family of the text theme element. Defaults to "".
#' @param text_colour The base colour of the text theme element.
#' @param legend_place The place of the legend. Either "right", "top" or "bottom".
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
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linetype The linetype of the panel.grid.major theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid.major theme element.
#' @param panel_grid_minor_linetype The linetype of the panel.grid.minor theme element.
#' @param panel_grid_minor_linewidth The linewidth of the panel.grid.minor theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_lighter(legend_place = "top"))
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
    text_size = 10,
    text_family = "",
    text_colour = flexoki::flexoki$base["black"],
    legend_place = "right",
    legend_axis_line_colour = NULL,
    legend_axis_line_linewidth = NULL,
    legend_background_fill = NULL,
    legend_key_fill = NULL,
    legend_ticks_colour = NULL,
    legend_ticks_linewidth = NULL,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = flexoki::flexoki$base["base600"],
    axis_line_linewidth = 0.25,
    axis_ticks_colour = NULL,
    axis_ticks_linewidth = NULL,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = "white",
    panel_grid_colour = flexoki::flexoki$base["base50"],
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "white"
) {

  # Process primary color parameters first
  text_colour <- as.character(text_colour)
  panel_background_fill <- as.character(panel_background_fill)
  plot_background_fill <- as.character(plot_background_fill)

  # Set defaults for dependent parameters after processing primary colors
  if (rlang::is_null(axis_line_colour)) axis_line_colour <- text_colour
  if (rlang::is_null(axis_ticks_colour)) axis_ticks_colour <- axis_line_colour
  if (rlang::is_null(axis_ticks_linewidth)) axis_ticks_linewidth <- axis_line_linewidth
  if (rlang::is_null(legend_axis_line_colour)) legend_axis_line_colour <- plot_background_fill
  if (rlang::is_null(legend_axis_line_linewidth)) legend_axis_line_linewidth <- axis_line_linewidth
  if (rlang::is_null(legend_background_fill)) legend_background_fill <- plot_background_fill
  if (rlang::is_null(legend_key_fill)) legend_key_fill <- plot_background_fill
  if (rlang::is_null(legend_ticks_colour)) legend_ticks_colour <- legend_axis_line_colour
  if (rlang::is_null(legend_ticks_linewidth)) legend_ticks_linewidth <- legend_axis_line_linewidth

  # Process dependent color parameters
  axis_line_colour <- as.character(axis_line_colour)
  axis_ticks_colour <- as.character(axis_ticks_colour)
  legend_axis_line_colour <- as.character(legend_axis_line_colour)
  legend_background_fill <- as.character(legend_background_fill)
  legend_key_fill <- as.character(legend_key_fill)
  legend_ticks_colour <- as.character(legend_ticks_colour)

  title_size <- text_size
  title_family <- text_family
  title_colour <- text_colour
  subtitle_size <- text_size
  subtitle_family <- text_family
  subtitle_colour <- text_colour
  caption_size <- ggplot2::rel(0.9)
  caption_family <- text_family

  caption_colour <- text_colour
  # caption_colour <- ifelse(
  #   is_panel_dark(),
  #   blend_multiply(text_colour),
  #   blend_screen(text_colour)
  # )
  caption_hjust <- 0

  # Base theme (same for all legend positions)
  theme <-
    # ggplot2::theme_grey(text_size = text_size, text_family = text_family) %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size,
        family = text_family,
        colour = text_colour,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = ggplot2::margin(),
        # debug = FALSE
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
        margin = ggplot2::margin(r = 2.75),
        hjust = 1
      ),
      axis.text.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 2.75),
        hjust = 0
      ),
      panel.background = ggplot2::element_rect(
        colour = panel_background_fill,
        fill = panel_background_fill
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(
        colour = panel_grid_colour,
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
        margin = ggplot2::margin(t = 0, r = 0, b = 8, l = 0)
      ),
      strip.text.x = NULL,
      strip.text.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(t = 8, r = 0, b = 0, l = 0)
      ),
      strip.text.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 8),
        angle = -90
      ),
      strip.text.y.left = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 8, b = 0, l = 0),
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
        colour = title_colour,
        hjust = 0,
        margin = ggplot2::margin(b = 27.50)
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        size = subtitle_size,
        family = subtitle_family,
        colour = subtitle_colour,
        hjust = 0,
        margin = ggplot2::margin(b = 22)
      ),
      plot.caption = ggplot2::element_text(
        size = caption_size,
        family = caption_family,
        colour = caption_colour,
        hjust = caption_hjust,
        margin = ggplot2::margin(t = 7.5, r = 0, b = 0, l = 0)
      ),
      plot.caption.position = "plot",
      plot.tag = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        hjust = 0,
        vjust = 0.5
      ),
      plot.tag.position = "topleft",
      plot.margin = ggplot2::margin(7.5, 17.5, 7.5, 7.5),
      complete = TRUE
    )

  # Apply legend position specific settings
  theme + legend_place(
    legend_place = legend_place,
    legend_key_fill = legend_key_fill,
    legend_background_fill = legend_background_fill,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_length = legend_ticks_length
  )
}

#' Beiger theme
#'
#' @description A complete theme with a beige panel background.
#'
#' @inheritParams theme_lighter
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_beiger(legend_position = "top"))
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
theme_beiger <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = flexoki::flexoki$base["black"],
    legend_place = "right",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = axis_line_linewidth,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = flexoki::flexoki$base["base600"],
    axis_line_linewidth = 0.25,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = flexoki::flexoki$base["base50"],
    panel_grid_colour = blend_multiply(panel_background_fill),
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "white"
) {
  theme_lighter(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    legend_place = legend_place,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
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


#' Greyer theme
#'
#' @description A complete theme with a greyer panel background.
#'
#' @inheritParams theme_lighter
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_greyer(legend_position = "top"))
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
theme_greyer <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = flexoki::flexoki$base["black"],
    legend_place = "right",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = axis_line_linewidth,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = flexoki::flexoki$base["base600"],
    axis_line_linewidth = 0.25,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = grey,
    panel_grid_colour = blend_multiply(panel_background_fill),
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = "white"
) {
  theme_lighter(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    legend_place = legend_place,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
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

#' Darker theme
#'
#' @description A complete theme for a dark panel background.
#'
#' @inheritParams theme_lighter
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_darker(legend_place = "top"))
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
theme_darker <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = flexoki::flexoki$base["base50"],
    legend_place = "right",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = axis_line_linewidth,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = flexoki::flexoki$base["base400"],
    axis_line_linewidth = 0.25,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length = grid::unit(3.66, "pt"),
    panel_background_fill = flexoki::flexoki$base["base950"],
    panel_grid_colour = flexoki::flexoki$base["black"],
    panel_grid_linetype = 1,
    panel_grid_linewidth = 1,
    panel_grid_minor_linetype = 1,
    panel_grid_minor_linewidth = 0.5,
    plot_background_fill = flexoki::flexoki$base["black"]
) {
  theme_lighter(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    legend_place = legend_place,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
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

#' Move the legend place
#'
#' @description Set legend position with optimized spacing for each placement.
#'
#' @param legend_place The position of the legend. Either "right", "top" or "bottom".
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#' @param legend_background_fill The fill (and colour) of the legend.background theme element.
#' @param legend_axis_line_colour The colour of the legend.axis.line theme element.
#' @param legend_axis_line_linewidth The linewidth of the legend.axis.line theme element.
#' @param legend_ticks_colour The colour of the legend.ticks theme element.
#' @param legend_ticks_linewidth The linewidth of the legend.ticks theme element.
#' @param legend_ticks_length The legend.ticks.length theme element.
#'
#' @return A ggplot theme object with legend position settings.
#' @export
#'
legend_place <- function(legend_place = "right",
                         legend_key_fill = NULL,
                         legend_background_fill = NULL,
                         legend_axis_line_colour = NULL,
                         legend_axis_line_linewidth = NULL,
                         legend_ticks_colour = NULL,
                         legend_ticks_linewidth = NULL,
                         legend_ticks_length = NULL) {

  current_theme <- ggplot2::get_theme()

  if (rlang::is_null(legend_key_fill)) legend_key_fill <- ggplot2::calc_element("legend.key", current_theme)@fill
  if (rlang::is_null(legend_background_fill)) legend_background_fill <- ggplot2::calc_element("legend.background", current_theme)@fill
  if (rlang::is_null(legend_axis_line_colour)) legend_axis_line_colour <- ggplot2::calc_element("legend.axis.line", current_theme)@colour
  if (rlang::is_null(legend_axis_line_linewidth)) legend_axis_line_linewidth <- ggplot2::calc_element("legend.axis.line", current_theme)@linewidth
  if (rlang::is_null(legend_ticks_colour)) legend_ticks_colour <- ggplot2::calc_element("legend.ticks", current_theme)@colour
  if (rlang::is_null(legend_ticks_linewidth)) legend_ticks_linewidth <- ggplot2::calc_element("legend.ticks", current_theme)@linewidth
  if (rlang::is_null(legend_ticks_length)) legend_ticks_length <- ggplot2::calc_element("legend.ticks.length", current_theme)

  if (!legend_place %in% c("right", "top", "bottom")) {
    rlang::abort("legend_place must be 'right', 'top', or 'bottom'")
  }

  if (legend_place == "right") {
    ggplot2::theme(
      # All legend elements for right position
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.location = "panel",
      legend.title.position = "top",
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 2.75),
      legend.box = "horizontal",
      legend.box.just = "top",
      legend.box.margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 5),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 5.5, l = 0)),
      legend.byrow = FALSE,
      legend.direction = "vertical",
      legend.spacing = grid::unit(11, "pt"),
      legend.spacing.y = grid::unit(16.50, "pt"),
      legend.spacing.x = NULL,
      legend.key = ggplot2::element_rect(
        colour = legend_key_fill,
        fill = legend_key_fill
      ),
      legend.key.height = ggplot2::rel(1),
      legend.key.width = ggplot2::rel(0.6),
      legend.key.spacing = NULL,
      legend.key.spacing.x = grid::unit(11, "pt"),
      legend.key.spacing.y = grid::unit(3.63, "pt"),
      legend.frame = NULL,
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(5.5, 0, 5.5, 5.5)
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
      legend.box.background = NULL,
      legend.box.spacing = NULL,
      legend.background = ggplot2::element_rect(
        colour = legend_background_fill,
        fill = legend_background_fill
      ),

      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.50)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 5.50)),

      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 1.87, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 8.25, l = 0)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 11, b = 0, l = 0), angle = 90),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5.50), angle = -90),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = 5.50, r = 0, b = 8.25, l = 0)), ###
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = 0, r = 0, b = 5.50, l = 0))
    )
  } else if (legend_place == "top") {
    ggplot2::theme(
      # All legend elements for top position
      legend.position = "top",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.title.position = "top",
      legend.margin = ggplot2::margin(t = 0, r = 22, b = 0, l = 0),
      legend.box = "vertical",
      legend.box.just = "left",
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 5, l = 0),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 5.5, l = 0)),
      legend.byrow = TRUE,
      legend.direction = "horizontal",
      legend.spacing = grid::unit(11, "pt"),
      legend.spacing.y = grid::unit(16.50, "pt"),
      legend.spacing.x = NULL,
      legend.key = ggplot2::element_rect(
        colour = legend_key_fill,
        fill = legend_key_fill
      ),
      legend.key.height = ggplot2::rel(1),
      legend.key.width = ggplot2::rel(0.6),

      legend.key.spacing = NULL,
      legend.key.spacing.x = grid::unit(11, "pt"),
      legend.key.spacing.y = grid::unit(3.63, "pt"),
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
      legend.box.background = NULL,
      legend.box.spacing = NULL,
      legend.background = ggplot2::element_rect(
        colour = legend_background_fill,
        fill = legend_background_fill
      ),

      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.50)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 5.50)),

      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 4.62, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 5.50, l = 0)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 11, b = 0, l = 0), angle = 90),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5.50), angle = -90),
      axis.text.x = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(t = 3.30, r = 0, b = 8.25, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(t = 0, r = 0, b = 3.30, l = 0))
    )
  } else if (legend_place == "bottom") {
    ggplot2::theme(
      # All legend elements for bottom position
      legend.position = "bottom",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.title.position = "top",
      legend.margin = ggplot2::margin(t = 0, r = 22, b = 0, l = 0),
      legend.box = "vertical",
      legend.box.just = "left",
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 2.75, r = 0, b = 5.50, l = 0)),
      legend.byrow = TRUE,
      legend.direction = "horizontal",
      legend.spacing = grid::unit(11, "pt"),
      legend.spacing.y = grid::unit(16.50, "pt"),
      legend.spacing.x = NULL,
      legend.key = ggplot2::element_rect(
        colour = legend_key_fill,
        fill = legend_key_fill
      ),
      legend.key.height = ggplot2::rel(1),
      legend.key.width = ggplot2::rel(0.6),

      legend.key.spacing = NULL,
      legend.key.spacing.x = grid::unit(11, "pt"),
      legend.key.spacing.y = grid::unit(3.63, "pt"),
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
      legend.box.background = NULL,
      legend.box.spacing = NULL,
      legend.background = ggplot2::element_rect(
        colour = legend_background_fill,
        fill = legend_background_fill
      ),

      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.50)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 5.50)),

      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 4.62, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 5.50, l = 0)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 11, b = 0, l = 0), angle = 90),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5.50), angle = -90),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = 3.30, r = 0, b = 8.25, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = 0, r = 0, b = 3.30, l = 0))
    )
  }
}


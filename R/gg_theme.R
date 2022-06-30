#' @title Quick theme for a ggplot.
#'
#' @description Quick theme for a ggplot visualisation.
#' @param font The font for all text to use. Defaults to "".
#' @param font_title The font for the title. If NULL, inherits from font argument.
#' @param font_subtitle The font for the subtitle. If NULL, inherits from font argument.
#' @param font_body The font for all text other than the title, subtitle and caption. If NULL, inherits from font argument.
#' @param font_caption The font for the caption. If NULL, inherits from font argument.
#' @param size_title The size of the title font. Defaults to 11.
#' @param size_subtitle The size of the subtitle font. Defaults to 10.
#' @param size_body The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param size_caption The size of the caption. Defaults to 9.
#' @param size_axis The size of the axis. Defaults to 0.3.
#' @param size_ticks The size of the ticks. Defaults to 0.3.
#' @param size_grid The size of the vertical major gridlines. Defaults to 0.2.
#' @param style_title The style of the title font. Defaults to "bold".
#' @param style_subtitle The style of the subtitle font. Defaults to "plain".
#' @param style_body The style of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param style_caption The style of the caption. Defaults to "plain".
#' @param pal_title The colour palette for the title font. Defaults to "#000000".
#' @param pal_subtitle The colour palette for the subtitle font. Defaults to "#323232".
#' @param pal_body The colour palette for all text other than the title, subtitle or caption. Defaults to "#323232".
#' @param pal_caption The colour palette for the caption. Defaults to "#7F7F7F".
#' @param pal_axis The colour palette for the axis. Defaults to "#323232".
#' @param pal_ticks The colour palette for the ticks. Defaults to "#323232".
#' @param pal_background A two colour vector. The first colour if for the panel (and legend key). The second colour is for the rest of the background.
#' @param pal_grid The colour palette for the vertical major gridlines. Defaults to "#D3D3D3".
#' @param y_grid TRUE or FALSE of whether to show hotizontal gridlines.
#' @param x_grid TRUE or FALSE of whether to show vertical gridlines.
#' @param void TRUE or FALSE of whether to drop all axis lines, ticks and x and y labels. Useful for maps. Defaults to FALSE.
#'
#' @return A ggplot theme.
#' @export
#'
gg_theme <- function(font = "",
                     font_title = NULL,
                     font_subtitle = NULL,
                     font_body = NULL,
                     font_caption = NULL,
                     size_title = 11,
                     size_subtitle = 10,
                     size_body = 10,
                     size_caption = 9,
                     size_axis = 0.3,
                     size_ticks = 0.3,
                     size_grid = 0.2,
                     style_title = "bold",
                     style_subtitle = "plain",
                     style_body = "plain",
                     style_caption = "plain",
                     pal_title = "#000000",
                     pal_subtitle = "#323232",
                     pal_body = "#323232",
                     pal_caption = "#7F7F7F",
                     pal_axis = "#323232",
                     pal_ticks = "#323232",
                     pal_background = c("#ffffff", "#ffffff"),
                     pal_grid = "#D3D3D3",
                     x_grid = FALSE,
                     y_grid = FALSE,
                     void = FALSE) {

    if (is.null(font_title)) font_title <- font
    if (is.null(font_subtitle)) font_subtitle <- font
    if (is.null(font_body)) font_body <- font

    if (y_grid == TRUE) {
      if (x_grid == FALSE) { #horizontal
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = ggplot2::element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0, vjust = size_body / 5, margin = ggplot2::margin(b = size_body)),
          plot.subtitle = ggplot2::element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0, vjust = size_body / 2.5, margin = ggplot2::margin(t = size_body * -0.2, b = size_body * 0.3)),
          axis.title.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = size_body / 1.33)),
          axis.title.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = ggplot2::margin(r = size_body)),
          legend.title = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = font_caption, size = size_caption, colour = pal_caption, face = style_caption, hjust = 1, margin = ggplot2::margin(t = size_caption)),
          axis.text.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = ggplot2::margin(b = size_body / 2)),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = pal_grid, size = size_grid),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          panel.background = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          strip.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = ggplot2::element_line(colour = pal_axis, size = size_axis),
          axis.ticks = ggplot2::element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = ggplot2::unit(5, "mm"),
          legend.key.width = ggplot2::unit(5, "mm"),
          legend.spacing.y = ggplot2::unit(0.15, "cm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if (x_grid == TRUE) { #both
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = ggplot2::element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0, vjust = size_body / 5, margin = ggplot2::margin(b = size_body)),
          plot.subtitle = ggplot2::element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0, vjust = size_body / 2.5, margin = ggplot2::margin(t = size_body * -0.2, b = size_body * 0.3)),
          axis.title.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = size_body / 1.33)),
          axis.title.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = ggplot2::margin(r = size_body)),
          legend.title = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = font_caption, size = size_caption, colour = pal_caption, face = style_caption, hjust = 1, margin = ggplot2::margin(t = size_caption)),
          axis.text.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = ggplot2::margin(b = size_body / 2)),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          plot.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          panel.background = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          panel.grid.major = ggplot2::element_line(colour = pal_grid, size = size_grid),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          strip.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = ggplot2::element_line(colour = pal_axis, size = size_axis),
          axis.ticks = ggplot2::element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = ggplot2::unit(5, "mm"),
          legend.key.width = ggplot2::unit(5, "mm"),
          legend.spacing.y = ggplot2::unit(0.15, "cm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
    }
    else if (y_grid == FALSE) {
      if (x_grid == FALSE) { #none
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = ggplot2::element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0, vjust = size_body / 5, margin = ggplot2::margin(b = size_body)),
          plot.subtitle = ggplot2::element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0, vjust = size_body / 2.5, margin = ggplot2::margin(t = size_body * -0.2, b = size_body * 0.3)),
          axis.title.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = size_body / 1.33)),
          axis.title.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = ggplot2::margin(r = size_body)),
          legend.title = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = font_caption, size = size_caption, colour = pal_caption, face = style_caption, hjust = 1, margin = ggplot2::margin(t = size_caption)),
          axis.text.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = ggplot2::margin(b = size_body / 2)),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          panel.background = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          strip.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = ggplot2::element_line(colour = pal_axis, size = size_axis),
          axis.ticks = ggplot2::element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = ggplot2::unit(5, "mm"),
          legend.key.width = ggplot2::unit(5, "mm"),
          legend.spacing.y = ggplot2::unit(0.15, "cm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if (x_grid == TRUE) { #vertical
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = ggplot2::element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0, vjust = size_body / 5, margin = ggplot2::margin(b = size_body)),
          plot.subtitle = ggplot2::element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0, vjust = size_body / 2.5, margin = ggplot2::margin(t = size_body * -0.2, b = size_body * 0.3)),
          axis.title.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = size_body / 1.33)),
          axis.title.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = ggplot2::margin(r = size_body)),
          legend.title = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = font_caption, size = size_caption, colour = pal_caption, face = style_caption, hjust = 1, margin = ggplot2::margin(t = size_caption)),
          axis.text.x = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text = ggplot2::element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = ggplot2::margin(b = size_body / 2)),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          plot.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          panel.background = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          panel.grid.major.x = ggplot2::element_line(colour = pal_grid, size = size_grid),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          strip.background = ggplot2::element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = ggplot2::element_line(colour = pal_axis, size = size_axis),
          axis.ticks = ggplot2::element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = ggplot2::unit(5, "mm"),
          legend.key.width = ggplot2::unit(5, "mm"),
          legend.spacing.y = ggplot2::unit(0.15, "cm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
    }

    if (void == TRUE) {
      theme <- theme +
        ggplot2::theme(axis.line = ggplot2::element_blank()) +
        ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }

    return(theme)
  }

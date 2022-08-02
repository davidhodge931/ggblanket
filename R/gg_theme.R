#' @title Quick theme for a ggplot.
#'
#' @description Quick theme for a ggplot visualisation.
#' @param font The font for all text to use. Defaults to "".
#' @param title_font The font for the title. If NULL, inherits from font argument.
#' @param title_pal The colour palette for the title font. Defaults to "#000000".
#' @param title_size The size of the title font. Defaults to 11.
#' @param title_style The style of the title font. Defaults to "bold".
#' @param subtitle_font The font for the subtitle. If NULL, inherits from font argument.
#' @param subtitle_pal The colour palette for the subtitle font. Defaults to "#323232".
#' @param subtitle_size The size of the subtitle font. Defaults to 10.
#' @param subtitle_style The style of the subtitle font. Defaults to "plain".
#' @param body_font The font for all text other than the title, subtitle and caption. If NULL, inherits from font argument.
#' @param body_pal The colour palette for all text other than the title, subtitle or caption. Defaults to "#323232".
#' @param body_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param body_style The style of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param caption_font The font for the caption. If NULL, inherits from font argument.
#' @param caption_pal The colour palette for the caption. Defaults to "#7F7F7F".
#' @param caption_size The size of the caption. Defaults to 9.
#' @param caption_style The style of the caption. Defaults to "plain".
#' @param axis_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_size The size of the axis. Defaults to 0.3.
#' @param ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param ticks_size The size of the ticks. Defaults to 0.3.
#' @param bg_panel_pal The colour palette for the panel background colour.
#' @param bg_plot_pal The colour palette for the plot background colour.
#' @param bg_legend_pal The colour palette for the legend key. Can also use special values of "plot" and "panel".
#' @param grid_h TRUE or FALSE of whether to show hotizontal gridlines. Defaults to FALSE.
#' @param grid_v TRUE or FALSE of whether to show vertical gridlines. Defaults to FALSE.
#' @param grid_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3".
#' @param grid_size The size of the vertical major gridlines. Defaults to 0.2.
#'
#' @return A ggplot theme.
#' @export
#'
gg_theme <- function(font = "",
                     title_pal = "#000000",
                     title_font = NULL,
                     title_size = 11,
                     title_style = "bold",
                     subtitle_font = NULL,
                     subtitle_pal = "#323232",
                     subtitle_size = 10,
                     subtitle_style = "plain",
                     body_font = NULL,
                     body_pal = "#323232",
                     body_size = 10,
                     body_style = "plain",
                     caption_font = NULL,
                     caption_pal = "#7F7F7F",
                     caption_size = 9,
                     caption_style = "plain",
                     axis_size = 0.3,
                     axis_pal = "#323232",
                     ticks_size = 0.3,
                     ticks_pal = "#323232",
                     bg_plot_pal = "#F1F3F5",
                     bg_panel_pal = "#FEFEFE",
                     bg_legend_pal = "plot",
                     grid_h = FALSE,
                     grid_v = FALSE,
                     grid_pal = "#D3D3D3",
                     grid_size = 0.2) {

    if (is.null(title_font)) title_font <- font
    if (is.null(subtitle_font)) subtitle_font <- font
    if (is.null(body_font)) body_font <- font

    if (bg_legend_pal == "plot") bg_legend_pal <- bg_plot_pal
    else if (bg_legend_pal == "panel") bg_legend_pal <- bg_panel_pal

    if (grid_h == TRUE) {
      if (grid_v == FALSE) { #horizontal
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style),
          plot.title = ggplot2::element_text(family = title_font, size = title_size, colour = title_pal, face = title_style, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
          plot.subtitle = ggplot2::element_text(family = subtitle_font, size = subtitle_size, colour = subtitle_pal, face = subtitle_style, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
          axis.title.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = body_size / 1.33)),
          axis.title.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, angle = 90, margin = ggplot2::margin(r = body_size)),
          legend.title = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = caption_font, size = caption_size, colour = caption_pal, face = caption_style, hjust = 1, margin = ggplot2::margin(t = caption_size)),
          axis.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
          strip.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = grid_pal, size = grid_size),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
          legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
          axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = bg_legend_pal, fill = bg_legend_pal),
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
      else if (grid_v == TRUE) { #both
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style),
          plot.title = ggplot2::element_text(family = title_font, size = title_size, colour = title_pal, face = title_style, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
          plot.subtitle = ggplot2::element_text(family = subtitle_font, size = subtitle_size, colour = subtitle_pal, face = subtitle_style, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
          axis.title.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = body_size / 1.33)),
          axis.title.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, angle = 90, margin = ggplot2::margin(r = body_size)),
          legend.title = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = caption_font, size = caption_size, colour = caption_pal, face = caption_style, hjust = 1, margin = ggplot2::margin(t = caption_size)),
          axis.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
          strip.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
          panel.grid.major = ggplot2::element_line(colour = grid_pal, size = grid_size),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
          axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = bg_legend_pal, fill = bg_legend_pal),
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
    else if (grid_h == FALSE) {
      if (grid_v == FALSE) { #none
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style),
          plot.title = ggplot2::element_text(family = title_font, size = title_size, colour = title_pal, face = title_style, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
          plot.subtitle = ggplot2::element_text(family = subtitle_font, size = subtitle_size, colour = subtitle_pal, face = subtitle_style, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
          axis.title.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = body_size / 1.33)),
          axis.title.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, angle = 90, margin = ggplot2::margin(r = body_size)),
          legend.title = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = caption_font, size = caption_size, colour = caption_pal, face = caption_style, hjust = 1, margin = ggplot2::margin(t = caption_size)),
          axis.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
          strip.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
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
          plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
          legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
          axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = bg_legend_pal, fill = bg_legend_pal),
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
      else if (grid_v == TRUE) { #vertical
        theme <- ggplot2::theme(
          text = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style),
          plot.title = ggplot2::element_text(family = title_font, size = title_size, colour = title_pal, face = title_style, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
          plot.subtitle = ggplot2::element_text(family = subtitle_font, size = subtitle_size, colour = subtitle_pal, face = subtitle_style, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
          axis.title.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = body_size / 1.33)),
          axis.title.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, angle = 90, margin = ggplot2::margin(r = body_size)),
          legend.title = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0, vjust = 0.5),
          plot.caption = ggplot2::element_text(family = caption_font, size = caption_size, colour = caption_pal, face = caption_style, hjust = 1, margin = ggplot2::margin(t = caption_size)),
          axis.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, margin = ggplot2::margin(t = 2)),
          axis.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 1, margin = ggplot2::margin(r = 2)),
          strip.text.x = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
          strip.text.y = ggplot2::element_text(family = body_font, size = body_size, colour = body_pal, face = body_style, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
          legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
          plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.5, "lines"),
          plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
          panel.grid.major.x = ggplot2::element_line(colour = grid_pal, size = grid_size),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
          axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
          axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
          legend.margin = ggplot2::margin(),
          legend.key = ggplot2::element_rect(colour = bg_legend_pal, fill = bg_legend_pal),
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

    return(theme)
  }

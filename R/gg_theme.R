#' @title Quick theme for a ggplot.
#'
#' @description Quick theme for a ggplot visualisation.
#' @param family The font family for all text to use. Defaults to "".
#' @param title_family The font family for the title. If NULL, inherits from family argument.
#' @param title_pal The colour palette for the title family. Defaults to "#000000".
#' @param title_size The size of the title family. Defaults to 11.
#' @param title_face The font style of the title family. Defaults to "bold".
#' @param subtitle_family The font family for the subtitle. If NULL, inherits from family argument.
#' @param subtitle_pal The colour palette for the subtitle family. Defaults to "#323232".
#' @param subtitle_size The size of the subtitle family. Defaults to 10.
#' @param subtitle_face The font style of the subtitle family. Defaults to "plain".
#' @param body_family The font family for all text other than the title, subtitle and caption. If NULL, inherits from family argument.
#' @param body_pal The colour palette for all text other than the title, subtitle or caption. Defaults to "#323232".
#' @param body_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param body_face The font style of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param caption_family The font family for the caption. If NULL, inherits from family argument.
#' @param caption_pal The colour palette for the caption. Defaults to "#7F7F7F".
#' @param caption_size The size of the caption. Defaults to 9.
#' @param caption_face The font style of the caption. Defaults to "plain".
#' @param axis_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_size The size of the axis. Defaults to 0.3.
#' @param ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param ticks_size The size of the ticks. Defaults to 0.3.
#' @param bg_panel_pal The colour palette for the panel background colour.
#' @param bg_plot_pal The colour palette for the plot background colour.
#' @param bg_legend_key_pal The colour palette for the legend key. Can also use special values of "plot" and "panel".
#' @param grid_h TRUE or FALSE of whether to show hotizontal gridlines. Defaults to FALSE.
#' @param grid_v TRUE or FALSE of whether to show vertical gridlines. Defaults to FALSE.
#' @param grid_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3".
#' @param grid_size The size of the vertical major gridlines. Defaults to 0.2.
#' @param facet_gap_size The size of the spacing between facet panels in units of "lines". Defaults to 1.5.
#' @param void TRUE or FALSE of whether to remove axis lines, ticks and x and y titles and labels.
#'
#' @return A ggplot theme.
#' @export
#'
gg_theme <- function(
    family = "",
    title_pal = "#000000",
    title_family = NULL,
    title_size = 11,
    title_face = "bold",
    subtitle_family = NULL,
    subtitle_pal = "#323232",
    subtitle_size = 10,
    subtitle_face = "plain",
    body_family = NULL,
    body_pal = "#323232",
    body_size = 10,
    body_face = "plain",
    caption_family = NULL,
    caption_pal = "#7F7F7F",
    caption_size = 9,
    caption_face = "plain",
    axis_size = 0.3,
    axis_pal = "#323232",
    ticks_size = 0.3,
    ticks_pal = "#323232",
    bg_plot_pal = "#F1F3F5",
    bg_panel_pal = "#FEFEFE",
    bg_legend_key_pal = "plot",
    grid_h = FALSE,
    grid_v = FALSE,
    grid_pal = "#D3D3D3",
    grid_size = 0.2,
    facet_gap_size = 1.5,
    void = FALSE) {

  if (is.null(title_family)) title_family <- family
  if (is.null(subtitle_family)) subtitle_family <- family
  if (is.null(body_family)) body_family <- family

  if (bg_legend_key_pal == "plot") bg_legend_key_pal <- bg_plot_pal
  else if (bg_legend_key_pal == "panel") bg_legend_key_pal <- bg_panel_pal

  if (grid_h == TRUE) {
    if (grid_v == FALSE) { #horizontal
      theme <- ggplot2::theme(
        text = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face),
        plot.title = ggplot2::element_text(family = title_family, size = title_size, colour = title_pal, face = title_face, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
        plot.subtitle = ggplot2::element_text(family = subtitle_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
        axis.title.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = body_size / 1.33)),
        axis.title.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = ggplot2::margin(r = body_size)),
        legend.title = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0, vjust = 0.5),
        plot.caption = ggplot2::element_text(family = caption_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = 1, margin = ggplot2::margin(t = caption_size)),
        axis.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = 2)),
        axis.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = ggplot2::margin(r = 2)),
        strip.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
        strip.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
        legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
        plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.border = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(facet_gap_size, "lines"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = grid_pal, size = grid_size),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
        legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.placement = "outside",
        axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
        axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = ggplot2::margin(),
        legend.key = ggplot2::element_rect(colour = bg_legend_key_pal, fill = bg_legend_key_pal),
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
        text = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face),
        plot.title = ggplot2::element_text(family = title_family, size = title_size, colour = title_pal, face = title_face, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
        plot.subtitle = ggplot2::element_text(family = subtitle_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
        axis.title.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = body_size / 1.33)),
        axis.title.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = ggplot2::margin(r = body_size)),
        legend.title = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0, vjust = 0.5),
        plot.caption = ggplot2::element_text(family = caption_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = 1, margin = ggplot2::margin(t = caption_size)),
        axis.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = 2)),
        axis.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = ggplot2::margin(r = 2)),
        strip.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
        strip.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
        legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
        plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.border = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(facet_gap_size, "lines"),
        plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
        panel.grid.major = ggplot2::element_line(colour = grid_pal, size = grid_size),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.placement = "outside",
        axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
        axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = ggplot2::margin(),
        legend.key = ggplot2::element_rect(colour = bg_legend_key_pal, fill = bg_legend_key_pal),
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
        text = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face),
        plot.title = ggplot2::element_text(family = title_family, size = title_size, colour = title_pal, face = title_face, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
        plot.subtitle = ggplot2::element_text(family = subtitle_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
        axis.title.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = body_size / 1.33)),
        axis.title.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = ggplot2::margin(r = body_size)),
        legend.title = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0, vjust = 0.5),
        plot.caption = ggplot2::element_text(family = caption_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = 1, margin = ggplot2::margin(t = caption_size)),
        axis.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = 2)),
        axis.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = ggplot2::margin(r = 2)),
        strip.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
        strip.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
        legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
        plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.border = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(facet_gap_size, "lines"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
        legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.placement = "outside",
        axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
        axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = ggplot2::margin(),
        legend.key = ggplot2::element_rect(colour = bg_legend_key_pal, fill = bg_legend_key_pal),
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
        text = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face),
        plot.title = ggplot2::element_text(family = title_family, size = title_size, colour = title_pal, face = title_face, hjust = 0, vjust = body_size / 5, margin = ggplot2::margin(b = body_size)),
        plot.subtitle = ggplot2::element_text(family = subtitle_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0, vjust = body_size / 2.5, margin = ggplot2::margin(t = body_size * -0.2, b = body_size * 0.3)),
        axis.title.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = body_size / 1.33)),
        axis.title.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = ggplot2::margin(r = body_size)),
        legend.title = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0, vjust = 0.5),
        plot.caption = ggplot2::element_text(family = caption_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = 1, margin = ggplot2::margin(t = caption_size)),
        axis.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, margin = ggplot2::margin(t = 2)),
        axis.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = ggplot2::margin(r = 2)),
        strip.text.x = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(b = body_size / 2)),
        strip.text.y = ggplot2::element_text(family = body_family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = ggplot2::margin(l = body_size / 2), angle = 270),
        legend.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 7.5, unit = "pt")),
        plot.margin = ggplot2::margin(t = 15, l = 7.5, b = 10, r = 20),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.border = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(facet_gap_size, "lines"),
        plot.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        panel.background = ggplot2::element_rect(colour = bg_panel_pal, fill = bg_panel_pal),
        panel.grid.major.x = ggplot2::element_line(colour = grid_pal, size = grid_size),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.background = ggplot2::element_rect(colour = bg_plot_pal, fill = bg_plot_pal),
        strip.placement = "outside",
        axis.line = ggplot2::element_line(colour = axis_pal, size = axis_size),
        axis.ticks = ggplot2::element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = ggplot2::margin(),
        legend.key = ggplot2::element_rect(colour = bg_legend_key_pal, fill = bg_legend_key_pal),
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

  if (void) {
    theme <- theme +
      ggplot2::theme(axis.text = ggplot2::element_blank()) +
      ggplot2::theme(axis.line = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  }

  return(theme)
}

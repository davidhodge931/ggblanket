#' @title Quick theme for a ggplot.
#'
#' @description Quick theme for a ggplot visualisation.
#' @param text_family The font family for all text to use. Defaults to "".
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param text_pal The colour palette for all text other than the title, subtitle or caption. Defaults to "#323232".
#' @param text_face The font style of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param title_size The size of the title text_family. Defaults to 11.
#' @param title_pal The colour palette for the title text_family. Defaults to "#000000".
#' @param title_face The font style of the title text_family. Defaults to "bold".
#' @param title_vjust The vertical adjustment for the title.
#' @param title_margin The margin for the title.
#' @param subtitle_size The size of the subtitle text_family. Defaults to 10.
#' @param subtitle_pal The colour palette for the subtitle text_family. Defaults to "#323232".
#' @param subtitle_face The font style of the subtitle text_family. Defaults to "plain".
#' @param subtitle_vjust The vertical adjustment for the subtitle.
#' @param subtitle_margin The margin for the title.
#' @param caption_size The size of the caption. Defaults to 9.
#' @param caption_pal The colour palette for the caption. Defaults to "#7F7F7F".
#' @param caption_face The font style of the caption. Defaults to "plain".
#' @param caption_hjust The horizontal adjustment for the caption.
#' @param axis_line_linewidth The size of the axis. Defaults to 0.3.
#' @param axis_line_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_ticks_linewidth The size of the ticks. Defaults to 0.3.
#' @param axis_ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param legend_key_pal The colour palette for the legend key. Defaults to the plot_background_pal.
#' @param panel_background_pal The colour palette for the panel background colour.
#' @param panel_grid_linewidth The size of the vertical major gridlines. Defaults to 0.2.
#' @param panel_grid_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3".
#' @param panel_spacing_units The size of the spacing between facet panels in units of "lines". Defaults to 1.5.
#' @param plot_background_pal The colour palette for the plot background colour.
#' @param void TRUE or FALSE of whether to remove axis lines, ticks and x and y titles and labels.
#'
#' @return A ggplot theme.
#' @export
#'
gg_theme2 <- function(
    text_family = "",
    text_size = 10,
    text_pal = "#323232",
    text_face = "plain",
    title_size = text_size + 1,
    title_pal = "#000000",
    title_face = "bold",
    title_vjust = text_size * 0.2,
    title_margin = ggplot2::margin(t = text_size * -0.5, b = text_size * 1.25),
    subtitle_size = text_size,
    subtitle_pal = "#323232",
    subtitle_face = "plain",
    subtitle_vjust = text_size * 0.4,
    subtitle_margin = ggplot2::margin(t = text_size * -0.5, b = text_size),
    caption_size = text_size - 1,
    caption_pal = "#7F7F7F",
    caption_face = "plain",
    caption_hjust = 0,
    axis_line_linewidth = 0.3,
    axis_line_pal = "#323232",
    axis_ticks_linewidth = NULL,
    axis_ticks_pal = NULL,
    legend_key_pal = NULL,
    panel_background_pal = "#FEFEFE",
    panel_grid_linewidth = 0.2,
    panel_grid_pal = "#D3D3D3",
    panel_spacing_units = 1.5,
    plot_background_pal = "#F1F3F5",
    void = FALSE) {

  if (is.null(axis_ticks_pal)) axis_ticks_pal <- axis_line_pal
  if (is.null(legend_key_pal)) legend_key_pal <- plot_background_pal
  if (is.null(axis_ticks_linewidth)) axis_ticks_linewidth <- axis_line_linewidth

  theme <- ggplot2::theme(
    text = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face),
    plot.title = ggplot2::element_text(family = text_family, size = title_size, colour = title_pal, face = title_face, hjust = 0, vjust = title_vjust, margin = title_margin),
    plot.subtitle = ggplot2::element_text(family = text_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
    axis.title.x = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, margin = ggplot2::margin(t = text_size * 0.75)),
    axis.title.y = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, angle = 90, margin = ggplot2::margin(r = text_size)),
    legend.title = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, hjust = 0, vjust = 0.5),
    plot.caption = ggplot2::element_text(family = text_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = caption_hjust, margin = ggplot2::margin(t = caption_size * 5/3)),
    axis.text.x = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, margin = ggplot2::margin(t = 2)),
    axis.text.y = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, hjust = 1, margin = ggplot2::margin(r = 2)),
    strip.text.x = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, hjust = 0.5, margin = ggplot2::margin(t = text_size * -0.25, b = text_size * 0.75)),
    strip.text.y = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, hjust = 0.5, margin = ggplot2::margin(l = text_size * 0.75), angle = 270),
    plot.margin = ggplot2::margin(t = 15, l = 10, b = 10, r = 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.border = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(panel_spacing_units, "lines"),
    plot.background = ggplot2::element_rect(colour = plot_background_pal, fill = plot_background_pal),
    panel.background = ggplot2::element_rect(colour = panel_background_pal, fill = panel_background_pal),
    panel.grid.major = ggplot2::element_line(colour = panel_grid_pal, size = panel_grid_linewidth),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(colour = plot_background_pal, fill = plot_background_pal),
    strip.background = ggplot2::element_rect(colour = plot_background_pal, fill = plot_background_pal),
    strip.placement = "outside",
    axis.line = ggplot2::element_line(colour = axis_line_pal, size = axis_line_linewidth),
    axis.ticks = ggplot2::element_line(colour = axis_ticks_pal, size = axis_ticks_linewidth),
    legend.margin = ggplot2::margin(),
    legend.key = ggplot2::element_rect(colour = legend_key_pal, fill = legend_key_pal),
    legend.key.height = ggplot2::unit(5, "mm"),
    legend.key.width = ggplot2::unit(5, "mm"),
    legend.spacing.y = ggplot2::unit(0.15, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box.margin = ggplot2::margin(t = -2.5),
    legend.text = ggplot2::element_text(
        margin = ggplot2::margin(r = 7.5)),
    legend.box = NULL,
    complete = TRUE
  )

  if (void) {
    theme <- theme +
      ggplot2::theme(axis.text = ggplot2::element_blank()) +
      ggplot2::theme(axis.line = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
      ggplot2::theme(axis.title = ggplot2::element_blank()) +
      theme(plot.margin = ggplot2::margin(t = 15, l = 20, b = 10, r = 20))
  }

  return(theme)
}

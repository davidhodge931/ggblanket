#' @title Quick theme for a ggplot
#'
#' @description Quick theme for a ggplot visualisation.
#' @param text_family The font family for all text to use. Defaults to "".
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param text_pal The colour for all text other than the title, subtitle or caption. Defaults to "#d3d3d3".
#' @param text_face The font style of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param title_size The size of the title text_family. Defaults to 11.
#' @param title_pal The colour for the title text_family. Defaults to "#d3d3d3".
#' @param title_face The font style of the title text_family. Defaults to "bold".
#' @param title_position The horizontal alignment of the title (and subtitle) to either "plot" or "panel".
#' @param title_hjust The horizontal adjustment for the title.
#' @param title_vjust The vertical adjustment for the title.
#' @param title_margin The margin for the title. A ggplot2::margin function.
#' @param subtitle_size The size of the subtitle text_family. Defaults to 10.
#' @param subtitle_pal The colour for the subtitle text_family. Defaults to "#a8a8a8".
#' @param subtitle_face The font style of the subtitle text_family. Defaults to "plain".
#' @param subtitle_hjust The horizontal adjustment for the subtitle.
#' @param subtitle_vjust The vertical adjustment for the subtitle.
#' @param subtitle_margin The margin for the title. A ggplot2::margin function.
#' @param caption_size The size of the caption. Defaults to 9.
#' @param caption_pal The colour for the caption. Defaults to "#777777".
#' @param caption_face The font style of the caption. Defaults to "plain".
#' @param caption_position The horizontal alignment of the caption to either "plot" or "panel".
#' @param caption_hjust The horizontal adjustment for the caption.
#' @param caption_vjust The vertical adjustment for the caption.
#' @param caption_margin The margin for the caption. A ggplot2::margin function.
#' @param line_linewidth The linewidth of the axis. Defaults to 0.125.
#' @param line_pal The colour for the axis. Defaults to "#adadad".
#' @param ticks_linewidth The linewidth of the ticks. Defaults to that of the line_linewidth argument.
#' @param ticks_length The length of the ticks. A grid::unit function.
#' @param ticks_pal The colour for the ticks. Defaults to the line_pal.
#' @param background_pal_plot The colour for the plot background colour. Defaults to "#000000".
#' @param background_pal_panel The colour for the panel background colour. Defaults to "#232323".
#' @param background_pal_key The colour for the legend key. Defaults to the background_pal_panel.
#' @param gridlines_linewidth The linewidth of the vertical major gridlines. Defaults to 0.1.
#' @param gridlines_pal The colour for the vertical major gridlines. Defaults to "#474747".
#'
#' @return A ggplot theme.
#' @export
#'
gg_theme_dark <- function(
    text_family = "",
    text_size = 10,
    text_pal = "#a8a8a8",
    text_face = "plain",
    title_size = text_size * 1.1,
    title_pal = "#d3d3d3",
    title_face = "bold",
    title_position = "plot",
    title_hjust = 0,
    title_vjust = 1,
    title_margin = ggplot2::margin(t = title_size * -0.5, b = title_size * 1.75),
    subtitle_size = text_size,
    subtitle_pal = "#a8a8a8",
    subtitle_face = "plain",
    subtitle_hjust = 0,
    subtitle_vjust = 1,
    subtitle_margin = ggplot2::margin(t = subtitle_size * -1.5, b = subtitle_size * 1.75),
    caption_size = text_size * 0.9,
    caption_pal = "#777777",
    caption_face = "plain",
    caption_position = "plot",
    caption_hjust = 0,
    caption_vjust = 0,
    caption_margin = ggplot2::margin(t = caption_size * 1.25),
    background_pal_plot = "#0a0a0a",
    background_pal_panel = "#232323",
    background_pal_key = NULL,
    line_linewidth = 0.125,
    line_pal = "#adadad",
    ticks_pal = NULL,
    ticks_linewidth = NULL,
    ticks_length = grid::unit(text_size / 4, "pt"),
    gridlines_linewidth = 0.1,
    gridlines_pal = "#474747") {

  if (is.null(ticks_pal)) ticks_pal <- line_pal
  if (is.null(background_pal_key)) background_pal_key <- background_pal_panel
  if (is.null(ticks_linewidth)) ticks_linewidth <- line_linewidth

  theme <- ggplot2::theme(

    plot.title = ggplot2::element_text(family = text_family, size = title_size, colour = title_pal, face = title_face, hjust = title_hjust, vjust = title_vjust, margin = title_margin),
    plot.title.position = title_position,
    plot.subtitle = ggplot2::element_text(family = text_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = subtitle_hjust, vjust = subtitle_vjust, margin = subtitle_margin),
    plot.caption = ggplot2::element_text(family = text_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
    plot.caption.position = caption_position,

    axis.title = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = text_size * 0.75)),
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 10)),
    axis.title.y.right = ggplot2::element_text(angle = 270, margin = ggplot2::margin(l = 10 - 1)),
    axis.text = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2)),
    axis.text.y = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(r = 2)),

    legend.title = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, hjust = 0, vjust = 0.5),
    legend.text = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, margin = ggplot2::margin(r = 7.5)),

    strip.text = ggplot2::element_text(family = text_family, size = text_size, colour = text_pal, face = text_face, hjust = 0.5, vjust = 0.5),
    strip.text.x = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(b = text_size * 0.5), angle = 0),
    strip.text.y = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(l = text_size * 0.75), angle = 270),

    axis.line = ggplot2::element_line(colour = line_pal, linewidth = line_linewidth),
    axis.ticks = ggplot2::element_line(colour = ticks_pal, linewidth = ticks_linewidth),
    axis.ticks.length = ticks_length,
    plot.background = ggplot2::element_rect(colour = background_pal_plot, fill = background_pal_plot),
    plot.margin = ggplot2::margin(t = 15, l = 10, b = 10, r = 20),
    panel.background = ggplot2::element_rect(colour = background_pal_panel, fill = background_pal_panel),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = gridlines_pal, linewidth = gridlines_linewidth),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(1.25, "lines"),
    legend.background = ggplot2::element_rect(colour = background_pal_plot, fill = background_pal_plot),
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(t = -2.5),
    legend.direction = "vertical",
    legend.justification = "left",
    legend.key = ggplot2::element_rect(colour = background_pal_key, fill = background_pal_key),
    legend.key.height = grid::unit(5, "mm"),
    legend.key.width = grid::unit(5, "mm"),
    legend.margin = ggplot2::margin(),
    legend.position = "right",
    legend.spacing.y = grid::unit(0.15, "cm"),
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",

    complete = TRUE
  )

  return(theme)
}

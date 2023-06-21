#' @title Quick theme for a ggplot
#'
#' @description Quick theme for a ggplot visualisation.
#' @param base_size The base size of the text. Defaults to 10.
#' @param base_family The base font family of the text. Defaults to "".
#' @param title_size The size of the title. Defaults to base_size * 1.1.
#' @param title_pal The colour of the title. Defaults to "#d5dfea".
#' @param title_face The font face of the title. Defaults to "bold".
#' @param title_position The horizontal alignment of the title (and subtitle) to either "plot" or "panel".
#' @param title_hjust The horizontal adjustment of the title.
#' @param title_vjust The vertical adjustment of the title.
#' @param title_margin The margin of the title. A ggplot2::margin function.
#' @param subtitle_size The size of the subtitle. Defaults to the base_size.
#' @param subtitle_pal The colour of the subtitle. Defaults to "#bbccdd".
#' @param subtitle_face The font face of the subtitle. Defaults to "plain".
#' @param subtitle_hjust The horizontal adjustment of the subtitle.
#' @param subtitle_vjust The vertical adjustment of the subtitle.
#' @param subtitle_margin The margin of the subtitle. A ggplot2::margin function.
#' @param caption_size The size of the caption. Defaults to base_size * 0.9.
#' @param caption_pal The colour of the caption. Defaults to scales::alpha("#d5dfea", 0.5).
#' @param caption_face The font face of the caption. Defaults to "plain".
#' @param caption_position The horizontal alignment of the caption to either "plot" or "panel".
#' @param caption_hjust The horizontal adjustment of the caption.
#' @param caption_vjust The vertical adjustment of the caption.
#' @param caption_margin The margin of the caption. A ggplot2::margin function.
#' @param other_text_size The size of all text other than the title, subtitle and caption. Defaults to the base_size.
#' @param other_text_pal The colour of all text other than the title, subtitle or caption. Defaults to "#bbccdd".
#' @param other_text_face The font face of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param background_pal_plot The colour of the plot background colour. Defaults to "#121b24".
#' @param background_pal_panel The colour of the panel background colour. Defaults to "#1f2f3e".
#' @param background_pal_key The colour of the legend key. Defaults to the background_pal_panel.
#' @param line_linewidth The linewidth of the axis line. Defaults to the other_text_size / 80 (i.e. 0.125).
#' @param line_pal The colour of the axis line. Defaults to "#bbccdd".
#' @param ticks_linewidth The linewidth of the axis ticks. Defaults to the line_linewidth.
#' @param ticks_length The length of the ticks. A grid::unit function.
#' @param ticks_pal The colour of the ticks. Defaults to the line_pal.
#' @param gridlines_linewidth The linewidth of the vertical major gridlines. Defaults to other_text_size / 100 (i.e. 0.1).
#' @param gridlines_pal The colour of the vertical major gridlines. Defaults to "#395673".
#'
#' @return A ggplot theme.
#' @export
#'
gg_theme_dark3 <- function(
    base_size = 10,
    base_family = "",
    title_size = base_size * 1.1,
    title_pal = "#d5dfea",
    title_face = "bold",
    title_position = "plot",
    title_hjust = 0,
    title_vjust = 1,
    title_margin = ggplot2::margin(((title_size ^ 0.5) * -0.5) - 3.85, b = title_size * 1.75),
    subtitle_size = other_text_size,
    subtitle_pal = "#bbccdd",
    subtitle_face = "plain",
    subtitle_hjust = 0,
    subtitle_vjust = 1,
    subtitle_margin = ggplot2::margin(t = subtitle_size * -1.5, b = subtitle_size * 1.75),
    caption_size = other_text_size * 0.9,
    caption_pal = scales::alpha("#d5dfea", 0.5),
    caption_face = "plain",
    caption_position = "plot",
    caption_hjust = 0,
    caption_vjust = 0,
    caption_margin = ggplot2::margin(t = caption_size * 1.25),
    other_text_size = base_size,
    other_text_pal = "#bbccdd",
    other_text_face = "plain",
    background_pal_plot = "#121b24",
    background_pal_panel = "#1f2f3e",
    background_pal_key = NULL,
    line_linewidth = other_text_size / 80,
    line_pal = "#bbccdd",
    ticks_pal = NULL,
    ticks_linewidth = NULL,
    ticks_length = grid::unit(other_text_size / 4, "pt"),
    gridlines_linewidth = other_text_size / 100,
    gridlines_pal = "#395673") {

  if (is.null(ticks_pal)) ticks_pal <- line_pal
  if (is.null(background_pal_key)) background_pal_key <- background_pal_panel
  if (is.null(ticks_linewidth)) ticks_linewidth <- line_linewidth

  theme <- ggplot2::theme(

    plot.title = ggplot2::element_text(family = base_family, size = title_size, colour = title_pal, face = title_face, hjust = title_hjust, vjust = title_vjust, margin = title_margin),
    plot.title.position = title_position,
    plot.subtitle = ggplot2::element_text(family = base_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = subtitle_hjust, vjust = subtitle_vjust, margin = subtitle_margin),
    plot.caption = ggplot2::element_text(family = base_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
    plot.caption.position = caption_position,

    axis.title = ggplot2::element_text(family = base_family, size = other_text_size, colour = other_text_pal, face = other_text_face),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = other_text_size * 0.75)),
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 10)),
    axis.title.y.right = ggplot2::element_text(angle = 270, margin = ggplot2::margin(l = 10 - 1)),
    axis.text = ggplot2::element_text(family = base_family, size = other_text_size, colour = other_text_pal, face = other_text_face),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2)),
    axis.text.y = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(r = 2)),

    legend.title = ggplot2::element_text(family = base_family, size = other_text_size, colour = other_text_pal, face = other_text_face, hjust = 0, vjust = 0.5),
    legend.text = ggplot2::element_text(family = base_family, size = other_text_size, colour = other_text_pal, face = other_text_face, margin = ggplot2::margin(r = 7.5)),

    strip.text = ggplot2::element_text(family = base_family, size = other_text_size, colour = other_text_pal, face = other_text_face, hjust = 0.5, vjust = 0.5),
    strip.text.x = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(b = other_text_size * 0.5), angle = 0),
    strip.text.y = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(l = other_text_size * 0.75), angle = 270),

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

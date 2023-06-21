#' @title Light theme for a ggplot
#'
#' @description Light theme for a ggplot visualisation.
#' @param text_size The base size of the text. Defaults to 10.
#' @param text_family The base font family of the text. Defaults to "".
#' @param title_size The size of the title. Defaults to text_size * 1.1.
#' @param title_pal The colour of the title. Defaults to "#05080a".
#' @param title_face The font face of the title. Defaults to "bold".
#' @param title_position The horizontal alignment of the title (and subtitle) to either "plot" or "panel".
#' @param title_hjust The horizontal adjustment of the title.
#' @param title_vjust The vertical adjustment of the title.
#' @param title_margin The margin of the title. A ggplot2::margin function.
#' @param subtitle_size The size of the subtitle. Defaults to the text_size.
#' @param subtitle_pal The colour of the subtitle. Defaults to "#121b24".
#' @param subtitle_face The font face of the subtitle. Defaults to "plain".
#' @param subtitle_hjust The horizontal adjustment of the subtitle.
#' @param subtitle_vjust The vertical adjustment of the subtitle.
#' @param subtitle_margin The margin of the subtitle. A ggplot2::margin function.
#' @param caption_size The size of the caption. Defaults to text_size * 0.9.
#' @param caption_pal The colour of the caption. scales::alpha("#05080a", 0.4).
#' @param caption_face The font face of the caption. Defaults to "plain".
#' @param caption_position The horizontal alignment of the caption to either "plot" or "panel".
#' @param caption_hjust The horizontal adjustment of the caption.
#' @param caption_vjust The vertical adjustment of the caption.
#' @param caption_margin The margin of the caption. A ggplot2::margin function.
#' @param other_text_size The size of all text other than the title, subtitle and caption. Defaults to the text_size.
#' @param other_text_pal The colour of all text other than the title, subtitle or caption. Defaults to "#121b24".
#' @param other_text_face The font face of all text other than the title, subtitle or caption. Defaults to "plain".
#' @param background_pal_plot The colour of the plot background colour. Defaults to "#f3f6f9".
#' @param background_pal_panel The colour of the panel background colour. Defaults to "#fcfdfe".
#' @param background_pal_key The colour of the legend key. Defaults to the background_pal_panel.
#' @param line_linewidth The linewidth of the axis line. Defaults to the other_text_size / 80 (i.e. 0.125).
#' @param line_pal The colour of the axis line. Defaults to "#121b24".
#' @param ticks_linewidth The linewidth of the axis ticks. Defaults to the line_linewidth.
#' @param ticks_length The length of the ticks. A grid::unit function.
#' @param ticks_pal The colour of the ticks. Defaults to the line_pal.
#' @param gridlines_linewidth The linewidth of the vertical major gridlines. Defaults to other_text_size / 100 (i.e. 0.1).
#' @param gridlines_pal The colour of the vertical major gridlines. Defaults to "#aec2d6".
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     title = "Penguins body mass by flipper length",
#'     subtitle = "Palmer Archipelago, Antarctica",
#'     caption = "Source: Gorman, 2020",
#'     theme = gg_theme_light()
#'   )
#'
gg_theme_light <- function(
    text_size = 10,
    text_family = "",
    title_size = text_size * 1.1,
    title_pal = "#05080a",
    title_face = "bold",
    title_position = "plot",
    title_hjust = 0,
    title_vjust = 1,
    title_margin = ggplot2::margin(t = ((title_size ^ 0.5) * -0.5) - 3.85, b = title_size * 1.75),
    subtitle_size = text_size,
    subtitle_pal = "#121b24",
    subtitle_face = "plain",
    subtitle_hjust = 0,
    subtitle_vjust = 1,
    subtitle_margin = ggplot2::margin(t = subtitle_size * -1.5, b = subtitle_size * 1.75),
    caption_size = text_size * 0.9,
    caption_pal = scales::alpha("#05080a", 0.4),
    caption_face = "plain",
    caption_position = "plot",
    caption_hjust = 0,
    caption_vjust = 0,
    caption_margin = ggplot2::margin(t = caption_size * 1.25),
    other_text_size = text_size,
    other_text_pal = "#121b24",
    other_text_face = "plain",
    background_pal_plot = "#f3f6f9",
    background_pal_panel = "#fcfdfe",
    background_pal_key = NULL,
    line_linewidth = other_text_size / 80,
    line_pal = "#121b24",
    ticks_pal = NULL,
    ticks_linewidth = NULL,
    ticks_length = grid::unit(other_text_size / 4, "pt"),
    gridlines_linewidth = other_text_size / 100,
    gridlines_pal = "#aec2d6") {

  if (is.null(ticks_pal)) ticks_pal <- line_pal
  if (is.null(background_pal_key)) background_pal_key <- background_pal_plot
  if (is.null(ticks_linewidth)) ticks_linewidth <- line_linewidth

  theme <- ggplot2::theme(

    plot.title = ggplot2::element_text(family = text_family, size = title_size, colour = title_pal, face = title_face, hjust = title_hjust, vjust = title_vjust, margin = title_margin),
    plot.title.position = title_position,
    plot.subtitle = ggplot2::element_text(family = text_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = subtitle_hjust, vjust = subtitle_vjust, margin = subtitle_margin),
    plot.caption = ggplot2::element_text(family = text_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
    plot.caption.position = caption_position,

    axis.title = ggplot2::element_text(family = text_family, size = other_text_size, colour = other_text_pal, face = other_text_face),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = other_text_size * 0.75)),
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 10)),
    axis.title.y.right = ggplot2::element_text(angle = 270, margin = ggplot2::margin(l = 10 - 1)),
    axis.text = ggplot2::element_text(family = text_family, size = other_text_size, colour = other_text_pal, face = other_text_face),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2)),
    axis.text.y = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(r = 2)),

    legend.title = ggplot2::element_text(family = text_family, size = other_text_size, colour = other_text_pal, face = other_text_face, hjust = 0, vjust = 0.5),
    legend.text = ggplot2::element_text(family = text_family, size = other_text_size, colour = other_text_pal, face = other_text_face, margin = ggplot2::margin(r = 7.5)),

    strip.text = ggplot2::element_text(family = text_family, size = other_text_size, colour = other_text_pal, face = other_text_face, hjust = 0.5, vjust = 0.5),
    strip.text.x = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(b = other_text_size * 0.5), angle = 0),
    strip.text.y = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(l = other_text_size * 0.75), angle = 270),

    axis.line = ggplot2::element_line(colour = line_pal, linewidth = line_linewidth),
    axis.ticks = ggplot2::element_line(colour = ticks_pal, linewidth = ticks_linewidth),
    axis.ticks.length = ticks_length,
    plot.background = ggplot2::element_rect(colour = background_pal_plot, fill = background_pal_plot),
    plot.margin = ggplot2::margin(t = 15, r = 20, b = 10, l = 10),
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

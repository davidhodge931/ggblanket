#' @title Dark theme for a ggplot
#'
#' @description Dark theme for a ggplot visualisation.
#' @param base_size The base size of the text. Defaults to 10.
#' @param base_family The base family of the text. Defaults to "".
#' @param base_face The base face of the text. Defaults to "plain".
#' @param base_pal The base colour of the text. Defaults to "#bbccdd".
#' @param title_family The font family of the title. Defaults to the base_family.
#' @param title_face The font face of the title. Defaults to "bold".
#' @param title_pal The colour of the title. Defaults to the base_pal first element.
#' @param title_size The size of the title. Defaults to the base_size * 1.1.
#' @param title_vjust The vertical adjustment of the title. Defaults to 0.5.
#' @param title_margin The margin of the title. A ggplot2::margin function.
#' @param subtitle_family The font family of the subtitle. Defaults to the base_family.
#' @param subtitle_face The font face of the subtitle. Defaults to the base_face.
#' @param subtitle_pal The colour of the subtitle. Defaults to the base_pal first element.
#' @param subtitle_size The size of the subtitle. Defaults to the base_size.
#' @param subtitle_vjust The vertical adjustment of the subtitle. Defaults to 0.5.
#' @param subtitle_margin The margin of the subtitle. A ggplot2::margin function.
#' @param caption_family The font family of the caption. Defaults to the base_family.
#' @param caption_face The font face of the caption. Defaults to the base_face.
#' @param caption_size The size of the caption. Defaults to the base_size * 0.9.
#' @param caption_alpha The alpha of the caption pal. Defaults to 0.33. Use 1 for no alpha.
#' @param caption_pal The colour of the caption (before caption_alpha is applied). Defaults to the base_pal first element.
#' @param caption_hjust The horizontal adjustment of the caption. Defaults to 0.
#' @param caption_vjust The vertical adjustment of the caption. Defaults to 0.5.
#' @param caption_margin The margin of the caption. A ggplot2::margin function.
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' #set for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = dark_mode()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(dark_mode())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
dark_mode <- function (
    base_size = 10,
    base_family = "",
    base_face = "plain",
    base_pal = "#bbccdd",
    title_family = NULL,
    title_face = "bold",
    title_pal = NULL,
    title_size = ggplot2::rel(1.1),
    title_vjust = 0.5,
    title_margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0),
    subtitle_family = NULL,
    subtitle_face = NULL,
    subtitle_pal = NULL,
    subtitle_size = NULL,
    subtitle_vjust = 0.5,
    subtitle_margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0),
    caption_family = NULL,
    caption_face = NULL,
    caption_alpha = 0.33,
    caption_pal = base_pal,
    caption_size = ggplot2::rel(0.9),
    caption_hjust = 0,
    caption_vjust = 0.5,
    # caption_margin = ggplot2::margin(t = base_size * 1, r = 0, b = 0, l = 0),
    caption_margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = 0, l = 0)
  ) {

  ggplot2::theme(
    line = ggplot2::element_line(colour = base_pal, linewidth = 10/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = pal_dark_mode[2], colour = pal_dark_mode[2], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = base_face, colour = base_pal, size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = base_size * 0.2), vjust = 0),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(10/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(10/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.75, r = 0, b = base_size * 0.25, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 1), angle = -90),
    legend.spacing = grid::unit(base_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(),
    legend.key = ggplot2::element_rect(fill = pal_dark_mode[2], colour = pal_dark_mode[2]),
    # legend.key.size = grid::unit(1.2, "lines"),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 5)),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "left",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0),
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = pal_dark_mode[3], colour = pal_dark_mode[3]),
    panel.border = ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major = ggplot2::element_line(colour = pal_dark_mode[4], linewidth = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = NA, colour = NA),
    strip.clip = "inherit",
    # strip.text = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.5, l = 0)),
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.25, r = 0, b = base_size * 0.5, l = 0)),
    strip.text.x = NULL,
    # strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = 0, l = 0)),
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(family = title_family, face = title_face, colour = title_pal, size = title_size, hjust = 0, vjust = title_vjust, margin = title_margin),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(family = subtitle_family, face = subtitle_face, colour = subtitle_pal, size = subtitle_size, hjust = 0, vjust = subtitle_vjust, margin = subtitle_margin),
    plot.caption = ggplot2::element_text(family = caption_family, face = caption_face, colour = scales::alpha(caption_pal, caption_alpha), size = caption_size, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    # plot.margin = ggplot2::margin(t = base_size * 1.75, r = base_size * 1.5, b = base_size * 0.75, l = base_size * 0.75),
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 1.5, b = base_size * 0.75, l = base_size * 0.75),

    complete = TRUE
  )
}

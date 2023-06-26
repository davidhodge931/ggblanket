#' #' @title Light theme for a ggplot
#' #'
#' #' @description Light theme for a ggplot visualisation.
#' #' @param base_size The base size of the text. Defaults to 10.
#' #' @param base_family The base family of the text. Defaults to "".
#' #' @param base_pal The base colour of the text. Defaults to "#121b24".
#' #' @param title_size The size of the title. Defaults to base_size * 1.1.
#' #' @param title_family The font family of the title. Defaults to the base_family.
#' #' @param title_pal The colour of the title. Defaults to 0.08 darker than the base_pal.
#' #' @param title_face The font face of the title. Defaults to "bold".
#' #' @param title_position The horizontal alignment of the title (and subtitle) to either "plot" or "panel".
#' #' @param title_hjust The horizontal adjustment of the title.
#' #' @param title_vjust The vertical adjustment of the title.
#' #' @param title_margin The margin of the title. A ggplot2::margin function.
#' #' @param subtitle_size The size of the subtitle. Defaults to the base_size.
#' #' @param subtitle_family The font family of the subtitle. Defaults to the base_family.
#' #' @param subtitle_pal The colour of the subtitle. Defaults to the base_pal.
#' #' @param subtitle_face The font face of the subtitle. Defaults to "plain".
#' #' @param subtitle_hjust The horizontal adjustment of the subtitle.
#' #' @param subtitle_vjust The vertical adjustment of the subtitle.
#' #' @param subtitle_margin The margin of the subtitle. A ggplot2::margin function.
#' #' @param caption_size The size of the caption. Defaults to base_size * 0.9.
#' #' @param caption_family The font family of the caption. Defaults to the base_family.
#' #' @param caption_pal The colour of the caption. Defaults to the base_pal with an alpha of 0.5.
#' #' @param caption_face The font face of the caption. Defaults to "plain".
#' #' @param caption_position The horizontal alignment of the caption to either "plot" or "panel".
#' #' @param caption_hjust The horizontal adjustment of the caption.
#' #' @param caption_vjust The vertical adjustment of the caption.
#' #' @param caption_margin The margin of the caption. A ggplot2::margin function.
#' #'
#' #' @return A ggplot theme.
#' #' @export
#' #'
#' #' @examples
#' #' library(palmerpenguins)
#' #'
#' #' penguins |>
#' #'   gg_point(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = species,
#' #'     title = "Penguins body mass by flipper length",
#' #'     subtitle = "Palmer Archipelago, Antarctica",
#' #'     caption = "Source: Gorman, 2020",
#' #'     theme = gg_theme_light()
#' #'   )
#' #'
#' gg_theme_light <- function(
#'     base_size = 10,
#'     base_family = "",
#'     base_pal = "#121b24",
#'     title_size = base_size * 1.1,
#'     title_family = base_family,
#'     title_pal = colorspace::darken(base_pal, 0.08, method = "absolute"),
#'     title_face = "bold",
#'     title_position = "plot",
#'     title_hjust = 0,
#'     title_vjust = 1,
#'     title_margin = ggplot2::margin(t = ((title_size ^ 0.5) * -0.5) - 3.85, b = title_size * 1.75),
#'     subtitle_size = base_size,
#'     subtitle_family = base_family,
#'     subtitle_pal = base_pal,
#'     subtitle_face = "plain",
#'     subtitle_hjust = 0,
#'     subtitle_vjust = 1,
#'     subtitle_margin = ggplot2::margin(t = subtitle_size * -1.5, b = subtitle_size * 1.75),
#'     caption_size = base_size * 0.9,
#'     caption_family = base_family,
#'     caption_pal = "#71767C", #colorspace::lighten("#121b24", 0.4, method = "absolute")
#'     caption_face = "plain",
#'     caption_position = "plot",
#'     caption_hjust = 0,
#'     caption_vjust = 0,
#'     caption_margin = ggplot2::margin(t = caption_size * 1.25)
#'     ) {
#'
#'   theme <- ggplot2::theme(
#'
#'     plot.title = ggplot2::element_text(family = title_family, size = title_size, colour = title_pal, face = title_face, hjust = title_hjust, vjust = title_vjust, margin = title_margin),
#'     plot.title.position = title_position,
#'     plot.subtitle = ggplot2::element_text(family = subtitle_family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = subtitle_hjust, vjust = subtitle_vjust, margin = subtitle_margin),
#'     plot.caption = ggplot2::element_text(family = caption_family, size = caption_size, colour = caption_pal, face = caption_face, hjust = caption_hjust, vjust = caption_vjust, margin = caption_margin),
#'     plot.caption.position = caption_position,
#'
#'     axis.title = ggplot2::element_text(family = base_family, size = base_size, colour = base_pal, face = "plain"),
#'     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.75)),
#'     axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 10)),
#'     axis.title.y.right = ggplot2::element_text(angle = 270, margin = ggplot2::margin(l = 10 - 1)),
#'     axis.text = ggplot2::element_text(family = base_family, size = base_size, colour = base_pal, face = "plain"),
#'     axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2)),
#'     axis.text.y = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(r = 2)),
#'
#'     legend.title = ggplot2::element_text(family = base_family, size = base_size, colour = base_pal, face = "plain", hjust = 0, vjust = 0.5),
#'     legend.text = ggplot2::element_text(family = base_family, size = base_size, colour = base_pal, face = "plain", margin = ggplot2::margin(r = 7.5)),
#'
#'     strip.text = ggplot2::element_text(family = base_family, size = base_size, colour = base_pal, face = "plain", hjust = 0.5, vjust = 0.5),
#'     strip.text.x = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(b = base_size * 0.5), angle = 0),
#'     strip.text.y = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(l = base_size * 0.75), angle = 270),
#'
#'     axis.line = ggplot2::element_line(colour = "#121b24", linewidth = base_size / 33, lineend = "square"),
#'     axis.ticks = ggplot2::element_line(colour = "#121b24", linewidth = base_size / 33),
#'     axis.ticks.length.x = grid::unit(base_size / 3, "pt"),
#'     axis.ticks.length.y = grid::unit(base_size / 4, "pt"),
#'     plot.background = ggplot2::element_rect(colour = "#e6ecf2", fill = "#e6ecf2"),
#'     plot.margin = ggplot2::margin(t = 15, r = 20, b = 11, l = 10),
#'     panel.background = ggplot2::element_rect(colour = "#fcfdfe", fill = "#fcfdfe"),
#'     panel.border = ggplot2::element_blank(),
#'     panel.grid.major = ggplot2::element_line(colour = "#DBE1E7", linewidth = base_size / 100),
#'     panel.grid.minor = ggplot2::element_blank(),
#'     panel.spacing = grid::unit(1.25, "lines"),
#'     legend.background = ggplot2::element_rect(colour = "#e6ecf2", fill = "#e6ecf2"),
#'     legend.box = NULL,
#'     legend.box.margin = ggplot2::margin(t = -2.5),
#'     legend.direction = "vertical",
#'     legend.justification = "left",
#'     legend.key = ggplot2::element_rect(colour = "#e6ecf2", fill = "#e6ecf2"),
#'     legend.key.height = grid::unit(5, "mm"),
#'     legend.key.width = grid::unit(5, "mm"),
#'     legend.margin = ggplot2::margin(),
#'     legend.position = "right",
#'     legend.spacing.y = grid::unit(0.15, "cm"),
#'     strip.background = ggplot2::element_blank(),
#'     strip.placement = "outside",
#'
#'     complete = TRUE
#'   )
#'
#'   return(theme)
#' }

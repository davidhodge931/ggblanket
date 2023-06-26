#' @title Light theme for a ggplot
#'
#' @description Light theme for a ggplot visualisation.
#' @param base_size The base size of the text. Defaults to 10.
#' @param base_family The base family of the text. Defaults to "".
#' @param base_pal The base colour of the text. Defaults to "#121b24".
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
gg_theme_light <- function (
    base_size = 10,
    base_family = "",
    base_pal = "#121b24") {

  ggplot2::theme(
    line = ggplot2::element_line(colour = base_pal, linewidth = 10/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = "#e6ecf2", colour = "#e6ecf2", linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain",
                        colour = base_pal, size = base_size, lineheight = 0.9,
                        hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.x =  ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.2), vjust = 1),
    axis.text.x.top =  ggplot2::element_text(margin = ggplot2::margin(b = base_size * 0.2), vjust = 0),
    axis.text.y =  ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right =  ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks.length.x = grid::unit(10/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(10/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.x =  ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.75)),
    axis.title.x.top =  ggplot2::element_text(margin = ggplot2::margin(b = base_size * 0.75)),
    axis.title.y =  ggplot2::element_text(margin = ggplot2::margin(r = base_size * 1), angle = 90),
    axis.title.y.right =  ggplot2::element_text(margin = ggplot2::margin(l = base_size * 1), angle = -90),
    legend.spacing = grid::unit(base_size * 0.125, "pt"),
    legend.spacing.x = NULL,
    legend.margin = ggplot2::margin(),
    legend.key = NULL,
    legend.key.size = grid::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text =  ggplot2::element_text(),
    legend.text.align = NULL,
    legend.title =  ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 5)),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "left",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5),
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background =  ggplot2::element_rect(fill = "#fcfdfe", colour = "#fcfdfe"),
    panel.border =  ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major =  ggplot2::element_line(colour = "#DBE1E7", linewidth = ggplot2::rel(0.33)),
    panel.grid.minor =  ggplot2::element_blank(),
    panel.spacing = grid::unit(1.25, "lines"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = NULL,
    strip.clip = "inherit",
    strip.text = NULL,
    strip.text.x =  ggplot2::element_text(margin = ggplot2::margin(b = base_size * 0.5)),
    strip.text.x.bottom =  ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.5)),
    strip.text.y =  ggplot2::element_text(margin = ggplot2::margin(l = base_size * 2/3), angle = -90),
    strip.text.y.left =  ggplot2::element_text(margin = ggplot2::margin(r = base_size * 2/3), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background =  ggplot2::element_rect(),
    plot.title =  ggplot2::element_text(size = ggplot2::rel(1.1), face = "bold", hjust = 0, vjust = 1, margin = ggplot2::margin(t = base_size * -0.5, b = base_size * 1.5)),
    plot.title.position = "plot",
    plot.subtitle =  ggplot2::element_text(hjust = 0, vjust = 1, margin = ggplot2::margin(t = base_size * -1, b = base_size * 1.5)),
    plot.caption =  ggplot2::element_text(size = ggplot2::rel(0.9), colour = scales::alpha(base_pal, 0.33), hjust = 0, vjust = 1, margin = ggplot2::margin(t = base_size)),
    plot.caption.position = "plot",
    plot.tag =  ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 1.5, r = base_size * 1.75, b = base_size * 0.75, l = base_size),

    complete = TRUE
  )
}

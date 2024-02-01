#' @title Dark ggplot theme with right top legend
#'
#' @description Dark theme for a ggplot visualisation with legend at right top.
#'
#' @param base_size The base size of the text. Defaults to 11.
#' @param base_family The base family of the text. Defaults to "".
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
#'     theme = dark_mode_b()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(dark_mode_b())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
dark_mode_rt <- function (
    base_size = 11,
    base_family = "") {

  ggplot2::theme(
    line = ggplot2::element_line(colour = darkness[1], linewidth = base_size/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = darkness[3], colour = darkness[3], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = darkness[1], size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(base_size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(base_size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(base_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = darkness[3], fill = darkness[3]),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = ggplot2::element_line(colour = darkness[3]),
    legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = darkness[2], colour = darkness[2]),
    panel.border = ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major = ggplot2::element_line(colour = darkness[3], linewidth = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = lightness[3], colour = darkness[3]),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 1, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = darkness[4], size = ggplot2::rel(0.9), hjust = 0, margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),

    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "top",
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = base_size * 0.75),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 1), angle = -90),

    complete = TRUE
  )
}

#' @title Dark ggplot theme with right legend
#'
#' @description Dark theme for a ggplot visualisation with right centre legend.
#'
#' @inheritParams dark_mode_rt
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
#'     theme = dark_mode_r()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(dark_mode_r())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
dark_mode_r <- function (
    base_size = 11,
    base_family = "") {

  ggplot2::theme(
    line = ggplot2::element_line(colour = darkness[1], linewidth = base_size/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = darkness[3], colour = darkness[3], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = darkness[1], size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(base_size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(base_size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(base_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = darkness[3], fill = darkness[3]),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = ggplot2::element_line(colour = darkness[3]),
    legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = darkness[2], colour = darkness[2]),
    panel.border = ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major = ggplot2::element_line(colour = darkness[3], linewidth = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = lightness[3], colour = darkness[3]),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = darkness[4], size = ggplot2::rel(0.9), hjust = 0, margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),

    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "left",
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = base_size * 0.75),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 1), angle = -90),

    complete = TRUE
  )
}

#' @title Dark ggplot theme with bottom legend
#'
#' @description Dark theme for a ggplot visualisation with bottom legend.
#'
#' @inheritParams dark_mode_rt
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
#'     theme = dark_mode_b()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(dark_mode_b())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
dark_mode_b <- function (
    base_size = 11,
    base_family = "") {

  ggplot2::theme(
    line = ggplot2::element_line(colour = darkness[1], linewidth = base_size/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = darkness[3], colour = darkness[3], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = darkness[1], size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(base_size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(base_size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(base_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = darkness[3], fill = darkness[3]),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = ggplot2::element_line(colour = darkness[3]),
    legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = darkness[2], colour = darkness[2]),
    panel.border = ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major = ggplot2::element_line(colour = darkness[3], linewidth = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = lightness[3], colour = darkness[3]),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = darkness[4], size = ggplot2::rel(0.9), hjust = 0, margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),

    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box.margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = 0, l = 0),
    legend.margin = ggplot2::margin(r = base_size * 2, b = base_size * 0.5),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.5, r = base_size * 1.25, b = base_size * 0.5, l = base_size * 0.5)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90),

    complete = TRUE
  )
}

#' @title Dark ggplot theme with top legend
#'
#' @description Dark theme for a ggplot visualisation with top legend.
#'
#' @inheritParams dark_mode_rt
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
#'     theme = dark_mode_t()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(dark_mode_t())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
dark_mode_t <- function (
    base_size = 11,
    base_family = "") {

  ggplot2::theme(
    line = ggplot2::element_line(colour = darkness[1], linewidth = base_size/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = darkness[3], colour = darkness[3], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = darkness[1], size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(base_size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(base_size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(base_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = darkness[3], fill = darkness[3]),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = ggplot2::element_line(colour = darkness[3]),
    legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = darkness[2], colour = darkness[2]),
    panel.border = ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major = ggplot2::element_line(colour = darkness[3], linewidth = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = lightness[3], colour = darkness[3]),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = darkness[4], size = ggplot2::rel(0.9), hjust = 0, margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),

    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box.margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0),
    legend.margin = ggplot2::margin(t = base_size * -1.5, r = base_size * 2, b = base_size * 0.5, l = 0),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.5, r = base_size * 1.25, b = base_size * 0.5, l = base_size * 0.5)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * 0, r = 0, b = base_size * 0.2, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90),

    complete = TRUE
  )
}

#' @title Dark ggplot theme with inside legend
#'
#' @description Dark theme for a ggplot visualisation with legend inside the panel.
#'
#' @inheritParams dark_mode_rt
#' @param legend_position_inside The placement of legends inside panels. A numeric vector of length two.
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
#'     theme = dark_mode_i(legend_position_inside = c(0.15, 0.7))
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(dark_mode_i())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
dark_mode_i <- function (
    base_size = 11,
    base_family = "",
    legend_position_inside = c(0.5, 0.5)) {

  ggplot2::theme(
    line = ggplot2::element_line(colour = darkness[1], linewidth = base_size/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = darkness[3], colour = darkness[3], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = darkness[1], size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.2), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(base_size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(base_size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(base_size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = darkness[3], fill = darkness[3]),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = ggplot2::element_line(colour = darkness[3]),
    legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = darkness[2], colour = darkness[2]),
    panel.border = ggplot2::element_blank(),
    panel.grid = NULL,
    panel.grid.major = ggplot2::element_line(colour = darkness[3], linewidth = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = lightness[3], colour = darkness[3]),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = 0, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = base_size * -2, r = 0, b = base_size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = darkness[4], size = ggplot2::rel(0.9), hjust = 0, margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),

    legend.position = "inside",
    legend.position.inside = legend_position_inside,
    legend.direction = "vertical",
    legend.justification = NULL,
    legend.margin = ggplot2::margin(t = base_size * 0.66, r = base_size * 0.33, b = base_size * 0.66, l = base_size * 0.66),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 0.5)),
    legend.background = ggplot2::element_rect(colour = darkness[3], fill = darkness[3]),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.2, r = 0, b = base_size * 1, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.2, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90),

    complete = TRUE
  )
}

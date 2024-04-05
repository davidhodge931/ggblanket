#' Mode theme foundation
#'
#' @description Theme foundation for `*_mode_*` functions.
#'
#' @param size The base size of the text. Defaults to 11. The title adds 10%.
#' @param family The family of the text. Defaults to "".
#' @param col_palette A 5 colour vector ordered for text, line, panel.background, plot.background and panel.grid. Fill inherits from these.
#' @param linewidth_palette A 2 element numeric vector with values for the linewidth of line/rect and panel.grid elements.
#'
#' @return A ggplot theme.
#' @keywords internal
#'
design_mode <- function(
    size = 11,
    family = "",
    col_palette = NULL,
    linewidth_palette = NULL) {

  if (rlang::is_null(col_palette) | rlang::is_null(linewidth_palette)) {
    rlang::abort("col_palette and linewidth_palette vectors must not be NULL")
  }

  ggplot2::theme(
    line = ggplot2::element_line(colour = col_palette[2], linewidth = linewidth_palette[1], linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = col_palette[4], colour = col_palette[4], linewidth = linewidth_palette[1], linetype = 1),
    text = ggplot2::element_text(family = family, face = "plain", colour = col_palette[1], size = size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = size * 0.25), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = size * 0.25), hjust = 0),
    axis.ticks = NULL,
    axis.ticks.length.x = grid::unit(size/3, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(size/4, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = NULL,
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * 1, b = 0, l = 0), angle = 90),
    legend.spacing = grid::unit(size * 1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(colour = col_palette[4], fill = col_palette[4]),
    legend.key.size = grid::unit(size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = NULL,
    legend.ticks.length = grid::unit(size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = col_palette[3], colour = col_palette[3]),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = col_palette[5], linewidth = linewidth_palette[2]),
    panel.grid.major = NULL,
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = NA, colour = NA),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = size * 0.66, l = 0)),
    strip.text.x = NULL,
    strip.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = size * 0.25, r = 0, b = size * 1, l = 0)),
    strip.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = size * 2/3), angle = -90),
    strip.text.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * 2/3, b = 0, l = 0), angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(0.15, "pt"),
    strip.switch.pad.wrap = grid::unit(0.15, "pt"),
    plot.background = ggplot2::element_rect(),
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0, margin = ggplot2::margin(t = size * -1, r = 0, b = size * 2.5, l = 0)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = size * -2, r = 0, b = size * 2, l = 0)),
    plot.caption = ggplot2::element_text(colour = scales::alpha(col_palette[1], 0.75), size = ggplot2::rel(0.85), hjust = 0, margin = ggplot2::margin(t = size * 0.5, r = 0, b = size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = size * 2, r = size * 2, b = size * 0.25, l = size * 0.75),

    complete = TRUE
  )
}

#' Flexible mode with right legend
#'
#' @description Flexible mode with right legend and customisable colour and linewidth.
#'
#' @inheritParams design_mode
#'
#' @return A ggplot theme.
#' @keywords internal
#'
design_mode_r <- function (
    size = 11,
    family = "",
    col_palette = NULL,
    linewidth_palette = NULL) {

  if (rlang::is_null(col_palette) | rlang::is_null(linewidth_palette)) {
    rlang::abort("col_palette and linewidth_palette vectors must not be NULL")
  }

  design_mode(
    size = size,
    family = family,
    col_palette = col_palette,
    linewidth_palette = linewidth_palette
  ) +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = c(0, 1),
      legend.location = "panel",
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = size * 0.75, l = size * 0.75),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = size * 0.5, l = 0)),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = size * 0.5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = size * -0.33, r = 0, b = size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = size * -1, r = 0, b = size * 1, l = 0)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = size * 0.3, r = 0, b = size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = size * -0.5, r = 0, b = size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * 1, b = 0, l = size * 1), angle = -90)
    )
}

#' Flexible mode with top legend
#'
#' @description Flexible mode with legend at top and customisable colour and linewidth.
#'
#' @inheritParams design_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
design_mode_t <- function (
    size = 11,
    family = "",
    col_palette = NULL,
    linewidth_palette = NULL) {

  if (rlang::is_null(col_palette) | rlang::is_null(linewidth_palette)) {
    rlang::abort("col_palette and linewidth_palette vectors must not be NULL")
  }

  design_mode(
    size = size,
    family = family,
    col_palette = col_palette,
    linewidth_palette = linewidth_palette
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = c(0, 0.5),
    legend.location = "plot",
    legend.box.margin = ggplot2::margin(t = size * 0.5, r = 0, b = size * 0.5, l = 0),
    legend.margin = ggplot2::margin(t = size * -1.5, r = size * 2, b = size * 0.5, l = 0),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = size * 0.25, r = 0, b = size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * 1.25, b = 0, l = size * 0.5)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = size * 0.3, r = 0, b = size * 1, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = size * -0.33, r = 0, b = size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = size * 0.5, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = size * 0, r = 0, b = size * 0.3, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * -0.5, b = 0, l = size * 1), angle = -90)
  )
}

#' Flexible mode with bottom legend
#'
#' @description Flexible mode with legend at bottom and customisable colour and linewidth.
#'
#' @inheritParams design_mode_r
#'
#' @return A ggplot theme.
#' @keywords internal
#'
design_mode_b <- function (
    size = 11,
    family = "",
    col_palette = NULL,
    linewidth_palette = NULL) {

  if (rlang::is_null(col_palette) | rlang::is_null(linewidth_palette)) {
    rlang::abort("col_palette and linewidth_palette vectors must not be NULL")
  }

  design_mode(
    size = size,
    family = family,
    col_palette = col_palette,
    linewidth_palette = linewidth_palette
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.box.margin = ggplot2::margin(t = size * -0.5, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(r = size * 2, b = size * 0.5),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = size * 0.25, r = 0, b = size * 0.5, l = 0)),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * 1.25, b = 0, l = size * 0.5)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = size * 0.3, r = 0, b = size * 1, l = 0)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = size * -0.33, r = 0, b = size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = size * -1, r = 0, b = size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = size * -0.5, r = 0, b = size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * -0.5, b = 0, l = size * 1), angle = -90)
    )
}

#' Flexible mode with no legend
#'
#' @description Flexible mode with no legend and customisable colour and linewidth.
#'
#' @inheritParams design_mode
#'
#' @return A ggplot theme.
#' @keywords internal
#'
design_mode_n <- function (
    size = 11,
    family = "",
    col_palette = NULL,
    linewidth_palette = NULL) {

  if (rlang::is_null(col_palette) | rlang::is_null(linewidth_palette)) {
    rlang::abort("col_palette and linewidth_palette vectors must not be NULL")
  }

  mode <- design_mode(
    size = size,
    family = family,
    col_palette = col_palette,
    linewidth_palette = linewidth_palette
  ) +
  ggplot2::theme(
    legend.position = "none",
    legend.direction = "vertical",
    legend.justification = c(0, 1),
    legend.location = "panel",
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = size * 0.75, l = size * 0.75),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = size * 0.5)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = size * -0.33, r = 0, b = size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = size * -1, r = 0, b = size * 1, l = 0)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = size * 0.3, r = 0, b = size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = size * -0.5, r = 0, b = size * 0.3, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = size * 1, b = 0, l = size * 1), angle = -90)
  )

  return(mode)
}

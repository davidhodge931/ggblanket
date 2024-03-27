#' Theme base for `base_mode_*` functions
#'
#' @description Theme base for `base_mode_*` functions.
#'
#' @param base_size The base size of the text. Defaults to 11.
#' @param base_family The base family of the text. Defaults to "".
#' @param base_pal A 5 colour vector with elements named "text", "line", "panel", "plot" and "grid".
#'
#' @keywords internal
base_mode <- function(
    base_size = 11,
    base_family = "",
    base_pal = c(
      "text" = "white",
      "line" = "white",
      "panel" = "white",
      "plot" = "white",
      "grid" = "white")
    ) {

  ggplot2::theme(
    line = ggplot2::element_line(colour = base_pal["line"], linewidth = base_size/33, linetype = 1, lineend = "square"),
    rect = ggplot2::element_rect(fill = base_pal["plot"], colour = base_pal["plot"], linewidth = base_size/33, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = base_pal["text"], size = base_size,
                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.25), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = base_size * 0.25), hjust = 0),
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
    legend.key = ggplot2::element_rect(colour = base_pal["plot"], fill = base_pal["plot"]),
    legend.key.size = grid::unit(base_size * 1.75, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = grid::unit(base_size * 0.33, "pt"),
    legend.key.spacing.y = grid::unit(base_size * 0.33, "pt"),
    legend.byrow = FALSE,
    legend.frame = NULL,
    legend.axis.line = NULL,
    legend.ticks = NULL,
    legend.ticks.length = grid::unit(base_size / 3.25, "pt"),
    legend.title.position = "top",
    legend.box = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    panel.background = ggplot2::element_rect(fill = base_pal["panel"], colour = base_pal["panel"]),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = base_pal["grid"], linewidth = ggplot2::rel(4)),
    panel.grid.major = NULL,
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(base_size * 2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = NA, colour = NA),
    strip.clip = "inherit",
    strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.66, l = 0)),
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
    plot.caption = ggplot2::element_text(colour = scales::alpha(base_pal["text"], 0.75), size = ggplot2::rel(0.85), hjust = 0, margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0)),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = base_size * 2, r = base_size * 2, b = base_size * 0.25, l = base_size * 0.75),

    complete = TRUE
  )
}

#' Theme base for `*_mode_r`
#'
#' @description Theme base for `*_mode_r` functions with right legend.
#'
#' @inheritParams base_mode
#'
#' @keywords internal
base_mode_r <- function (
    base_size = 11,
    base_family = "",
    base_pal = c(
      "text" = "white",
      "line" = "white",
      "panel" = "white",
      "plot" = "white",
      "grid" = "white")
    ) {

  base_mode(
    base_size = base_size,
    base_family = base_family,
    base_pal = base_pal
  ) +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = c(0, 1),
      legend.location = "panel",
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = base_size * 0.75),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.3, r = 0, b = base_size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 1), angle = -90)
    )
}

#' Theme base for `*_mode_t`
#'
#' @description Theme base for `*_mode_t` functions with top legend.
#'
#' @inheritParams base_mode_r
#'
#' @keywords internal
base_mode_t <- function (
    base_size = 11,
    base_family = "",
    base_pal = c(
      "text" = "white",
      "line" = "white",
      "panel" = "white",
      "plot" = "white",
      "grid" = "white")
    ) {

  base_mode(
    base_size = base_size,
    base_family = base_family,
    base_pal = base_pal
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = c(0, 0.5),
    legend.location = "plot",
    legend.box.margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = base_size * 0.5, l = 0),
    legend.margin = ggplot2::margin(t = base_size * -1.5, r = base_size * 2, b = base_size * 0.5, l = 0),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1.25, b = 0, l = base_size * 0.5)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.3, r = 0, b = base_size * 1, l = 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * 0, r = 0, b = base_size * 0.3, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90)
  )
}

#' Theme base for `*_mode_b`
#'
#' @description Theme base for `*_mode_b` functions with bottom legend.
#'
#' @inheritParams base_mode_r
#'
#' @keywords internal
base_mode_b <- function (
    base_size = 11,
    base_family = "",
    base_pal = c(
      "text" = "white",
      "line" = "white",
      "panel" = "white",
      "plot" = "white",
      "grid" = "white")
    ) {

  base_mode(
    base_size = base_size,
    base_family = base_family,
    base_pal = base_pal
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.box.margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(r = base_size * 2, b = base_size * 0.5),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25, r = 0, b = base_size * 0.5, l = 0)),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1.25, b = 0, l = base_size * 0.5)),
      axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.3, r = 0, b = base_size * 1, l = 0)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
      axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.3, l = 0)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * -0.5, b = 0, l = base_size * 1), angle = -90)
    )
}

#' Theme base for `*_mode_n`
#'
#' @description Theme base for `*_mode_n` functions with no legend.
#'
#' @inheritParams base_mode
#'
#' @keywords internal
base_mode_n <- function (
    base_size = 11,
    base_family = "",
    base_pal = c(
      "text" = "white",
      "line" = "white",
      "panel" = "white",
      "plot" = "white",
      "grid" = "white")
    ) {

  mode <- base_mode(
    base_size = base_size,
    base_family = base_family,
    base_pal = base_pal
  ) +
  ggplot2::theme(
    legend.position = "none",
    legend.direction = "vertical",
    legend.justification = c(0, 1),
    legend.location = "panel",
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.75, l = base_size * 0.75),
    legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = base_size * 0.5)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -0.33, r = 0, b = base_size * 0.75, l = 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = base_size * -1, r = 0, b = base_size * 1, l = 0)),
    axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = base_size * 0.3, r = 0, b = base_size * 1, l = 0)),
    axis.text.x.top = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(t = base_size * -0.5, r = 0, b = base_size * 0.3, l = 0)),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = base_size * 1, b = 0, l = base_size * 1), angle = -90)
  )

  return(mode)
}

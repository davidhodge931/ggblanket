#' Annotate text
#'
#' @description Create annotated text labels.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' @param ... Arguments passed to `ggplot2::annotate("text", ....)`. Require named arguments (and support trailing commas).
#' @param x,y Position of the text. Use `I()` for normalized coordinates (0-1).
#' @param label The text to display.
#' @param colour The colour of the text. Inherits from the current theme `axis.text` etc.
#' @param size The size of the text. Inherits from the current theme `axis.text` etc.
#' @param family The font family of the text. Inherits from the current theme `axis.text` etc.
#' @param hjust,vjust Horizontal and vertical justification. Defaults to `0.5`.
#' @param angle Text rotation angle. Defaults to `0`.
#'
#' @return A list containing annotation layers.
#' @export
annotate_text <- function(
    ...,
    x = NULL,
    y = NULL,
    label = NULL,
    colour = NULL,
    size = NULL,
    family = NULL,
    hjust = 0.5,
    vjust = 0.5,
    angle = 0
) {
  # Validate required arguments
  if (rlang::is_null(x) || rlang::is_null(y) || rlang::is_null(label)) {
    rlang::abort("x, y, and label must all be specified")
  }

  # Get current theme and resolve text properties
  current_theme <- ggplot2::theme_get()
  text_hierarchy <- c("axis.text.x", "axis.text")

  resolved_text_element <- NULL
  for (element_name in text_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!rlang::is_null(element) && !inherits(element, "element_blank")) {
      resolved_text_element <- element
      break
    }
  }

  if (rlang::is_null(resolved_text_element)) {
    resolved_text_element <- list(colour = "black", size = 11, family = "")
  }

  # Extract theme properties
  text_colour <- colour %||% resolved_text_element$colour %||% "black"
  text_size <- size %||% resolved_text_element$size %||% 11
  text_family <- family %||% resolved_text_element$family %||% ""

  list(
    ggplot2::annotate(
      "text",
      x = x,
      y = y,
      label = label,
      colour = text_colour,
      size = text_size / 2.845276,  # Convert to ggplot2 size units
      family = text_family,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      ...
    )
  )
}

#' Annotate segment
#'
#' @description Create annotated line segments.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)`. Require named arguments (and support trailing commas).
#' @param x,y,xend,yend Start and end positions of the segment. Use `I()` for normalized coordinates (0-1).
#' @param colour The colour of the segment. Inherits from the current theme `axis.line` etc.
#' @param linewidth The linewidth of the segment. Inherits from the current theme `axis.line` etc.
#' @param linetype The linetype of the segment. Inherits from the current theme `axis.line` etc.
#'
#' @return A list containing annotation layers.
#' @export
annotate_segment <- function(
    ...,
    x = NULL,
    y = NULL,
    xend = NULL,
    yend = NULL,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL
) {
  # Validate required arguments
  if (rlang::is_null(x) || rlang::is_null(y) || rlang::is_null(xend) || rlang::is_null(yend)) {
    rlang::abort("x, y, xend, and yend must all be specified")
  }

  # Get current theme and resolve line properties
  current_theme <- ggplot2::theme_get()
  line_hierarchy <- c("axis.line.x", "axis.line")

  resolved_line_element <- NULL
  for (element_name in line_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!rlang::is_null(element) && !inherits(element, "element_blank")) {
      resolved_line_element <- element
      break
    }
  }

  if (rlang::is_null(resolved_line_element)) {
    resolved_line_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  # Extract theme properties
  line_colour <- colour %||% resolved_line_element$colour %||% "black"
  line_linewidth <- linewidth %||% resolved_line_element$linewidth %||% 0.5
  line_linetype <- linetype %||% resolved_line_element$linetype %||% 1

  list(
    ggplot2::annotate(
      "segment",
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      colour = line_colour,
      linewidth = line_linewidth,
      linetype = line_linetype,
      ...
    )
  )
}

#' Annotate curve
#'
#' @description Create annotated curved lines.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' @param ... Arguments passed to `ggplot2::annotate("curve", ....)`. Require named arguments (and support trailing commas).
#' @param x,y,xend,yend Start and end positions of the curve. Use `I()` for normalized coordinates (0-1).
#' @param curvature A numeric value giving the amount of curvature. Defaults to `0.5`.
#' @param angle A numeric value between 0 and 180, giving an amount to skew the control points of the curve. Defaults to `90`.
#' @param ncp The number of control points used to draw the curve. Defaults to `5`.
#' @param colour The colour of the curve. Inherits from the current theme `axis.line` etc.
#' @param linewidth The linewidth of the curve. Inherits from the current theme `axis.line` etc.
#' @param linetype The linetype of the curve. Inherits from the current theme `axis.line` etc.
#'
#' @return A list containing annotation layers.
#' @export
annotate_curve <- function(
    ...,
    x = NULL,
    y = NULL,
    xend = NULL,
    yend = NULL,
    curvature = 0.5,
    angle = 90,
    ncp = 5,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL
) {
  # Validate required arguments
  if (rlang::is_null(x) || rlang::is_null(y) || rlang::is_null(xend) || rlang::is_null(yend)) {
    rlang::abort("x, y, xend, and yend must all be specified")
  }

  # Get current theme and resolve line properties
  current_theme <- ggplot2::theme_get()
  line_hierarchy <- c("axis.line.x", "axis.line")

  resolved_line_element <- NULL
  for (element_name in line_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!rlang::is_null(element) && !inherits(element, "element_blank")) {
      resolved_line_element <- element
      break
    }
  }

  if (rlang::is_null(resolved_line_element)) {
    resolved_line_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  # Extract theme properties
  line_colour <- colour %||% resolved_line_element$colour %||% "black"
  line_linewidth <- linewidth %||% resolved_line_element$linewidth %||% 0.5
  line_linetype <- linetype %||% resolved_line_element$linetype %||% 1

  list(
    ggplot2::annotate(
      "curve",
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      colour = line_colour,
      linewidth = line_linewidth,
      linetype = line_linetype,
      ...
    )
  )
}


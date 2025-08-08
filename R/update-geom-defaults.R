#' Update the colour/fill
#'
#' @description
#' Updates the active theme for colour/fill styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param col colour/fill base for most geoms.
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_col <- function(
  ...,
  col = ifelse(is_panel_light(), "#4797C3FF", "#357BA2FF")
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(colour = col, fill = col))

  font_geoms <- c("text", "label")
  reference_geoms <- c("hline", "vline")
  all_geoms <- c(
    "abline",
    "contour",
    "count",
    "curve",
    "dotplot",
    "density2d",
    "errorbar",
    "freqpoly",
    "function",
    "jitter",
    "line",
    "linerange",
    "path",
    "point",
    "pointrange",
    "qq",
    "quantile",
    "rug",
    "segment",
    "smooth",
    "spoke",
    "step",
    "area",
    "bar",
    "boxplot",
    "col",
    "density",
    "map",
    "polygon",
    "rect",
    "ribbon",
    "tile",
    "violin",
    "crossbar",
    "bin2d",
    "hex",
    "raster",
    "contour_filled",
    "density2d_filled",
    "text",
    "label",
    "hline",
    "vline"
  )

  selected_geoms <- setdiff(all_geoms, c(font_geoms, reference_geoms))

  # Build named list of theme elements
  theme_args <- list()

  # Apply colour and fill to all geoms
  for (geom in selected_geoms) {
    geom_name <- paste0("geom.", gsub("_", "", geom))

    theme_args[[geom_name]] <- ggplot2::element_geom(
      colour = col,
      fill = col
    )
  }
}

#' Update the shape
#'
#' @description
#' Updates the active theme for shape styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param shape A shape.
#'
#' @return Updated geom defaults for shape
#'
#' @export
update_geom_shape <- function(
  ...,
  shape = 21
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(pointshape = shape))
}

#' Update the linetype
#'
#' @description
#' Updates the active theme for linetype styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param linetype linetype.
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linetype <- function(
  ...,
  linetype = 1
) {
  ggplot2::update_theme(
    geom = ggplot2::element_geom(linetype = linetype, bordertype = linetype)
  )
}

#' Update the linewidth
#'
#' @description
#' Updates the active theme for linewidth styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param linewidth linewidth.
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linewidth <- function(
  ...,
  linewidth = 0.5
) {
  ggplot2::update_theme(
    geom.abline = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.blank = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.contour = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.curve = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.density2d = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.dotplot = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.errorbar = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.errorbarh = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.function = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.hline = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.label = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.line = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.linerange = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.path = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.quantile = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.rug = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.segment = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.smooth = ggplot2::element_geom(
      linewidth = linewidth / 2,
      borderwidth = linewidth / 2
    ),
    geom.spoke = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.step = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.text = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    ),
    geom.vline = ggplot2::element_geom(
      linewidth = linewidth,
      borderwidth = linewidth
    )
  )
}

#' Update the size
#'
#' @description
#' Updates the active theme for size styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param size A size for point geoms.
#'
#' @return Updated geom defaults for size
#'
#' @export
update_geom_size <- function(
  ...,
  size = 1.5
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(pointsize = size))
}

#' Update the stroke
#'
#' @description
#' Updates the active theme for stroke styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param stroke A stroke for point geoms.
#'
#' @return Updated geom defaults for stroke
#'
#' @export
update_geom_stroke <- function(
  ...,
  stroke = 0.5
) {
  ggplot2::update_theme(
    geom.point = ggplot2::element_geom(borderwidth = stroke),
    geom.jitter = ggplot2::element_geom(borderwidth = stroke),
    geom.count = ggplot2::element_geom(borderwidth = stroke),
    geom.qq = ggplot2::element_geom(borderwidth = stroke)
  )
}

#' Update the geom text/label
#'
#' @description
#' Updates the active theme to apply consistent size styling.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param colour A colour.
#' @param fill A fill.
#' @param size A size.
#' @param family A family.
#'
#' @return Updated geom defaults for size
#'
#' @noRd
update_geom_font <- function(
    ...,
    colour = NULL,
    fill = NULL,
    size = NULL,
    family = NULL
) {
  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Try to resolve from most specific axis text elements first
  # Check axis.text.x.bottom, axis.text.x.top, axis.text.y.left, axis.text.y.right
  axis_text_elements <- c(
    "axis.text.x.bottom",
    "axis.text.x.top",
    "axis.text.y.left",
    "axis.text.y.right"
  )

  # Find the first non-blank resolved element
  resolved_element <- axis_text_elements |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))

  # If no specific axis text element found, fall back to general text
  if (is.null(resolved_element)) {
    resolved_element <- ggplot2::calc_element("text", current_theme)
  }

  # Handle size
  if (rlang::is_null(size)) {
    # Get resolved size from theme
    size <- resolved_element$size %||% 11
    # Size from theme is already in correct units for fontsize
  } else {
    # If size is provided by user in mm, convert to fontsize scale
    # fontsize expects points, so convert mm to points
    size <- as.numeric(size) / 0.352777778
  }

  # Handle colour
  colour <- colour %||% resolved_element$colour %||% "black"

  # Handle fill - use panel.background for fill default
  if (rlang::is_null(fill)) {
    resolved_panel <- ggplot2::calc_element("panel.background", current_theme)
    fill <- resolved_panel$fill %||% "white"
  }

  # Handle family
  family <- family %||% resolved_element$family %||% ""

  ggplot2::update_theme(
    geom.text = ggplot2::element_geom(
      colour = colour,
      fontsize = size,
      family = family
    ),
    geom.label = ggplot2::element_geom(
      colour = colour,
      fill = fill,
      fontsize = size,
      family = family
    )
  )
}

#' Update the geom hline/vline
#'
#' @description
#' Updates the active theme to apply consistent reference line styling.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param colour A colour.
#' @param linewidth A linewidth.
#'
#' @return Updated geom defaults
#'
#' @noRd
update_geom_hvline <- function(
  ...,
  colour = NULL,
  linewidth = NULL
) {
  # Get current theme for font defaults
  current_theme <- ggplot2::get_theme()

  # Handle reference line linewidth defaults
  linewidth <- linewidth %||%
    current_theme$axis.line.x.bottom$linewidth %||%
    current_theme$axis.line.x.top$linewidth %||%
    current_theme$axis.line.y.left$linewidth %||%
    current_theme$axis.line.y.right$linewidth %||%
    current_theme$axis.line.x$linewidth %||%
    current_theme$axis.line.y$linewidth %||%
    current_theme$axis.line$linewidth %||%
    0.25

  colour <- colour %||%
    current_theme$axis.line.x.bottom$colour %||%
    current_theme$axis.line.x.top$colour %||%
    current_theme$axis.line.y.left$colour %||%
    current_theme$axis.line.y.right$colour %||%
    current_theme$axis.line.x$colour %||%
    current_theme$axis.line.y$colour %||%
    current_theme$axis.line$colour %||%
    "black"

  ggplot2::update_theme(
    geom.abline = ggplot2::element_geom(
      linewidth = linewidth
    ),
    geom.vline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth
    ),
    geom.hline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth
    )
  )
}

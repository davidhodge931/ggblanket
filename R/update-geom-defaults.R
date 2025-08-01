#' Update the colour/fill
#'
#' @description
#' Updates the active theme for colour/fill styling for set aesthetics.
#'
#' @param col colour/fill base for most geoms.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_col <- function(
  col = "#357BA2FF",
  ...
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
#' @param shape A shape.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for shape
#'
#' @export
update_geom_shape <- function(
  shape = 21,
  ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(pointshape = shape))
}

#' Update the linetype
#'
#' @description
#' Updates the active theme for linetype styling for set aesthetics.
#'
#' @param linetype linetype.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linetype <- function(
  linetype = 1,
  ...
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
#' @param linewidth linewidth.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linewidth <- function(
  linewidth = 0.5,
  ...
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
#' @param size A size for point geoms.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for size
#'
#' @export
update_geom_size <- function(
    size = 1.5,
    ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(pointsize = size))
}

#' Update the stroke
#'
#' @description
#' Updates the active theme for stroke styling for set aesthetics.
#'
#' @param stroke A stroke for point geoms.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for stroke
#'
#' @export
update_geom_stroke <- function(
    stroke = 0.5,
    ...
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
#' @param ... Additional arguments (not used).
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
  # Get current theme for font defaults
  current_theme <- ggplot2::get_theme()

  if (rlang::is_null(size)) {
    # Get the raw size value from theme hierarchy
    raw_size <- current_theme$axis.text.x$size %||%
      current_theme$axis.text.y$size %||%
      current_theme$axis.text$size %||%
      current_theme$text$size
    # If we found a theme size, handle rel() objects
    if (!is.null(raw_size)) {
      if (inherits(raw_size, "rel")) {
        base_size <- current_theme$text$size
        # If base_size is also rel() or NULL, use default
        if (is.null(base_size) || inherits(base_size, "rel")) {
          base_size <- 11
        }
        size <- as.numeric(raw_size) * base_size
      } else {
        size <- raw_size
      }
    } else {
      # Only use the conversion factor for the final fallback
      size <- 11
    }
    # Theme sizes are already in the correct units for fontsize
    # No conversion needed
  } else {
    # If size is provided by user in mm, convert to fontsize scale
    # fontsize expects points, so convert mm to points
    size <- as.numeric(size) / 0.352777778
  }

  colour <- colour %||%
    current_theme$axis.text.x$colour %||%
    current_theme$axis.text.y$colour %||%
    current_theme$axis.text$colour %||%
    current_theme$text$colour %||%
    "black"

  fill <- fill %||%
    current_theme$panel.background$fill %||%
    "white"

  if (rlang::is_null(family)) {
    family <- current_theme$axis.text.x$family %||%
      current_theme$axis.text.y$family %||%
      current_theme$axis.text$family %||%
      current_theme$text$family %||%
      ""
  }

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

#' Update the geom abline/vline/hline
#' Note abline updates on linewidth, but not on col.
#'
#' @description
#' Updates the active theme to apply consistent reference line styling.
#'
#' @param ... Additional arguments (not used).
#' @param colour A colour.
#' @param linewidth A linewidth.
#'
#' @return Updated geom defaults
#'
#' @noRd
update_geom_reference <- function(
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
    0.25

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

#' Update the colour and fill for geoms
#'
#' @description
#' Update the colour and fill for most geoms. Excludes "text", "label", "hline", and "vline".
#'
#' @param col A default hex code for the colour and fill of most geoms. Defaults to "#357BA2FF" (i.e. `blue`).
#' @param colour A default hex code for the colour of most geoms. Defaults to col.
#' @param fill A default hex code for the fill of most geoms. Defaults to col.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for colour and fill
#' @export
weave_geom_col <- function(col = blue, colour = col, fill = col, ...) {

  ggplot2::update_theme(
    #includes polygons (with borders)
    geom.area = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.bar = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.bin2d = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.boxplot = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.col = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.contour_filled = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.crossbar = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.density = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.density_2d_filled = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.hex = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.polygon = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.raster = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.rect = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.ribbon = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.sf = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.smooth = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.tile = ggplot2::element_geom(colour = colour, fill = fill ),
    geom.violin = ggplot2::element_geom(colour = colour, fill = fill ),

    #else includes points
    geom.point = ggplot2::element_geom(colour = colour, fill = colour),
    geom.pointrange = ggplot2::element_geom(colour = colour, fill = colour),

    #else just lines
    geom.abline = ggplot2::element_geom(colour = colour),
    geom.contour = ggplot2::element_geom(colour = colour),
    geom.curve = ggplot2::element_geom(colour = colour),
    geom.density2d = ggplot2::element_geom(colour = colour),
    geom.errorbar = ggplot2::element_geom(colour = colour),
    geom.function = ggplot2::element_geom(colour = colour),
    geom.line = ggplot2::element_geom(colour = colour),
    geom.linerange = ggplot2::element_geom(colour = colour),
    geom.path = ggplot2::element_geom(colour = colour),
    geom.quantile = ggplot2::element_geom(colour = colour),
    geom.rug = ggplot2::element_geom(colour = colour),
    geom.segment = ggplot2::element_geom(colour = colour),
    geom.spoke = ggplot2::element_geom(colour = colour),
    geom.step = ggplot2::element_geom(colour = colour),
  )
}

#' Update the linetype for some geoms
#'
#' @description
#' Update the linetype for geoms with unnecessary border lines to 0. Excludes boxplot, crossbar and smooth.
#'
#' @param linetype A default linetype for geoms not specified by linetype_border, linetype_box. or linetype_sf. Defaults to 1.
#' @param linetype_sf A default linetype for sf geoms. Defaults to 0.
#' @param linetype_box A default linetype for boxplot and crossbar geoms. Defaults to 1.
#' @param linetype_border A default linetype for polygon-is geoms (other than boxplot, crossbar or sf). Defaults to 0.
#'
#' @return Updated geom defaults for linetype
#' @export
#'
weave_geom_linetype <- function(linetype = 1, linetype_sf = 1, linetype_box = 1, linetype_border = 0) {

  ggplot2::update_theme(

    #includes everything
    geom.sf = ggplot2::element_geom(linetype = linetype_sf),

    #includes polygons with necessary borders
    geom.boxplot = ggplot2::element_geom(bordertype = linetype_box),
    geom.crossbar = ggplot2::element_geom(bordertype = linetype_box),

    #includes polygons (with borders)
    geom.area = ggplot2::element_geom(bordertype = linetype_border),
    geom.bar = ggplot2::element_geom(bordertype = linetype_border),
    geom.bin2d = ggplot2::element_geom(bordertype = linetype_border),
    geom.col = ggplot2::element_geom(bordertype = linetype_border),
    geom.contour_filled = ggplot2::element_geom(bordertype = linetype_border),
    geom.density = ggplot2::element_geom(bordertype = linetype_border),
    geom.density_2d_filled = ggplot2::element_geom(bordertype = linetype_border),
    geom.hex = ggplot2::element_geom(bordertype = linetype_border),
    geom.polygon = ggplot2::element_geom(bordertype = linetype_border),
    geom.raster = ggplot2::element_geom(bordertype = linetype_border),
    geom.rect = ggplot2::element_geom(bordertype = linetype_border),
    geom.ribbon = ggplot2::element_geom(bordertype = linetype_border),
    geom.tile = ggplot2::element_geom(bordertype = linetype_border),
    geom.violin = ggplot2::element_geom(bordertype = linetype_border),

    # else smooth which ggplot2 interprets as line
    geom.smooth = ggplot2::element_geom(linetype = linetype),

    #else includes points
    geom.point = ggplot2::element_geom(linetype = linetype),
    geom.pointrange = ggplot2::element_geom(linetype = linetype),

    #else just lines
    geom.abline = ggplot2::element_geom(linetype = linetype),
    geom.contour = ggplot2::element_geom(linetype = linetype),
    geom.curve = ggplot2::element_geom(linetype = linetype),
    geom.density2d = ggplot2::element_geom(linetype = linetype),
    geom.errorbar = ggplot2::element_geom(linetype = linetype),
    geom.function = ggplot2::element_geom(linetype = linetype),
    geom.line = ggplot2::element_geom(linetype = linetype),
    geom.linerange = ggplot2::element_geom(linetype = linetype),
    geom.path = ggplot2::element_geom(linetype = linetype),
    geom.quantile = ggplot2::element_geom(linetype = linetype),
    geom.rug = ggplot2::element_geom(linetype = linetype),
    geom.segment = ggplot2::element_geom(linetype = linetype),
    geom.spoke = ggplot2::element_geom(linetype = linetype),
    geom.step = ggplot2::element_geom(linetype = linetype),
  )
}

#' Update the linewidth for geoms
#'
#' @description
#' Update the linewidth for most geoms. Excludes "text", "label", "hline", and "vline".
#'
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for linewidth
#' @export
weave_geom_linewidth <- function(linewidth = 0.66, ...) {

  #includes polygons (with borders)
  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.bar = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.bin2d = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.boxplot = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.col = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.contour_filled = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.crossbar = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.density = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.density_2d_filled = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.hex = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.polygon = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.raster = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.rect = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.ribbon = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.smooth = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.sf = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.tile = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),
    geom.violin = ggplot2::element_geom(borderwidth = linewidth, linewidth = linewidth),

    #else includes points
    geom.point = ggplot2::element_geom(linewidth = linewidth),
    geom.pointrange = ggplot2::element_geom(linewidth = linewidth),

    #else just lines
    geom.abline = ggplot2::element_geom(linewidth = linewidth),
    geom.contour = ggplot2::element_geom(linewidth = linewidth),
    geom.curve = ggplot2::element_geom(linewidth = linewidth),
    geom.density2d = ggplot2::element_geom(linewidth = linewidth),
    geom.errorbar = ggplot2::element_geom(linewidth = linewidth),
    geom.function = ggplot2::element_geom(linewidth = linewidth),
    geom.line = ggplot2::element_geom(linewidth = linewidth),
    geom.linerange = ggplot2::element_geom(linewidth = linewidth),
    geom.path = ggplot2::element_geom(linewidth = linewidth),
    geom.quantile = ggplot2::element_geom(linewidth = linewidth),
    geom.rug = ggplot2::element_geom(linewidth = linewidth),
    geom.segment = ggplot2::element_geom(linewidth = linewidth),
    geom.spoke = ggplot2::element_geom(linewidth = linewidth),
    geom.step = ggplot2::element_geom(linewidth = linewidth),
  )
}

#' Update the size for point/pointrange geoms
#'
#' @description
#' Update the size for point/pointrange geoms. Excludes "text" and "label".
#'
#' @param size A default size for the point geom. Defaults to 1.5. The pointrange size defaults to dividing by 6 (i.e. 0.25).
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for size
#' @export
weave_geom_size <- function(size = 1.5, ...) {

  ggplot2::update_theme(
    geom.point = ggplot2::element_geom(pointsize = size),
    geom.pointrange = ggplot2::element_geom(pointsize = size / 6),
  )
}

#' Set the text geom defaults
#'
#' @description Update the "text" geom defaults. Note all other text is controlled by the theme.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour A hex code.
#' @param fill A hex code.
#' @param size A size in mm. Use `pt_to_mm` to convert.
#' @param family A family.
#'
#' @noRd
#'
#' @examples
#' weave_geom_label(size = 4.233333)
#' weave_geom_label(size = pt_to_mm(12))
#'
weave_geom_text <- function(
    ...,
    colour = NULL,
    size = NULL,
    family = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract theme properties with fallback hierarchy
  if (rlang::is_null(colour)) {
    colour <- current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"
  }

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

  if (rlang::is_null(family)) {
    family <- current_theme$axis.text.x$family %||%
      current_theme$axis.text.y$family %||%
      current_theme$axis.text$family %||%
      current_theme$text$family %||%
      ""
  }

  # Update the theme
  ggplot2::update_theme(
    geom.text = ggplot2::element_geom(
      colour = colour,
      family = family,
      fontsize = size  # Size in mm, matching geom_text behavior
    )
  )
}

#' Set the label geom defaults
#'
#' @description Update the "label" geom defaults. Note all other text is controlled by the theme.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param fill A hex code.
#' @param colour A hex code.
#' @param size A size in mm. Use `pt_to_mm` to convert.
#' @param family A family.
#'
#' @noRd
#'
#' @examples
#' weave_geom_label(size = 4.233333)
#' weave_geom_label(size = pt_to_mm(12))
#'
weave_geom_label <- function(
    ...,
    fill = NULL,
    colour = NULL,
    size = NULL,
    family = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract theme properties with fallback hierarchy
  if (rlang::is_null(fill)) {
    fill <- current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"
  }

  if (rlang::is_null(colour)) {
    colour <- current_theme$panel.background$fill %||%
      current_theme$plot.background$fill %||%
      "white"
  }

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

  if (rlang::is_null(family)) {
    family <- current_theme$axis.text.x$family %||%
      current_theme$axis.text.y$family %||%
      current_theme$axis.text$family %||%
      current_theme$text$family %||%
      ""
  }

  # Update the theme
  ggplot2::update_theme(
    geom.label = ggplot2::element_geom(
      colour = colour,
      fill = fill,
      family = family,
      fontsize = size  # Size in correct scale for fontsize
    )
  )
}

#' Set the geom hline defaults
#'
#' @description Update the "hline" geom defaults.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour A hex code.
#' @param linewidth A linewidth.
#'
#' @noRd
weave_geom_hline <- function(
    ...,
    colour = NULL,
    linewidth = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract theme properties with fallback hierarchy
  if (rlang::is_null(colour)) {
    colour <- current_theme$axis.line.x.bottom$colour %||%
      current_theme$axis.line.x.top$colour %||%
      current_theme$axis.line.x$colour %||%
      current_theme$axis.line$colour %||%
      "black"
  }

  if (rlang::is_null(linewidth)) {
    linewidth <- current_theme$axis.line.x.bottom$linewidth %||%
      current_theme$axis.line.x.top$linewidth %||%
      current_theme$axis.line.x$linewidth %||%
      current_theme$axis.line$linewidth %||%
      0.25
  }

  # Update the theme
  ggplot2::update_theme(
    geom.hline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth
    )
  )
}

#' Set the geom vline defaults
#'
#' @description Update the "vline" geom defaults.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour A hex code.
#' @param linewidth A linewidth.
#'
#' @noRd
weave_geom_vline <- function(
    ...,
    colour = NULL,
    linewidth = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract theme properties with fallback hierarchy
  if (rlang::is_null(colour)) {
    colour <- current_theme$axis.line.y.left$colour %||%
      current_theme$axis.line.y.right$colour %||%
      current_theme$axis.line.y$colour %||%
      current_theme$axis.line$colour %||%
      "black"
  }


  if (rlang::is_null(linewidth)) {
    linewidth <- current_theme$axis.line.y.left$linewidth %||%
      current_theme$axis.line.y.right$linewidth %||%
      current_theme$axis.line.y$linewidth %||%
      current_theme$axis.line$linewidth %||%
      0.25
  }

  # Update the theme
  ggplot2::update_theme(
    geom.vline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth
    )
  )
}

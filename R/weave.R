# global defaults
ggblanket_global <- new.env(parent = emptyenv())

ggblanket_global$theme <- NULL
ggblanket_global$perspective <- NULL
ggblanket_global$axis_line_transparent <- NULL
ggblanket_global$axis_ticks_transparent <- NULL
ggblanket_global$axis_line_transparent <- NULL

ggblanket_global$label_case <- NULL

ggblanket_global$col_palette_discrete <- NULL
ggblanket_global$col_palette_continuous <- NULL
ggblanket_global$col_palette_o <- NULL
ggblanket_global$col_palette_na_d <- NULL
ggblanket_global$col_palette_na_c <- NULL
ggblanket_global$col_palette_na_o <- NULL

#' Set a theme
#'
#' @description Set a theme for the theme argument in `gg_*` functions.
#'
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param perspective The perspective of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `perspective` of the plot.
#'
#' @noRd
weave_theme <- function(
    theme = theme_lighter(),
    ...,
    perspective = NULL,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE
) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- theme
  invisible(old)

  old <- ggblanket_global$perspective
  ggblanket_global$perspective <- perspective
  invisible(old)

  old <- ggblanket_global$axis_line_transparent
  ggblanket_global$axis_line_transparent <- axis_line_transparent
  invisible(old)

  old <- ggblanket_global$axis_ticks_transparent
  ggblanket_global$axis_ticks_transparent <- axis_ticks_transparent
  invisible(old)

  old <- ggblanket_global$panel_grid_transparent
  ggblanket_global$panel_grid_transparent <- panel_grid_transparent
  invisible(old)

  if (ggplot2::is_theme(theme)) {
    ggplot2::set_theme(new = theme)
  } else {
    ggplot2::set_theme(new = theme[[1]]) #if list, assume first element is theme
  }
}

#' Set a label case function
#'
#' @description Set a function to format the label of unlabelled variables.
#'
#' @param label_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_label_case <- function(label_case = snakecase::to_sentence_case, ...) {
  old <- ggblanket_global$label_case
  ggblanket_global$label_case <- label_case
  invisible(old)
}

#' Update the colour and fill for geoms
#'
#' @description
#' Update the colour and fill for most geoms. Excludes "text", "label", "hline", and "vline".
#'
#' @param colour A default hex code for the colour of most geoms. Defaults to blue.
#' @param fill A default hex code for the fill of most geoms. Inherits from colour.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for colour and fill
#' @export
weave_geom_colour_fill <- function(colour = blue, fill = colour, ...) {

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
#' Update the linetype for geoms with unnecessary border lines to zero. Excludes boxplot and crossbar.
#'
#' @param bordertype A default linetype for geoms with unnecessary border lines. Defaults to 0.
#'
#' @return Updated geom defaults for linetype
#' @export
#'
weave_geom_bordertype <- function(bordertype = 0) {

  ggplot2::update_theme(

    #includes polygons with necessary borders
    # geom.boxplot = ggplot2::element_geom(bordertype = 1, linetype = 1),
    # geom.crossbar = ggplot2::element_geom(bordertype = 1, linetype = 1),

    #includes polygons (with borders)
    geom.area = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.bar = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.bin2d = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.col = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.contour_filled = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.density = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.density_2d_filled = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.hex = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.polygon = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.raster = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.rect = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.ribbon = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.sf = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.tile = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),
    geom.violin = ggplot2::element_geom(bordertype = bordertype, linetype = bordertype),

    #else includes points
    # geom.point = ggplot2::element_geom(linetype = 1),
    # geom.pointrange = ggplot2::element_geom(linetype = 1),

    #else just lines
    # geom.abline = ggplot2::element_geom(linetype = 1),
    # geom.contour = ggplot2::element_geom(linetype = 1),
    # geom.curve = ggplot2::element_geom(linetype = 1),
    # geom.density2d = ggplot2::element_geom(linetype = 1),
    # geom.errorbar = ggplot2::element_geom(linetype = 1),
    # geom.function = ggplot2::element_geom(linetype = 1),
    # geom.line = ggplot2::element_geom(linetype = 1),
    # geom.linerange = ggplot2::element_geom(linetype = 1),
    # geom.path = ggplot2::element_geom(linetype = 1),
    # geom.quantile = ggplot2::element_geom(linetype = 1),
    # geom.rug = ggplot2::element_geom(linetype = 1),
    # geom.segment = ggplot2::element_geom(linetype = 1),
    # geom.spoke = ggplot2::element_geom(linetype = 1),
    # geom.step = ggplot2::element_geom(linetype = 1),
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


#' Set geom palettes
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param col_palette_discrete For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default. Use NULL to leave as is.
#' @param col_palette_continuous For a continuous scale, a character vector of hex codes. Use NULL for ggplot2 default. Use NULL to leave as is.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default. Use NULL to leave as is.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#'
#' @noRd
weave_geom_palettes <- function(
  ...,
  col_palette_discrete = jumble,
  col_palette_continuous = viridisLite::mako(n = 9, direction = -1),
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_d = "#CDC5BFFF",
  col_palette_na_c = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
  col_palette_na_o = "#988F88FF"
) {
  if (!rlang::is_null(col_palette_discrete)) {
    weave_col_palette_discrete(
      col_palette_discrete = col_palette_discrete,
      col_palette_na_d = col_palette_na_d
    )
  }
  if (!rlang::is_null(col_palette_continuous)) {
    weave_col_palette_continuous(
      col_palette_continuous = col_palette_continuous,
      col_palette_na_c = col_palette_na_c
    )
  }
  if (!rlang::is_null(col_palette_o)) {
    weave_col_palette_o(
      col_palette_o = col_palette_o,
      col_palette_na_o = col_palette_na_o
    )
  }
}

#' Set a discrete geom_colour and geom_fill palette
#'
#' @param col_palette_discrete For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_col_palette_discrete <- function(
  col_palette_discrete = jumble,
  col_palette_na_d = "#CDC5BFFF",
  ...
) {
  if (rlang::is_null(col_palette_na_d)) {
    col_palette_na_d <- "grey50"
  }

  if (!rlang::is_null(col_palette_discrete)) {
    if (!rlang::is_function(col_palette_discrete)) {
      col_palette_discrete <- c(col_palette_discrete, rep(col_palette_na_d, times = 100))
    }
    old <- ggblanket_global$col_palette_discrete
    ggblanket_global$col_palette_discrete <- col_palette_discrete
    invisible(old)

    old <- ggblanket_global$col_palette_na_d
    ggblanket_global$col_palette_na_d <- col_palette_na_d
    invisible(old)

    options(
      ggplot2.discrete.colour = function() {
        ggplot2::scale_colour_manual(
          values = col_palette_discrete,
          na.value = col_palette_na_d
        )
      },
      ggplot2.discrete.fill = function() {
        ggplot2::scale_fill_manual(
          values = col_palette_discrete,
          na.value = col_palette_na_d
        )
      }
    )
  }
}

#' Set a continuous geom_colour and geom_fill palette
#'
#' @param col_palette_continuous For a continuous scale, A character vector of hex codes (or names) or a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_col_palette_continuous <- function(
  col_palette_continuous = viridisLite::mako(n = 9, direction = -1),
  col_palette_na_c = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
  ...
) {
  if (rlang::is_null(col_palette_continuous)) {
    col_palette_continuous <- scales::pal_seq_gradient(
      low = "#132B43",
      high = "#56B1F7"
    )(seq(0, 1, length.out = 20))
  }

  if (rlang::is_null(col_palette_na_c)) {
    col_palette_na_c <- "grey50"
  }

  col_palette_continuous <- if (is.function(col_palette_continuous)) {
    col_palette_continuous(256)
  } else {
    col_palette_continuous
  }

  old <- ggblanket_global$col_palette_continuous
  ggblanket_global$col_palette_continuous <- col_palette_continuous
  invisible(old)

  old <- ggblanket_global$col_palette_na_c
  ggblanket_global$col_palette_na_c <- col_palette_na_c
  invisible(old)

  options(
    ggplot2.continuous.colour = function() {
      ggplot2::scale_colour_gradientn(
        colours = col_palette_continuous,
        na.value = col_palette_na_c
      )
    },
    ggplot2.continuous.fill = function() {
      ggplot2::scale_fill_gradientn(
        colours = col_palette_continuous,
        na.value = col_palette_na_c
      )
    }
  )
}

#' Set an ordinal geom_colour and geom_fill palette
#'
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_col_palette_o <- function(
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_o = "#988F88FF",
  ...
) {
  if (rlang::is_null(col_palette_o)) {
    col_palette_o <- scales::pal_viridis()
  }
  if (rlang::is_null(col_palette_na_o)) {
    col_palette_na_o <- "grey50"
  }

  old <- ggblanket_global$col_palette_o
  ggblanket_global$col_palette_o <- col_palette_o
  invisible(old)

  old <- ggblanket_global$col_palette_na_o
  ggblanket_global$col_palette_na_o <- col_palette_na_o
  invisible(old)
}

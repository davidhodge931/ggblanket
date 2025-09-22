#' Set the style
#'
#' @description
#' Set a consistent style. Most users will only need
#' `theme`, `col`, `col_palette_discrete`, `col_palette_continuous` and `aspect_*` arguments.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme (e.g. [theme_greyer()], [theme_greyer()], [theme_darker()] or [theme_lighter()]).
#' @param col A default hex code for the colour and fill of most geoms.
#' @param col_palette_discrete For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_continuous For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_ordinal For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_continuous`.
#' @param col_na A NA colour/fill value.
#' @param colour_border_transform A function with input of the `col` or `col_palette`.
#' @param fill_border_transform A function with input of the `col` or `col_palette`.
#' @param shape A default shape for point geoms. Must be an integer between 0 and 25.
#' @param shape_palette_discrete For shape scales, a numeric vector of shape codes.
#' @param shape_na A NA shape value.
#' @param linetype A default linetype for most geoms.
#' @param linetype_palette_discrete For linetype scales, a character vector or a `scales::pal_*` function.
#' @param linewidth A default linewidth for geoms. A number.
#' @param linewidth_border A default linewidth for geoms that have a border. A number.
#' @param size A default size for point geoms.
#' @param stroke A default stroke for point geoms.
#' @param axis_line_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis line for an `"x"` `aspect`, and vice versa.
#' @param axis_ticks_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis ticks for an `"x"` `aspect`, and vice versa.
#' @param panel_grid_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the x panel grid for an `"x"` `aspect`, and vice versa.
#' @param panel_heights The height of the panels. E.g. `grid::unit(5, "cm")`.
#' @param panel_widths The width of the panels. E.g. `grid::unit(7.5, "cm")`.
#'
#' @return Invisibly returns NULL. Sets global styling options as a side effect.
#'
#' @seealso
#' [theme_greyer()], [theme_darker()] for theme options
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#'
#' # Use col_palette_discrete for both colour and fill
#' set_blanket(
#'   col_palette_discrete = c("#E69F00", "#56B4E9", "#009E73")
#' )
#'
set_blanket <- function(
    ...,
    theme = theme_greyer(),
    col = ifelse(is_panel_dark(), ocean, blue),
    col_palette_discrete = scales::pal_hue(),
    col_palette_continuous = direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9)),
    shape = 21,
    linetype = 1,
    linewidth = 0.66,
    linewidth_border = 0.25,
    size = 1.5,
    stroke = 0.5,
    panel_heights = NULL,
    panel_widths = NULL,
    # options
    col_palette_ordinal = NULL,
    col_na = "#A6A6A6FF",
    shape_palette_discrete = c(21, 24, 22, 23, 25),
    shape_na = 4,
    linetype_palette_discrete = 1:6,
    colour_border_transform = \(x) {
      if (is_panel_dark()) {
        blend_screen(x)
      } else {
        blend_multiply(x)
      }
    },
    fill_border_transform = NULL,
    axis_line_aspect = "transparent",
    axis_ticks_aspect = "transparent",
    panel_grid_aspect = "transparent"
) {

  # Weave theme and adjustments
  weave_theme(
    theme = theme,
    col = col,
    shape = shape,
    linetype = linetype,
    linewidth = linewidth,
    linewidth_border = linewidth_border,
    size = size,
    stroke = stroke,
    col_palette_discrete = col_palette_discrete,
    col_palette_continuous = col_palette_continuous,
    panel_heights = panel_heights,
    panel_widths = panel_widths
  )

  # Weave options
  weave_options(
    col_palette_ordinal = col_palette_ordinal,
    col_na = col_na,
    shape_palette_discrete = shape_palette_discrete,
    shape_na = shape_na,
    linetype_palette_discrete = linetype_palette_discrete,
    colour_border_transform = colour_border_transform,
    fill_border_transform = fill_border_transform,
    axis_line_aspect = axis_line_aspect,
    axis_ticks_aspect = axis_ticks_aspect,
    panel_grid_aspect = panel_grid_aspect
  )

  invisible(NULL)
}

#' Weave theme settings
#'
#' @description
#' Weaves together the base theme and context-dependent theme adjustments.
#' This function sets the ggplot2 theme and updates various geom defaults.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object to set as the base theme.
#' @param col A default hex code for the colour and fill of most geoms.
#' @param shape A default shape for point geoms. Must be an integer between 0 and 25.
#' @param linetype A default linetype for most geoms.
#' @param linewidth A default linewidth for geoms.
#' @param linewidth_border A default linewidth for geoms that have a border.
#' @param size A default size for point geoms.
#' @param stroke A default stroke for point geoms.
#' @param col_palette_discrete For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_continuous For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param panel_heights The height of the panels. E.g. `grid::unit(5, "cm")`.
#' @param panel_widths The width of the panels. E.g. `grid::unit(7.5, "cm")`.
#'
#' @return Invisibly returns NULL. Updates the active theme as a side effect.
#'
#' @export
weave_theme <- function(
    ...,
    theme = NULL,
    col = NULL,
    shape = NULL,
    linetype = NULL,
    linewidth = NULL,
    linewidth_border = NULL,
    size = NULL,
    stroke = NULL,
    col_palette_discrete = NULL,
    col_palette_continuous = NULL,
    panel_heights = NULL,
    panel_widths = NULL
) {

  # Set the base theme if provided
  if (!rlang::is_null(theme)) {
    ggplot2::set_theme(theme)
  }

  # Update geom col
  if (!rlang::is_null(col)) {
    update_geom_col(col = col)
  }

  # Update geom shape
  if (!rlang::is_null(shape)) {
    update_geom_shape(shape = shape)
  }

  # Update geom linetype
  if (!rlang::is_null(linetype)) {
    update_geom_linetype(linetype = linetype)
  }

  # Update geom linewidth (including border)
  if (!rlang::is_null(linewidth) || !rlang::is_null(linewidth_border)) {
    update_geom_linewidth(
      linewidth = linewidth,
      linewidth_border = linewidth_border
    )
  }

  # Update geom size
  if (!rlang::is_null(size)) {
    update_geom_size(size = size)
  }

  # Update geom stroke
  if (!rlang::is_null(stroke)) {
    update_geom_stroke(stroke = stroke)
  }

  # Update geom palettes
  if (!rlang::is_null(col_palette_discrete) || !rlang::is_null(col_palette_continuous)) {
    update_geom_palettes(
      col_palette_discrete = col_palette_discrete,
      col_palette_continuous = col_palette_continuous
    )
  }

  # Always update font to ensure consistency
  update_geom_font()

  # Update panel dimensions
  if (!rlang::is_null(panel_heights) || !rlang::is_null(panel_widths)) {
    update_panel_dimensions(
      panel_heights = panel_heights,
      panel_widths = panel_widths
    )
  }

  invisible(NULL)
}











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
    col = ifelse(is_panel_dark(), ocean, blue)
) {

  if (!rlang::is_null(col)) {
    ggplot2::update_theme(geom = ggplot2::element_geom(colour = col, fill = col))
  }
}

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
    col = ifelse(is_panel_dark(), ocean, blue)
) {

  if (!rlang::is_null(col)) {
    ggplot2::update_theme(geom = ggplot2::element_geom(colour = col, fill = col))
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
  if (!rlang::is_null(shape)) {
    ggplot2::update_theme(geom = ggplot2::element_geom(pointshape = shape))
  }
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
  if (!rlang::is_null(linetype)) {
    ggplot2::update_theme(
      geom = ggplot2::element_geom(linetype = linetype, bordertype = linetype)
    )
  }
}

#' Update the linewidth
#'
#' @description
#' Updates the active theme for linewidth styling for set aesthetics.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param linewidth linewidth.
#' @param linewidth_border A number, or a function with input of the set linewidth.
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linewidth <- function(
    ...,
    linewidth = 0.66,
    linewidth_border = 0.25
) {

  if (!rlang::is_null(linewidth)) {
    ggplot2::update_theme(
      geom = ggplot2::element_geom(
        linewidth = linewidth,
        borderwidth = linewidth
      )
    )
  }

  #border
  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.bin2d = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.col = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.contour_filled = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.density = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.density2d_filled = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.map = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.raster = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.sf = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.tile = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
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

  if (!rlang::is_null(size)) {
    ggplot2::update_theme(geom = ggplot2::element_geom(pointsize = size))
  }
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
  if (!rlang::is_null(stroke)) {
    ggplot2::update_theme(
      geom.point = ggplot2::element_geom(borderwidth = stroke),
      geom.jitter = ggplot2::element_geom(borderwidth = stroke),
      geom.count = ggplot2::element_geom(borderwidth = stroke),
      geom.qq = ggplot2::element_geom(borderwidth = stroke)
    )
  }
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
    purrr::detect(\(x) !rlang::is_null(x) && !inherits(x, "element_blank"))

  # If no specific axis text element found, fall back to general text
  if (rlang::is_null(resolved_element)) {
    resolved_element <- ggplot2::calc_element("text", current_theme)
  }

  # Handle size
  if (rlang::is_null(size)) {
    # Get resolved size from theme
    size <-  resolved_element$size %||% 11
    # Size from theme is already in correct units for fontsize
  } else {
    # If size is provided by user in mm, convert to fontsize scale
    # fontsize expects points, so convert mm to points
    size <- as.numeric(size) / 0.352777778
  }

  # Handle colour
  colour <- colour %||% resolved_element$colour %||% "black"

  # Handle fill - use panel.background for fill default
  fill <- fill %||% "white"

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

#' Update panel dimensions
#'
#' @description
#' Updates the active theme for panel height and width styling.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param panel_heights The height of the panels.
#' @param panel_widths The width of the panels.
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_panel_dimensions <- function(
    ...,
    panel_heights = NULL,
    panel_widths = NULL
) {

  if (!rlang::is_null(panel_heights)) {
    ggplot2::update_theme(
      panel.heights = panel_heights,
    )
  }

  if (!rlang::is_null(panel_widths)) {
    ggplot2::update_theme(
      panel.widths = panel_widths,
    )
  }
}

#' Update the default geom palettes
#'
#' @description
#' Updates the active theme for consistent colour/fill palette styling.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param col_palette_discrete For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_continuous For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_palettes <- function(
    ...,
    col_palette_discrete = scales::pal_hue(),
    col_palette_continuous = direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
) {
  # Update theme-level palettes
  ggplot2::update_theme(
    palette.colour.discrete = col_palette_discrete,
    palette.fill.discrete = col_palette_discrete,
    palette.colour.continuous = col_palette_continuous,
    palette.fill.continuous = col_palette_continuous
  )
}

#' Update the global options
#'
#' @description
#' Sets global options.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param col_palette_ordinal For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_continuous`.
#' @param col_na A hex code (or name) for the `NA` value.
#' @param shape_palette_discrete For shape scales, a numeric vector of shape codes. Defaults to c(21, 24, 22, 23, 25).
#' @param shape_na A NA shape value.
#' @param linetype_palette_discrete For linetype scales, a character vector or a `scales::pal_*` function. Defaults to 1:6.
#' @param colour_border_transform A function with input of the set `col`. Defaults to `blend_screen`/`blend_multiply` based on the panel.
#' @param fill_border_transform A function with input of the set `col`. Defaults to NULL.
#' @param axis_line_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis line for an `"x"` `aspect`, and vice versa.
#' @param axis_ticks_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis ticks for an `"x"` `aspect`, and vice versa.
#' @param panel_grid_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the x panel grid for an `"x"` `aspect`, and vice versa.
#'
#' @return Global options.
#'
#' @export
weave_options <- function(
    ...,
    col_palette_ordinal = NULL,
    col_na = "#A6A6A6FF",
    shape_palette_discrete = c(21, 24, 22, 23, 25),
    shape_na = 4,
    linetype_palette_discrete = 1:6,
    colour_border_transform = \(x) {
      if (is_panel_dark()) {
        blend_screen(x)
      } else {
        blend_multiply(x)
      }
    },
    fill_border_transform = NULL,
    axis_line_aspect = "transparent",
    axis_ticks_aspect = "transparent",
    panel_grid_aspect = "transparent"
) {
  # Handle ordinal palette default
  if (rlang::is_null(col_palette_ordinal)) {
    col_palette_continuous <- ggplot2::calc_element("palette.colour.continuous", ggplot2::get_theme())
    if (rlang::is_null(col_palette_continuous)) {
      col_palette_continuous <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
    }

    # If continuous palette is a vector, convert to gradient function
    if (is.character(col_palette_continuous) || is.numeric(col_palette_continuous)) {
      col_palette_ordinal <- scales::pal_gradient_n(colours = col_palette_continuous)
    } else if (is.function(col_palette_continuous)) {
      # If it's already a function, use it directly
      col_palette_ordinal <- col_palette_continuous
    }
  }

  # Validate aspect args
  if (!axis_line_aspect %in% c("transparent", "blank", "keep")) {
    rlang::abort("axis_line_aspect must be 'transparent', 'blank', or 'keep'")
  }
  if (!axis_ticks_aspect %in% c("transparent", "blank", "keep")) {
    rlang::abort("axis_ticks_aspect must be 'transparent', 'blank', or 'keep'")
  }
  if (!panel_grid_aspect %in% c("transparent", "blank", "keep")) {
    rlang::abort("panel_grid_aspect must be 'transparent', 'blank', or 'keep'")
  }

  # Set global ggblanket options for the rest
  options(
    # Ordinal col palette
    ggblanket.col_palette_ordinal = col_palette_ordinal,

    # NA col value
    ggblanket.col_na = col_na,
    ggblanket.shape_na = shape_na,

    # Other palettes
    ggblanket.shape_palette_discrete = shape_palette_discrete,
    ggblanket.linetype_palette_discrete = linetype_palette_discrete,

    # Border transform
    ggblanket.colour_border_transform = colour_border_transform,
    ggblanket.fill_border_transform = fill_border_transform,

    # Aspect
    ggblanket.axis_line_aspect = axis_line_aspect,
    ggblanket.axis_ticks_aspect = axis_ticks_aspect,
    ggblanket.panel_grid_aspect = panel_grid_aspect
  )
}

#' Automatically Orient Palette Direction for Panel Contrast
#'
#' @description
#' Orients a palette to maximize contrast with the ggplot2 panel background.
#' Compares the luminance of palette endpoints with the panel and ensures
#' maximum contrast is placed at high values (default) or low values (reversed).
#' Returns the palette in the same format as provided.
#'
#' @param palette A palette in any format:
#'   - A function that takes n (e.g., `scales::pal_viridis()`)
#'   - A function that takes values 0-1 (e.g., `scales::pal_gradient_n()`)
#'   - A character vector of colours
#'   - Other colour objects
#' @param ... Additional arguments (for extensibility)
#' @param reverse Logical. If FALSE (default), maximum contrast is placed
#'   at high values. If TRUE, maximum contrast is placed at low values.
#'
#' @return The palette in the same format as input, oriented for optimal contrast
#' @export
direction_contrast <- function(palette, ..., reverse = FALSE) {
  # Force evaluation
  force(palette)
  force(reverse)

  # Handle functions
  if (is.function(palette)) {
    # Capture all attributes from the original function
    original_attrs <- attributes(palette)

    # Return a wrapped function that applies direction at call time
    if (inherits(palette, "pal_discrete")) {
      # n-based function
      wrapped_fn <- function(x) {
        colours <- palette(x)
        .apply_direction(colours, reverse)
      }
    } else if (inherits(palette, "pal_continuous")) {
      # value-based function
      wrapped_fn <- function(x) {
        colours <- palette(x)
        .apply_direction(colours, reverse)
      }
    } else {
      # Unknown function type - return wrapped version that tries to detect
      wrapped_fn <- function(x) {
        # Try to get colours from the palette
        colours <- tryCatch(
          palette(x),
          error = function(e) {
            # If x is a single number, might be n-based, try as values
            if (length(x) == 1 && is.numeric(x)) {
              palette(seq(0, 1, length.out = x))
            } else {
              stop(e)
            }
          }
        )
        .apply_direction(colours, reverse)
      }
    }

    # CRITICAL: Preserve ALL attributes from the original function
    # This includes class, type, nlevels, etc.
    attributes(wrapped_fn) <- original_attrs

    # Add a marker that this has been wrapped with direction
    attr(wrapped_fn, "direction_wrapped") <- TRUE

    return(wrapped_fn)
  }

  # Handle non-functions (vectors)
  colours <- as.character(palette)

  # Single colour: return unchanged
  if (length(colours) == 1) {
    return(palette)  # Return in original format
  }

  # Multiple colours: apply direction
  oriented_colours <- .apply_direction(colours, reverse)

  # Return in same format as input
  if (inherits(palette, "character")) {
    return(oriented_colours)
  } else {
    # Preserve original class if possible
    tryCatch(
      structure(oriented_colours, class = class(palette)),
      error = function(e) oriented_colours
    )
  }
}

#' Apply direction logic to a colour vector
#' @noRd
.apply_direction <- function(colours, reverse = FALSE) {
  # Single colour or empty: return unchanged
  if (length(colours) <= 1) {
    return(colours)
  }

  # Get panel luminance
  panel_luminance <- .get_panel_luminance()

  # Get luminance of first and last colours
  first_lum <- .get_colour_luminance(colours[1])
  last_lum <- .get_colour_luminance(colours[length(colours)])

  # Calculate contrast with panel
  first_contrast <- abs(first_lum - panel_luminance)
  last_contrast <- abs(last_lum - panel_luminance)

  # Determine if reversal is needed
  needs_reversal <- if (!reverse) {
    # Want maximum contrast at high end (default)
    last_contrast < first_contrast
  } else {
    # Want maximum contrast at low end
    first_contrast < last_contrast
  }

  if (needs_reversal) {
    rev(colours)
  } else {
    colours
  }
}

#' Get panel background luminance
#' @noRd
.get_panel_luminance <- function(theme = NULL) {
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour
  panel_col <- ggplot2::calc_element(theme = theme, element = "panel.background")@fill

  # If panel is transparent/NA, check plot background
  if (.is_transparent_or_na(panel_col)) {
    plot_col <- ggplot2::calc_element(theme = theme, element = "plot.background")@fill

    if (.is_transparent_or_na(plot_col)) {
      return(100)  # Default to light
    }

    panel_col <- plot_col
  }

  `.get_colour_luminance`(panel_col)
}

#' Get colour luminance
#' @noRd
.get_colour_luminance <- function(col) {
  if (.is_transparent_or_na(col)) {
    return(100)  # Default to light
  }

  tryCatch({
    farver::get_channel(colour = col, channel = "l", space = "hcl")
  }, error = function(e) {
    100  # Default to light if error
  })
}

#' Check if a colour value is transparent or NA
#' @noRd
.is_transparent_or_na <- function(col) {
  rlang::is_null(col) ||
    length(col) == 0 ||
    is.na(col) ||
    identical(col, "transparent") ||
    identical(col, NA_character_) ||
    (is.character(col) && tolower(col) == "transparent")
}

#' Check if theme panel background is dark
#'
#' @description
#' Determines whether the current ggplot2 theme has a dark or light panel background
#' by examining its luminance.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @export
is_panel_dark <- function(..., theme = NULL) {
  # Get theme if not provided
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  col <- ggplot2::calc_element(theme = theme, element = "panel.background")@fill

  # Use .is_col_dark to check if the panel colour is dark
  is_col_dark(col)
}

#' Check if a colour is dark
#'
#' @description
#' Determines whether a colour is dark by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @export
#'
#' @examples
#' is_col_dark("#0095A8FF")
#'
is_col_dark <- function(col) {
  # Handle NULL or missing input
  if (rlang::is_null(col) || length(col) == 0) {
    return(FALSE)
  }

  # Calculate luminance of the colour
  col_luminance <- farver::get_channel(
    colour = col,
    channel = "l",
    space = "hcl"
  )

  # Return TRUE if low luminance
  col_luminance <= 50
}


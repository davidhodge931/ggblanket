#' Combine individual aesthetics with mapping argument
#'
#' This function handles the mapping argument which can contain multiple aes() objects.
#' We use a defuse-and-inject approach to properly handle nested aes objects.
#'
#' @param individual_aesthetics List of quosures from individual arguments
#' @param mapping Additional aes() mapping that may contain multiple aes objects
#' @return Combined aesthetic mapping
#' @keywords internal
combine_aesthetics <- function(individual_aesthetics, mapping) {
  base_mapping <- if (length(individual_aesthetics) > 0) {
    rlang::inject(ggplot2::aes(!!!individual_aesthetics))
  } else {
    ggplot2::aes()
  }

  if (rlang::is_null(mapping)) return(base_mapping) else {
    purrr::iwalk(mapping, \(x, name) base_mapping[[name]] <<- x)
    return(base_mapping)
  }
}

#' Separate fixed values from aesthetic mappings
#'
#' @param aesthetics Named list of quosures from rlang::enquos()
#' @param data Optional data frame to check for column names
#' @return List with two components:
#'   - fixed: Named list of literal values for layer parameters
#'   - mapped: Named list of quosures for aesthetic mappings
separate_fixed_and_mapped_aesthetics <- function(aesthetics, data = NULL) {

  # Aesthetic names that accept single values
  value_aesthetics <- c("col", "colour", "fill", "shape", "linetype",
                        "linewidth", "size", "alpha", "stroke")

  # Helper to check if quosure evaluates to a single value
  is_single_value <- function(quo, name) {
    # Literal values are always fixed
    if (rlang::is_syntactic_literal(rlang::quo_get_expr(quo))) return(TRUE)

    # Symbols: check if column name first, then try to evaluate
    if (rlang::is_symbol(rlang::quo_get_expr(quo))) {
      symbol_name <- rlang::as_name(rlang::quo_get_expr(quo))
      if (!rlang::is_null(data) && symbol_name %in% names(data)) return(FALSE)
    }

    # Try to evaluate - if it's a single value, it's fixed
    if (name %in% value_aesthetics) {
      tryCatch({
        val <- rlang::eval_tidy(quo)
        length(val) == 1 && (is.atomic(val) || is.na(val))
      }, error = function(e) FALSE)
    } else {
      FALSE
    }
  }

  # Partition into fixed and mapped
  is_fixed <- purrr::imap_lgl(aesthetics, is_single_value)

  list(
    fixed = aesthetics[is_fixed] |> purrr::map(rlang::eval_tidy),
    mapped = aesthetics[!is_fixed]
  )
}

#' Identify Scale from Built ggplot Object
#'
#' Extracts scale type information (continuous, discrete, binned) from a built
#' ggplot2 object for x, y, colour, and fill aesthetics.
#'
#' @param built A built ggplot object, typically from [ggplot2::ggplot_build()]
#'
#' @return A named list containing scale information for available aesthetics.
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' built1 <- ggplot_build(p1)
#' identify(built1)
#'
#' @export
identify <- function(built) {
  scales_info <- list()

  extract_scale_info <- function(scale) {
    class_name <- class(scale)[1]

    type <- if (stringr::str_detect(class_name, "Continuous")) {
      "continuous"
    } else if (stringr::str_detect(class_name, "Binned")) {
      "binned"
    } else if (stringr::str_detect(class_name, "Discrete")) {
      "discrete"
    } else {
      NA_character_
    }

    temporal <- if (stringr::str_detect(class_name, "Datetime")) {
      "datetime"
    } else if (stringr::str_detect(class_name, "Date")) {
      "date"
    } else {
      NA_character_
    }

    list(
      class = class_name,
      type = type,
      temporal = temporal,
      limits = scale$get_limits()
    )
  }

  if (length(built$layout$panel_scales_x) > 0) {
    scales_info$x <- extract_scale_info(built$layout$panel_scales_x[[1]])
  }

  if (length(built$layout$panel_scales_y) > 0) {
    scales_info$y <- extract_scale_info(built$layout$panel_scales_y[[1]])
  }

  for (i in seq_along(built$plot$scales$scales)) {
    scale <- built$plot$scales$scales[[i]]

    if ("colour" %in% scale$aesthetics) {
      scales_info$colour <- extract_scale_info(scale)
    }

    if ("fill" %in% scale$aesthetics) {
      scales_info$fill <- extract_scale_info(scale)
    }
  }

  return(scales_info)
}

#' Get expand via limits with limits of 0 also having equivalent 0 expand
#' @param limits The limits of a built ggplot2 object
#' @return A expansion vector
#' @keywords internal
get_expand <- function(limits) {
  mult_lower <- if (limits[1] == 0) 0 else 0.05
  mult_upper <- if (limits[2] == 0) 0 else 0.05

  expand <- ggplot2::expansion(mult = c(mult_lower, mult_upper))
  return(expand)
}

#' Get label function based on scale temporal
#' @param stat A stat
#' @param temporal Scale temporal (e.g., "date", "datetime", "time", or NA)
#' @return A scales label function
#' @keywords internal
get_labels <- function(stat, temporal) {
  if (is_stat_sf(stat)) labels <- ggplot2::waiver()
  else {
    if (is.na(temporal)) {
      labels <- scales::label_comma()
    }
    else {
      labels <- switch(temporal,
                       date = scales::label_date_short(),
                       datetime = scales::label_date_short(),
                       time = scales::label_time(),
                       scales::label_comma()
      )
    }
  }
  return(labels)
}

#' Get transform function based on scale temporal
#' @param temporal Scale temporal (e.g., "date", "datetime", "time", or NA)
#' @return A scales transform function
#' @keywords internal
get_transform <- function(temporal) {
  if (is.na(temporal)) {
    transform <- scales::transform_identity()
  }
  else {
    transform <- switch(temporal,
                        date = scales::transform_date(),
                        datetime = scales::transform_time(),
                        time = scales::transform_hms(),
                        scales::transform_identity()
    )
  }

  return(transform)
}

#' Get scale limits_continuous for a discrete scale
#' @param limits Scale limits
#' @return A scale limits_continuous
#' @keywords internal
get_limits_continuous <- function(limits) {
  if (!is.null(limits)) {
    if (is.vector(limits)) {
      if (is.numeric(limits)) {
        limits_continuous <- limits
      }
      else {
        limits_continuous <- NULL
      }
    } else {
      limits_continuous <- NULL
    }
  }
  else {
    limits_continuous <- NULL
  }

  return(limits_continuous)
}

#' Get scale limits for a discrete scale
#' @param limits Scale limits
#' @return A scale limits
#' @keywords internal
get_limits <- function(limits) {
  if (!is.null(limits)) {
    if (is.vector(limits)) {
      if (is.numeric(limits)) {
        limits <- NULL
      }
    }
  }
  return(limits)
}

#' Is the stat sf or sf_coordinates
#' @param stat_str A stat string e.g. "sf"
#' @keywords internal
is_stat_sf <- function(stat_str) {
  stat_str %in% c("sf", "sf_coordinates")
}

#' Get coord
#' @param stat_str A stat
#' @param coord_xlim Zoom limits for the x axis.
#' @param coord_ylim Zoom limits for the y axis.
#' @param coord_clip Drawing clipped to the panel. "on" or "off".
#' @param coord_reverse Which direction to reverse. "none", "x", "y" or "xy".
#' @param coord_ratio Aspect ratio, expressed as y / x.
#'
#' @return A coord
#' @keywords internal
get_coord <- function(stat_str, coord_xlim, coord_ylim, coord_clip, coord_reverse,
                      coord_ratio) {

  if (is_stat_sf(stat_str)) {
    coord <- ggplot2::coord_sf(
      xlim = coord_xlim,
      ylim = coord_ylim,
      clip = coord_clip,
      reverse = coord_reverse,
      ratio = coord_ratio
    )
  }
  else {
    coord <- ggplot2::coord_cartesian(
      xlim = coord_xlim,
      ylim = coord_ylim,
      clip = coord_clip,
      reverse = coord_reverse,
      ratio = coord_ratio
    )
  }

  return(coord)
}

#' Modify theme elements for a specific axis aspect
#'
#' @param aspect Axis aspect to modify: `"x"` modifies y-axis and x-grid elements,
#'   `"y"` modifies x-axis and y-grid elements.
#' @param aspect_axis_line Treatment for axis lines: `"transparent"` makes them
#'   invisible but present, `"keep"` leaves them unchanged, `"blank"` removes them.
#' @param aspect_axis_ticks Treatment for axis ticks.
#' @param aspect_panel_grid Treatment for panel grid lines.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' library(ggplot2)
#'
#' # Hide y-axis lines and x-grid
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_to_aspect("x", aspect_axis_line = "blank", aspect_panel_grid = "blank")
#'
#' @export
theme_to_aspect <- function(
    aspect = c("x", "y"),
    aspect_axis_line = NULL,
    aspect_axis_ticks = NULL,
    aspect_panel_grid = NULL) {

  aspect_axis_line <- aspect_axis_line %||% getOption("ggblanket.aspect_axis_line") %||% "transparent"
  aspect_axis_ticks <- aspect_axis_ticks %||% getOption("ggblanket.aspect_axis_ticks") %||% "transparent"
  aspect_panel_grid <- aspect_panel_grid %||% getOption("ggblanket.aspect_panel_grid") %||% "transparent"

  # Validate inputs
  aspect <- rlang::arg_match(aspect, values = c("x", "y"))
  aspect_axis_line <- rlang::arg_match(aspect_axis_line, values = c("transparent", "keep", "blank"))
  aspect_axis_ticks <- rlang::arg_match(aspect_axis_ticks, values = c("transparent", "keep", "blank"))
  aspect_panel_grid <- rlang::arg_match(aspect_panel_grid, values = c("transparent", "keep", "blank"))

  theme <- ggplot2::theme()

  if (aspect == "x") {
    # For x aspect, modify y-axis elements and x-grid elements

    # Axis lines (y-axis)
    if (aspect_axis_line == "transparent") {
      theme <- theme +
        ggplot2::theme(
          axis.line.y.left = ggplot2::element_line(colour = "transparent"),
          axis.line.y.right = ggplot2::element_line(colour = "transparent")
        )
    } else if (aspect_axis_line == "blank") {
      theme <- theme +
        ggplot2::theme(
          axis.line.y.left = ggplot2::element_blank(),
          axis.line.y.right = ggplot2::element_blank()
        )
    }
    # "keep" means no modification

    # Axis ticks (y-axis)
    if (aspect_axis_ticks == "transparent") {
      theme <- theme +
        ggplot2::theme(
          axis.ticks.y.left = ggplot2::element_line(colour = "transparent"),
          axis.ticks.y.right = ggplot2::element_line(colour = "transparent"),
          axis.minor.ticks.y.left = ggplot2::element_line(colour = "transparent"),
          axis.minor.ticks.y.right = ggplot2::element_line(colour = "transparent")
        )
    } else if (aspect_axis_ticks == "blank") {
      theme <- theme +
        ggplot2::theme(
          axis.ticks.y.left = ggplot2::element_blank(),
          axis.ticks.y.right = ggplot2::element_blank(),
          axis.minor.ticks.y.left = ggplot2::element_blank(),
          axis.minor.ticks.y.right = ggplot2::element_blank()
        )
    }

    # Panel grid (x-grid)
    if (aspect_panel_grid == "transparent") {
      theme <- theme +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
          panel.grid.minor.x = ggplot2::element_line(colour = "transparent")
        )
    } else if (aspect_panel_grid == "blank") {
      theme <- theme +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
    }
  }
  else if (aspect == "y") {
    # For y aspect, modify x-axis elements and y-grid elements

    # Axis lines (x-axis)
    if (aspect_axis_line == "transparent") {
      theme <- theme +
        ggplot2::theme(
          axis.line.x.bottom = ggplot2::element_line(colour = "transparent"),
          axis.line.x.top = ggplot2::element_line(colour = "transparent")
        )
    } else if (aspect_axis_line == "blank") {
      theme <- theme +
        ggplot2::theme(
          axis.line.x.bottom = ggplot2::element_blank(),
          axis.line.x.top = ggplot2::element_blank()
        )
    }

    # Axis ticks (x-axis)
    if (aspect_axis_ticks == "transparent") {
      theme <- theme +
        ggplot2::theme(
          axis.ticks.x.bottom = ggplot2::element_line(colour = "transparent"),
          axis.ticks.x.top = ggplot2::element_line(colour = "transparent"),
          axis.minor.ticks.x.bottom = ggplot2::element_line(colour = "transparent"),
          axis.minor.ticks.x.top = ggplot2::element_line(colour = "transparent")
        )
    } else if (aspect_axis_ticks == "blank") {
      theme <- theme +
        ggplot2::theme(
          axis.ticks.x.bottom = ggplot2::element_blank(),
          axis.ticks.x.top = ggplot2::element_blank(),
          axis.minor.ticks.x.bottom = ggplot2::element_blank(),
          axis.minor.ticks.x.top = ggplot2::element_blank()
        )
    }

    # Panel grid (y-grid)
    if (aspect_panel_grid == "transparent") {
      theme <- theme +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
          panel.grid.minor.y = ggplot2::element_line(colour = "transparent")
        )
    } else if (aspect_panel_grid == "blank") {
      theme <- theme +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
    }
  }

  return(theme)
}

#' Determine plot aspect from scale types and flipped aesthetics
#'
#' Returns "y" if the plot has a horizontal orientation. This can be determined by:
#' - Non-discrete x with discrete y scales
#' - Presence of flipped_aes = TRUE in any data layer
#'
#' @param x_type Scale type for x-axis: "discrete", "continuous", or "binned"
#' @param y_type Scale type for y-axis: "discrete", "continuous", or "binned"
#' @param built A built ggplot object from ggplot_build(). Optional.
#'
#' @return "y" for horizontal orientation, "x" otherwise
#'
#' @noRd
#' @keywords internal
get_aspect <- function(x_type, y_type, built = NULL) {

  # First check if any layer has flipped aesthetics
  if (!is.null(built)) {
    for (i in seq_along(built$data)) {
      layer_data <- built$data[[i]]
      if ("flipped_aes" %in% names(layer_data)) {
        if (any(layer_data$flipped_aes, na.rm = TRUE)) {
          return("y")
        }
      }
    }
  }

  # Fall back to scale type detection
  if (x_type == "discrete" & y_type %in% c("continuous", "binned")) {
    aspect <- "x"
  } else if (x_type %in% c("continuous", "binned") & y_type == "discrete") {
    aspect <- "y"
  } else {
    aspect <- "x"
  }

  return(aspect)
}

#' Check if geom is a border-type geom
#'
#' Border geoms are those that typically have both fill and colour aesthetics,
#' representing shapes with borders. Point geoms with shapes 21-25 are also
#' considered border geoms.
#'
#' @param geom_str A character string (e.g., "rect").
#' @param shape Optional shape parameter for point geoms. If provided and the geom
#'   is a point-type geom, shapes 21-25 will be considered border geoms.
#'
#' @return Logical indicating if the geom is a border-type geom
#'
#' @keywords internal
is_geom_border <- function(geom_str, shape = NULL) {
  # Validate input
  if (!is.character(geom_str) || length(geom_str) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg geom_str} specification",
      "i" = "{.arg geom_str} must be a single character string or geom_str object",
      "x" = "Got {.cls {class(geom_str)}} of length {length(geom_str)}"
    ))
  }

  # Define polygon/area border geom_strs
  border_polygons <- c(
    "area",
    "bar",
    "boxplot",
    "col",
    "crossbar",
    "density",
    "map",
    "polygon",
    "rect",
    "ribbon",
    "sf",
    "smooth",
    "tile",
    "violin",
    "raster",
    "contour_filled",
    "density2d_filled",
    "bin2d",
    "hex",
    "star"  # extension
  )

  # Define point geom_strs that can be border based on shape
  border_points <- c("point", "pointrange", "dotplot")

  # Check if it's a polygon border geom_str
  is_border_polygon <- geom_str %in% border_polygons

  # Check if it's a point border geom_str with appropriate shape
  is_border_point <- geom_str %in% border_points &&
    !rlang::is_null(shape) &&
    shape %in% 21:25

  is_border_polygon || is_border_point
}

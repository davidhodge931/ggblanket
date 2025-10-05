#' Handle the col aesthetic mapping to colour and fill
#' @param aesthetics Named list of quosures
#' @return Modified list of aesthetics
#' @keywords internal
handle_col_aesthetic <- function(aesthetics) {

  # If col is provided, use it for colour and fill unless overridden
  if ("col" %in% names(aesthetics)) {
    col_value <- aesthetics[["col"]]

    # Set colour if not explicitly provided
    if (!"colour" %in% names(aesthetics)) {
      aesthetics[["colour"]] <- col_value
    }

    # Set fill if not explicitly provided
    if (!"fill" %in% names(aesthetics)) {
      aesthetics[["fill"]] <- col_value
    }

    # Remove the col aesthetic
    aesthetics[["col"]] <- NULL
  }

  return(aesthetics)
}

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

  # Create base mapping from individual aesthetics
  if (length(individual_aesthetics) > 0) {
    base_mapping <- rlang::inject(ggplot2::aes(!!!individual_aesthetics))
  } else {
    base_mapping <- ggplot2::aes()
  }

  # If no additional mapping, return base
  if (is.null(mapping)) {
    return(base_mapping)
  }

  # Validate mapping is an aes object
  if (!inherits(mapping, "uneval")) {
    cli::cli_abort(c(
      "Invalid {.arg mapping} argument",
      "i" = "{.arg mapping} must be created with {.fn aes}",
      "x" = "Got {.cls {class(mapping)}}"
    ))
  }

  # Simple approach: just combine the mappings directly
  # ggplot2's aes() function should handle nested aes objects automatically
  combined <- base_mapping

  # Add or override aesthetics from mapping
  for (aesthetic_name in names(mapping)) {
    combined[[aesthetic_name]] <- mapping[[aesthetic_name]]
  }

  return(combined)
}

#' Resolve geom specification to ggplot2 Geom object
#' @param geom Either character string or Geom object
#' @return A ggplot2 Geom object
#' @keywords internal
resolve_geom <- function(geom) {
  if (inherits(geom, "Geom")) {
    return(geom)
  }

  if (!is.character(geom) || length(geom) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg geom} specification",
      "i" = "{.arg geom} must be a single character string or Geom object",
      "x" = "Got {.cls {class(geom)}} of length {length(geom)}"
    ))
  }

  # Handle special cases
  if (geom == "blank") {
    return(ggplot2::GeomBlank)
  }

  # Try to find the geom by name
  geom_name <- paste0("Geom", tools::toTitleCase(geom))

  if (exists(geom_name, envir = asNamespace("ggplot2"), inherits = FALSE)) {
    return(get(geom_name, envir = asNamespace("ggplot2")))
  }

  # Better error with suggestions
  cli::cli_abort(c(
    "Unknown {.arg geom}: {.val {geom}}",
    "i" = "Use either a character like {.val point} or object like {.code ggplot2::GeomPoint}"
  ))
}

#' Resolve stat specification to ggplot2 Stat object
#' @param stat Either character string or Stat object
#' @return A ggplot2 Stat object
#' @keywords internal
resolve_stat <- function(stat) {
  if (inherits(stat, "Stat")) {
    return(stat)
  }

  if (!is.character(stat) || length(stat) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg stat} specification",
      "i" = "{.arg stat} must be a single character string or Stat object",
      "x" = "Got {.cls {class(stat)}} of length {length(stat)}"
    ))
  }

  # Handle special cases
  if (stat == "identity") {
    return(ggplot2::StatIdentity)
  }

  # Try to find the stat by name
  stat_name <- paste0("Stat", tools::toTitleCase(stat))

  if (exists(stat_name, envir = asNamespace("ggplot2"), inherits = FALSE)) {
    return(get(stat_name, envir = asNamespace("ggplot2")))
  }

  cli::cli_abort(c(
    "Unknown {.arg stat}: {.val {stat}}",
    "i" = "Use either a character like {.val identity} or object like {.code ggplot2::StatIdentity}"
  ))
}

#' Resolve position specification to ggplot2 Position object
#' @param position Either function call, Position object, or character
#' @return A ggplot2 Position object
#' @keywords internal
resolve_position <- function(position) {
  if (inherits(position, "Position")) {
    return(position)
  }

  if (is.function(position)) {
    # If it's a function, call it to get the Position object
    result <- position()
    if (!inherits(result, "Position")) {
      cli::cli_abort(c(
        "Position function did not return a Position object",
        "x" = "Got {.cls {class(result)}}"
      ))
    }
    return(result)
  }

  if (is.character(position) && length(position) == 1) {
    # Handle character specification
    position_func_name <- paste0("position_", position)

    if (exists(position_func_name, envir = asNamespace("ggplot2"), inherits = FALSE)) {
      position_func <- get(position_func_name, envir = asNamespace("ggplot2"))
      return(position_func())
    }

    cli::cli_abort(c(
      "Unknown {.arg position}: {.val {position}}",
      "i" = "Use either a function like {.code ggplot2::position_identity()} or character"
    ))
  }

  cli::cli_abort(c(
    "Invalid {.arg position} specification",
    "i" = "{.arg position} must be a function, Position object, or character string",
    "x" = "Got {.cls {class(position)}}"
  ))
}

#' Separate fixed values from aesthetic mappings
#'
#' This function takes a list of quosures (captured with enquos()) and separates
#' them into fixed parameter values and aesthetic mappings based on whether they
#' contain literal values or expressions/symbols.
#'
#' @param aesthetics Named list of quosures from rlang::enquos()
#' @return List with two components:
#'   - fixed: Named list of literal values for layer parameters
#'   - mapped: Named list of quosures for aesthetic mappings
#' @keywords internal
separate_fixed_and_mapped_aesthetics <- function(aesthetics) {
  fixed <- list()
  mapped <- list()

  for (name in names(aesthetics)) {
    quo <- aesthetics[[name]]

    # Get the expression from the quosure
    expr <- rlang::quo_get_expr(quo)

    # Check if it's a literal value (not a symbol or call)
    if (rlang::is_syntactic_literal(expr)) {
      # It's a constant like "red", 4, TRUE - treat as fixed value
      fixed[[name]] <- rlang::eval_tidy(quo)
    } else {
      # It's a symbol (column name) or expression - treat as aesthetic mapping
      mapped[[name]] <- quo
    }
  }

  return(list(fixed = fixed, mapped = mapped))
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
#' # Continuous scales
#' p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' built1 <- ggplot_build(p1)
#' identify_scale(built1)
#' # $x: type = "continuous", $y: type = "continuous"
#'
#' # Mixed scale types with colour
#' p2 <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, colour = hp)) + geom_point()
#' built2 <- ggplot_build(p2)
#' identify_scale(built2)
#' # $x: type = "discrete", $y: type = "continuous", $colour: type = "continuous"
#'
#' # Binned color scale
#' p3 <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
#'       geom_point() +
#'       scale_color_binned()
#' built3 <- ggplot_build(p3)
#' identify_scale(built3)
#' # $x: type = "continuous", $y: type = "continuous", $colour: type = "binned"
#'
#' # Fill scale
#' p4 <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) + geom_bar()
#' built4 <- ggplot_build(p4)
#' identify_scale(built4)
#' # $x: type = "discrete", $y: type = "continuous", $fill: type = "discrete"
#'
#' @export
identify_scale <- function(built) {
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
#' @param stat A stat
is_stat_sf <- function(stat) {
  identical(stat, ggplot2::StatSf) ||
    identical(stat, ggplot2::StatSfCoordinates) ||
    (is.character(stat) && stat %in% c("sf", "sf_coordinates"))
}

#' Get coord
#' @param stat A stat
#' @param aspect Either "x" or or "y"
#' @return A coord
#' @keywords internal
get_coord <- function(stat, aspect) {
  if (aspect == "y") reverse <- "y"
  else reverse <- "none"

  if (is_stat_sf(stat)) {
    coord <- ggplot2::coord_sf(clip = "off")
  }
  else {
    coord <- ggplot2::coord_cartesian(clip = "off", reverse = reverse)
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

#' Get the aesthetic aspect from a ggplot build object
#'
#' Determines whether the plot uses standard (x) or flipped (y) coordinate system
#' by checking if any layer has flipped aesthetics.
#'
#' @param built A ggplot build object (from `ggplot2::ggplot_build()`)
#'
#' @return A character string: "y" if any layer has flipped aesthetics, "x" otherwise
#'
#' @noRd
#' @keywords internal
get_aspect <- function(built) {
  flipped_aes <- FALSE

  for (layer_data in built$data) {
    if ("flipped_aes" %in% names(layer_data)) {
      if (any(layer_data[["flipped_aes"]])) {
        flipped_aes <- TRUE
        break
      }
    }
  }

  aspect <- if (flipped_aes) "y" else "x"
  return(aspect)
}

#' Check if geom is a border-type geom
#'
#' Border geoms are those that typically have both fill and colour aesthetics,
#' representing shapes with borders. Point geoms with shapes 21-25 are also
#' considered border geoms.
#'
#' @param geom Either a character string (e.g., "rect") or a ggproto Geom object
#'   (e.g., ggplot2::GeomRect)
#' @param shape Optional shape parameter for point geoms. If provided and the geom
#'   is a point-type geom, shapes 21-25 will be considered border geoms.
#'
#' @return Logical indicating if the geom is a border-type geom
#'
#' @keywords internal
is_geom_border <- function(geom, shape = NULL) {
  # Convert to character if it's a Geom object
  if (inherits(geom, "Geom")) {
    geom_class <- class(geom)[1]
    geom <- tolower(sub("^Geom", "", geom_class))
  }

  # Validate input
  if (!is.character(geom) || length(geom) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg geom} specification",
      "i" = "{.arg geom} must be a single character string or Geom object",
      "x" = "Got {.cls {class(geom)}} of length {length(geom)}"
    ))
  }

  # Define polygon/area border geoms
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

  # Define point geoms that can be border based on shape
  border_points <- c("point", "jitter", "count", "qq", "pointrange")

  # Check if it's a polygon border geom
  is_border_polygon <- geom %in% border_polygons

  # Check if it's a point border geom with appropriate shape
  is_border_point <- geom %in% border_points &&
    !rlang::is_null(shape) &&
    shape %in% 21:25

  is_border_polygon || is_border_point
}

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

  if (!rlang::is_null(mapping)) {
    all_mappings <- c(as.list(base_mapping), as.list(mapping))
    base_mapping <- do.call(ggplot2::aes, all_mappings)
  }

  return(base_mapping)
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
#' ggplot2 object for x, y, fill, colour, alpha, shape, linetype, linewidth,
#' and size aesthetics.
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
#' identify_scale(built1)
#'
#' # Example with shape
#' p2 <- ggplot(mtcars, aes(x = wt, y = mpg, shape = factor(cyl))) +
#'   geom_point()
#' built2 <- ggplot_build(p2)
#' identify_scale(built2)
#'
#' # Example with multiple aesthetics
#' p3 <- ggplot(mtcars, aes(x = wt, y = mpg, colour = hp, size = disp,
#'                          shape = factor(cyl))) +
#'   geom_point()
#' built3 <- ggplot_build(p3)
#' identify_scale(built3)
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
    subtype <- if (stringr::str_detect(class_name, "Datetime")) {
      "datetime"
    } else if (stringr::str_detect(class_name, "Date")) {
      "date"
    } else {
      NA_character_
    }
    list(
      class = class_name,
      type = type,
      subtype = subtype,
      limits = scale$get_limits()
    )
  }

  # Extract x and y scales
  if (length(built$layout$panel_scales_x) > 0) {
    scales_info$x <- extract_scale_info(built$layout$panel_scales_x[[1]])
  }
  if (length(built$layout$panel_scales_y) > 0) {
    scales_info$y <- extract_scale_info(built$layout$panel_scales_y[[1]])
  }

  # Extract other scales (fill, colour, alpha, shape, linetype, linewidth, size)
  aesthetics_to_extract <- c("fill", "colour", "alpha", "shape", "linetype",
                             "linewidth", "size")

  for (i in seq_along(built$plot$scales$scales)) {
    scale <- built$plot$scales$scales[[i]]

    for (aes in aesthetics_to_extract) {
      if (aes %in% scale$aesthetics) {
        scales_info[[aes]] <- extract_scale_info(scale)
      }
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

#' Get label function
#' @param coord_type Either "cartesian" or "sf".
#' @param subtype Scale subtype (e.g., "date", "datetime", "time", or NA)
#' @param aesthetic The aesthetic being labeled (e.g., "x", "y", "fill", "colour")
#' @return A scales label function
#' @keywords internal
get_labels <- function(coord_type, subtype, aesthetic = NULL) {
  # Only waive labels for x/y position scales with sf coordinates
  if (coord_type == "sf" && !is.null(aesthetic) && aesthetic %in% c("x", "y")) {
    return(ggplot2::waiver())
  }

  if (is.na(subtype)) {
    labels <- scales::label_number()
  }
  else {
    labels <- switch(subtype,
                     date = scales::label_date_short(leading = ""),
                     datetime = scales::label_date_short(leading = ""),
                     time = scales::label_time(),
                     scales::label_number()
    )
  }

  return(labels)
}

#' Get transform function based on scale subtype
#' @param subtype Scale subtype (e.g., "date", "datetime", "time", or NA)
#' @return A scales transform function
#' @keywords internal
get_transform <- function(subtype) {
  if (is.na(subtype)) {
    transform <- scales::transform_identity()
  }
  else {
    transform <- switch(subtype,
                        date = scales::transform_date(),
                        datetime = scales::transform_time(),
                        time = scales::transform_hms(),
                        scales::transform_identity()
    )
  }

  return(transform)
}

#' Get coord
#' @param coord_type Either "cartesian" or "sf".
#' @param coord_xlim Zoom limits for the x axis.
#' @param coord_ylim Zoom limits for the y axis.
#' @param coord_clip Drawing clipped to the panel. "on" or "off".
#' @param coord_reverse Which direction to reverse. "none", "x", "y" or "xy".
#' @param coord_ratio focus ratio, expressed as y / x.
#'
#' @return A coord
#' @keywords internal
get_coord <- function(coord_type, coord_xlim, coord_ylim, coord_clip, coord_reverse,
                      coord_ratio) {

  if (coord_type == "sf") {
    coord <- ggplot2::coord_sf(
      xlim = coord_xlim,
      ylim = coord_ylim,
      clip = coord_clip,
      reverse = coord_reverse
    )
  } else {
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

#' Determine plot focus from scale types and flipped aesthetics
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
get_focus <- function(x_type, y_type, built = NULL) {

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
    focus <- "x"
  } else if (x_type %in% c("continuous", "binned") & y_type == "discrete") {
    focus <- "y"
  } else {
    focus <- "x"
  }

  return(focus)
}

set_polish <- function(polish = NULL) {
  options("ggblanket.polish" = polish)
}

get_polish <- function() {
  getOption("ggblanket.polish")
}

set_border_colour <- function(border_colour = NULL) {
  options("ggblanket.border_colour" = border_colour)
}

get_border_colour <- function() {
  getOption("ggblanket.border_colour")
}

#' Get geom function and string identifier
#'
#' @param geom A geom specified as:
#'   - String: `"point"` or `"density_ridges"`
#'   - Function: `geom_point` or `ggridges::geom_density_ridges` (no parentheses)
#'
#' @return List with `fn` (geom function), `str` (geom name string), and `is_border_geom` (TRUE if both fill and colour are available)
#' @keywords internal
get_geom_info <- function(geom) {
  # Case 1: String input (e.g., "point" or "density_ridges")
  if (is.character(geom)) {
    geom_name <- paste0("geom_", geom)
    # Try ggplot2 first
    geom_fn <- tryCatch({
      getExportedValue("ggplot2", geom_name)
    }, error = function(e) NULL)
    if (!is.null(geom_fn)) {
      temp_result <- geom_fn()
      # Handle sf geoms (returns a list)
      if (is.list(temp_result) && !inherits(temp_result, "LayerInstance")) {
        temp_result <- temp_result[[1]]  # Get the layer from the list
      }
      geom_obj <- temp_result$geom  # Define geom_obj here!
      return(list(
        fn = geom_fn,
        str = geom,
        is_border_geom = "fill" %in% geom_obj$aesthetics() & "colour" %in% geom_obj$aesthetics()
      ))
    }
    # Search other loaded packages
    for (pkg in loadedNamespaces()) {
      if (pkg == "ggplot2") next
      geom_fn <- tryCatch({
        getExportedValue(pkg, geom_name)
      }, error = function(e) NULL)
      if (!is.null(geom_fn)) {
        temp_result <- geom_fn()
        # Handle sf geoms (returns a list)
        if (is.list(temp_result) && !inherits(temp_result, "LayerInstance")) {
          temp_result <- temp_result[[1]]
        }
        # Handle LayerInstance (e.g., ggstar::geom_star)
        if (inherits(temp_result, "LayerInstance")) {
          geom_obj <- temp_result$geom
          geom_str <- class(geom_obj)[1] |>
            stringr::str_remove("^Geom") |>
            snakecase::to_snake_case()
        } else {
          geom_obj <- temp_result$geom
          geom_str <- geom
        }
        return(list(
          fn = geom_fn,
          str = geom_str,
          is_border_geom = "fill" %in% geom_obj$aesthetics() & "colour" %in% geom_obj$aesthetics()
        ))
      }
    }
    stop("Geom '", geom_name, "' not found. Make sure the package is loaded.",
         call. = FALSE)
  }
  # Case 2: Already a Geom object (user called the function with ())
  if (inherits(geom, "Geom")) {
    stop("Use geom function without parentheses (e.g., geom_histogram not geom_histogram())",
         call. = FALSE)
  }
  # Case 3: Function input (e.g., geom_point or ggridges::geom_density_ridges)
  if (is.function(geom)) {
    temp_result <- geom()
    # Handle sf geoms (returns a list)
    if (is.list(temp_result) && !inherits(temp_result, "LayerInstance")) {
      temp_result <- temp_result[[1]]
    }
    # Handle LayerInstance (e.g., ggstar::geom_star)
    if (inherits(temp_result, "LayerInstance")) {
      geom_obj <- temp_result$geom
      geom_str <- class(geom_obj)[1] |>
        stringr::str_remove("^Geom") |>
        snakecase::to_snake_case()
    } else {
      # Standard Geom object
      geom_obj <- temp_result$geom
      geom_str <- class(geom_obj)[1] |>
        stringr::str_remove("^Geom") |>
        snakecase::to_snake_case()
    }
    return(list(
      fn = geom,
      str = geom_str,
      is_border_geom = "fill" %in% geom_obj$aesthetics() & "colour" %in% geom_obj$aesthetics()
    ))
  }
  stop("geom must be a string or function", call. = FALSE)
}

#' Convert discrete palette to function
#'
#' Ensures a discrete palette is a function. If already a function, returns as-is.
#' If a vector, wraps it in a function that subsets the first n values.
#'
#' @param palette A palette function, vector, NULL, or waiver
#' @return A palette function or NULL/waiver
#' @keywords internal
as_discrete_palette <- function(palette) {
  if (is.function(palette)) return(palette)
  if (is.null(palette) || inherits(palette, "waiver")) return(palette)
  if (is.atomic(palette)) {
    return(function(n) palette[seq_len(n)])
  }
  stop("palette must be a function, vector, or NULL", call. = FALSE)
}

#' Convert continuous palette to function
#'
#' Ensures a continuous palette is a function. If already a function, returns as-is.
#' If a character vector, uses scales::pal_gradient_n for color interpolation.
#' If a numeric vector, creates an interpolation function using rescale or approxfun.
#'
#' @param palette A palette function, vector, NULL, or waiver
#' @return A palette function or NULL/waiver
#' @keywords internal
as_continuous_palette <- function(palette) {
  if (is.function(palette)) return(palette)
  if (is.null(palette) || inherits(palette, "waiver")) return(palette)
  if (is.atomic(palette)) {
    if (is.character(palette)) {
      return(scales::pal_gradient_n(palette))
    } else if (is.numeric(palette)) {
      return(function(x) scales::rescale(x, to = palette, from = c(0, 1)))
    }
  }
  stop("palette must be a function, vector, or NULL", call. = FALSE)
}

#' Title
#'
#' @param geom
#' @param colour
#' @param linewidth
#'
#' @returns
#' @export
#'
#' @examples
as_border <- function(geom = NULL, colour = NULL, linewidth = NULL) {
    list(
      geom = geom,
      colour = colour,
      linewidth = linewidth
    )
}

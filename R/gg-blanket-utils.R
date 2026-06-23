#' Combine individual aesthetics with mapping argument
#'
#' This function handles the mapping argument which can contain multiple aes() objects.
#' We use a defuse-and-inject approach to properly handle nested aes objects.
#'
#' @param individual_aesthetics List of quosures from individual arguments
#' @param mapping Additional aes() mapping that may contain multiple aes objects
#' @return Combined aesthetic mapping
#' @noRd
combine_aesthetics <- function(individual_aesthetics, mapping) {
  base_mapping <- if (length(individual_aesthetics) > 0) {
    rlang::inject(ggplot2::aes(!!!individual_aesthetics))
  } else {
    ggplot2::aes()
  }

  if (!is.null(mapping)) {
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
#' @noRd
separate_fixed_and_mapped_aesthetics <- function(aesthetics, data = NULL) {
  # Aesthetic names that can accept single values (fixed) or be mapped
  value_aesthetics <- c(
    "colour",
    "fill",
    "alpha",
    "shape",
    "linetype",
    "linewidth",
    "size",
    "stroke",
    "width",
    "height",
    "angle",
    "radius",
    "label",
    "weight",
    "group",
    "slope",
    "intercept",
    "sample",
    "z"
  )

  # Position aesthetics should always be mapped (to affect scale limits)
  position_aesthetics <- c(
    "x",
    "xmin",
    "xmax",
    "xend",
    "xintercept",
    "y",
    "ymin",
    "ymax",
    "yend",
    "yintercept",
    "z"
  )

  # Helper to check if quosure evaluates to a single value
  is_single_value <- function(quo, name) {
    # Position aesthetics are ALWAYS mapped (even literals) to affect scales
    if (name %in% position_aesthetics) {
      return(FALSE)
    }

    # Literal values are always fixed
    if (rlang::is_syntactic_literal(rlang::quo_get_expr(quo))) {
      return(TRUE)
    }

    # Symbols: check if column name first, then try to evaluate
    if (rlang::is_symbol(rlang::quo_get_expr(quo))) {
      symbol_name <- rlang::as_name(rlang::quo_get_expr(quo))
      if (!is.null(data) && symbol_name %in% names(data)) return(FALSE)
    }

    # Try to evaluate - if it's a single value, it's fixed
    if (name %in% value_aesthetics) {
      tryCatch(
        {
          val <- rlang::eval_tidy(quo)
          length(val) == 1 && (is.atomic(val) || is.na(val))
        },
        error = function(e) FALSE
      )
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
#' @noRd
identify_scale <- function(built) {
  scales_info <- list()

  extract_scale_info <- function(scale) {
    class_name <- class(scale)[1]

    transform_name <- tryCatch(
      scale$get_transformation()$name,
      error = function(e) NA_character_
    )

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
    } else if (identical(transform_name, "hms")) {
      # detect via transform
      "time"
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
  aesthetics_to_extract <- c(
    "fill",
    "colour",
    "alpha",
    "shape",
    "linetype",
    "linewidth",
    "size"
  )

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
#' @noRd
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
#' @noRd
get_labels <- function(coord_type, subtype, aesthetic = NULL) {
  # Only waive labels for x/y position scales with sf coordinates
  if (coord_type == "sf" && !is.null(aesthetic) && aesthetic %in% c("x", "y")) {
    return(ggplot2::waiver())
  }

  if (is.na(subtype)) {
    labels <- scales::label_number()
  } else {
    labels <- switch(
      subtype,
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
#' @noRd
get_transform <- function(subtype) {
  if (is.na(subtype)) {
    transform <- scales::transform_identity()
  } else {
    transform <- switch(
      subtype,
      date = scales::transform_date(),
      datetime = scales::transform_time(),
      time = scales::transform_hms(),
      scales::transform_identity()
    )
  }

  return(transform)
}

#' Get coord
#'
#' @param coord_type Coordinate system type. Either `"cartesian"` (default), `"sf"`, or `"transform"`.
#' @param coord_clip Whether drawing is clipped to the panel. Either `"on"` (default) or `"off"`.
#' @param coord_ratio Aspect ratio expressed as y / x, for [ggplot2::coord_cartesian()].
#' @param coord_reverse Which axes to reverse. One of `"none"` (default), `"x"`, `"y"`, or `"xy"`.
#' @param coord_xlim,coord_ylim Zoom limits within the coordinate system.
#' @param ... Additional arguments passed to the underlying coord function.
#'
#' @return A ggplot2 coord object
#' @noRd
get_coord <- function(
  coord_type = "cartesian",
  coord_clip = NULL,
  coord_ratio = NULL,
  coord_reverse = "none",
  coord_xlim = NULL,
  coord_ylim = NULL,


  ...
) {
  if (coord_type == "sf") {
    ggplot2::coord_sf(
      xlim = coord_xlim,
      ylim = coord_ylim,
      clip = coord_clip,
      reverse = coord_reverse,
      ...
    )
  }
  else {
    ggplot2::coord_cartesian(
      xlim = coord_xlim,
      ylim = coord_ylim,
      clip = coord_clip,
      reverse = coord_reverse,
      ratio = coord_ratio,
      ...
    )
  }
}

#' Determine plot orientation from scale types and mapped aesthetics
#'
#' Returns "y" if the plot has a horizontal orientation. This can be determined by:
#' - Only y is mapped (and x is not)
#' - Non-discrete x with discrete y scales
#' - Presence of flipped_aes = TRUE in any data layer when y is discrete
#'
#' @param x_type Scale type for x-axis: "discrete", "continuous", or "binned"
#' @param y_type Scale type for y-axis: "discrete", "continuous", or "binned"
#' @param built A built ggplot object from ggplot_build(). Optional.
#' @param is_x_mapped Whether x is explicitly mapped by the user.
#' @param is_y_mapped Whether y is explicitly mapped by the user.
#'
#' @return "y" for horizontal orientation, "x" otherwise
#'
#' @noRd
get_orientation <- function(x_type, y_type, built = NULL, is_x_mapped = FALSE, is_y_mapped = FALSE) {

  # If only y is mapped, orientation is horizontal
  if (is_y_mapped && !is_x_mapped) return("y")

  # If only x is mapped, orientation is vertical
  if (is_x_mapped && !is_y_mapped) return("x")

  if (!is.null(built)) {
    if (y_type == "discrete") {
      for (i in seq_along(built$data)) {
        layer_data <- built$data[[i]]
        if ("flipped_aes" %in% names(layer_data)) {
          if (any(layer_data$flipped_aes, na.rm = TRUE)) {
            return("y")
          }
        }
      }
    }
  }

  if (x_type == "discrete" & y_type %in% c("continuous", "binned")) {
    orientation <- "x"
  } else if (x_type %in% c("continuous", "binned") & y_type == "discrete") {
    orientation <- "y"
  } else {
    orientation <- "x"
  }

  return(orientation)
}

set_refine <- function(refine = NULL) {
  options("ggblanket.refine" = refine)
}

get_refine <- function() {
  getOption("ggblanket.refine")
}

set_colour_border <- function(colour_border = NULL) {
  if (is.null(colour_border)) {
    colour_border <- \(x) {
      if (is.null(x)) return(x)
      if (is_panel_dark()) blends::screen(x) else blends::multiply(x)
    }
  }
  options("ggblanket.colour_border" = colour_border)
}

get_colour_border <- function() {
  getOption("ggblanket.colour_border")
}

set_fill_border <- function(fill_border = NULL) {
  options("ggblanket.fill_border" = fill_border)
}

get_fill_border <- function() {
  getOption("ggblanket.fill_border")
}

set_linewidth_border <- function(linewidth_border = NULL) {
  options("ggblanket.linewidth_border" = linewidth_border)
}

get_linewidth_border <- function() {
  getOption("ggblanket.linewidth_border")
}

set_stroke <- function(stroke = NULL) {
  options("ggblanket.stroke" = stroke)
}
get_stroke <- function() {
  getOption("ggblanket.stroke")
}

set_coord_clip <- function(coord_clip = "on") {
  options("ggblanket.coord_clip" = coord_clip)
}
get_coord_clip <- function() {
  getOption("ggblanket.coord_clip")
}

#' Get geom function and string identifier
#'
#' @param geom A geom specified as:
#'   - String: `"point"` or `"density_ridges"`
#'   - Function: `geom_point` or `ggridges::geom_density_ridges` (no parentheses)
#'
#' @return List with `fn` (geom function), `str` (geom name string),
#'   `is_border_geom` (TRUE if both fill and colour are available), and
#'   `is_stroke_geom` (TRUE if stroke is available).
#' @noRd
get_geom_info <- function(geom) {

  # Case 1: String input (e.g., "point" or "density_ridges")
  if (is.character(geom)) {
    geom_name <- paste0("geom_", geom)

    # Try ggplot2 first
    geom_fn <- tryCatch(
      getExportedValue("ggplot2", geom_name),
      error = function(e) NULL
    )

    if (!is.null(geom_fn)) {
      temp_result <- geom_fn()
      if (is.list(temp_result) && !inherits(temp_result, "LayerInstance")) {
        temp_result <- temp_result[[1]]
      }
      geom_obj <- temp_result$geom
      return(list(
        fn             = geom_fn,
        str            = geom,
        is_border_geom = "fill"   %in% geom_obj$aesthetics() &
          "colour" %in% geom_obj$aesthetics(),
        is_stroke_geom = "stroke" %in% geom_obj$aesthetics()
      ))
    }

    # Search other loaded packages
    for (pkg in loadedNamespaces()) {
      if (pkg == "ggplot2") next

      geom_fn <- tryCatch(
        getExportedValue(pkg, geom_name),
        error = function(e) NULL
      )

      if (!is.null(geom_fn)) {
        temp_result <- geom_fn()
        if (is.list(temp_result) && !inherits(temp_result, "LayerInstance")) {
          temp_result <- temp_result[[1]]
        }
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
          fn             = geom_fn,
          str            = geom_str,
          is_border_geom = "fill"   %in% geom_obj$aesthetics() &
            "colour" %in% geom_obj$aesthetics(),
          is_stroke_geom = "stroke" %in% geom_obj$aesthetics()
        ))
      }
    }

    stop(
      "Geom '", geom_name, "' not found. Make sure the package is loaded.",
      call. = FALSE
    )
  }

  # Case 2: Already a Geom object (user called the function with ())
  if (inherits(geom, "Geom")) {
    stop(
      "Use geom function without parentheses (e.g., geom_histogram not geom_histogram())",
      call. = FALSE
    )
  }

  # Case 3: Function input (e.g., geom_point or ggridges::geom_density_ridges)
  if (is.function(geom)) {
    temp_result <- geom()
    if (is.list(temp_result) && !inherits(temp_result, "LayerInstance")) {
      temp_result <- temp_result[[1]]
    }
    if (inherits(temp_result, "LayerInstance")) {
      geom_obj <- temp_result$geom
      geom_str <- class(geom_obj)[1] |>
        stringr::str_remove("^Geom") |>
        snakecase::to_snake_case()
    } else {
      geom_obj <- temp_result$geom
      geom_str <- class(geom_obj)[1] |>
        stringr::str_remove("^Geom") |>
        snakecase::to_snake_case()
    }
    return(list(
      fn             = geom,
      str            = geom_str,
      is_border_geom = "fill"   %in% geom_obj$aesthetics() &
        "colour" %in% geom_obj$aesthetics(),
      is_stroke_geom = "stroke" %in% geom_obj$aesthetics()
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
#' @noRd
as_discrete_palette <- function(palette) {
  if (is.function(palette)) palette
  else if (is.null(palette) || inherits(palette, "waiver")) palette
  else if (is.atomic(palette)) function(n) unname(palette[seq_len(n)])
  else stop("palette must be a function, vector, or NULL", call. = FALSE)
}

#' Convert continuous palette to function
#'
#' Ensures a continuous palette is a function. If already a function, returns as-is.
#' If a character vector, uses scales::pal_gradient_n for color interpolation.
#' If a numeric vector, creates an interpolation function using rescale or approxfun.
#'
#' @param palette A palette function, vector, NULL, or waiver
#' @return A palette function or NULL/waiver
#' @noRd
as_continuous_palette <- function(palette) {
  if (is.function(palette)) palette
  else if (is.null(palette) || inherits(palette, "waiver")) palette
  else if (is.atomic(palette)) {
    if (is.character(palette)) scales::pal_gradient_n(palette)
    else if (is.numeric(palette)) function(x) scales::rescale(x, to = palette, from = c(0, 1))
  }
  else stop("palette must be a function, vector, or NULL", call. = FALSE)
}

# Helper — compose a border function with a palette, regardless of type
apply_border_to_palette <- function(palette, border_fn) {
  if (is.function(palette)) {
    \(n) border_fn(palette(n))
  } else if (is.atomic(palette)) {
    result <- border_fn(palette)
    names(result) <- names(palette)  # preserve names through border transform
    result
  } else {
    palette
  }
}

#' Coerce a facet argument to a quosures object
#'
#' Ensures a facet argument is a `quosures` object suitable for passing to
#' [ggplot2::facet_wrap()] or [ggplot2::facet_grid()]. If already a `quosures`
#' object (e.g. from [ggplot2::vars()]), returns as-is. Otherwise wraps a bare
#' variable name in [ggplot2::vars()].
#'
#' @param x A bare variable name or a [ggplot2::vars()] expression.
#'
#' @return A `quosures` object.
#' @noRd
as_facet_vars <- function(x) {
  if (inherits(x, "quosures")) {
    # already vars(...)
    x
  } else {
    ggplot2::vars(!!rlang::enquo(x))
  } # bare name → wrap it
}

#' Get panel background luminance
#' @noRd
get_panel_luminance <- function(theme = NULL) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour
  panel_col <- ggplot2::calc_element(
    theme = theme,
    element = "panel.background"
  )@fill

  # If panel is transparent/NA, check plot background
  if (is_transparent_or_na(panel_col)) {
    plot_col <- ggplot2::calc_element(
      theme = theme,
      element = "plot.background"
    )@fill

    if (is_transparent_or_na(plot_col)) {
      return(100) # Default to light
    }

    panel_col <- plot_col
  }

  `get_colour_luminance`(panel_col)
}

#' Get colour luminance
#' @noRd
get_colour_luminance <- function(col) {
  if (is_transparent_or_na(col)) {
    return(100) # Default to light
  }

  tryCatch(
    {
      farver::get_channel(colour = col, channel = "l", space = "hcl")
    },
    error = function(e) {
      100 # Default to light if error
    }
  )
}

#' Check if a colour value is transparent or NA
#' @noRd
is_transparent_or_na <- function(col) {
  is.null(col) ||
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
#' @noRd
#'
is_panel_dark <- function(theme = ggplot2::get_theme()) {
  bg <- ggplot2::calc_element(theme = theme, element = "panel.background")
  if (is.null(bg) || inherits(bg, "element_blank")) {
    return(FALSE)
  }
  col <- bg@fill
  if (is.null(col) || is.na(col) || col == "transparent") return(FALSE)
  luminance <- farver::get_channel(col, "l", space = "hsl")
  luminance < 50
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
#' @noRd
#'
is_col_dark <- function(col) {
  # Handle NULL or missing input
  if (is.null(col) || length(col) == 0) {
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

# Helper — detect if palette is a named vector warranting scale_*_manual routing
is_named_palette <- function(palette) {
  is.atomic(palette) && !is.null(names(palette))
}

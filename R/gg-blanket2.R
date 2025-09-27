#' A modern wrapper that creates a complete ggplot with a layer, accepting all
#' possible aesthetics as unquoted arguments. The `col` aesthetic is mapped to
#' both colour and fill.
#'
#' @param data A data frame.
#' @param ... Arguments passed to [ggplot2::layer(params = list(...))].
#' @param geom A geom as either character ("point") or object (ggplot2::GeomPoint).
#' @param stat A stat as either character ("identity") or object (ggplot2::StatIdentity).
#' @param position A position as either function (ggplot2::position_identity()) or object (ggplot2::PositionIdentity).
#' @param x Variable mapped to x, or a set value.
#' @param xmin Variable mapped to xmin, or a set value.
#' @param xmax Variable mapped to xmax, or a set value.
#' @param xend Variable mapped to xend, or a set value.
#' @param y Variable mapped to y, or a set value.
#' @param ymin Variable mapped to ymin, or a set value.
#' @param ymax Variable mapped to ymax, or a set value.
#' @param yend Variable mapped to yend, or a set value.
#' @param z Variable mapped to z, or a set value.
#' @param col Variable mapped to both colour and fill, or a set value.
#' @param alpha Variable mapped to alpha, or a set value.
#' @param shape Variable mapped to shape, or a set value.
#' @param linetype Variable mapped to linetype, or a set value.
#' @param linewidth Variable mapped to linewidth, or a set value.
#' @param size Variable mapped to size, or a set value.
#' @param stroke Variable mapped to stroke, or a set value.
#' @param label Variable mapped to label, or a set value.
#' @param weight Variable mapped to weight, or a set value.
#' @param group Variable mapped to group, or a set value.
#' @param width Variable mapped to width, or a set value.
#' @param height Variable mapped to height, or a set value.
#' @param slope Variable mapped to slope, or a set value.
#' @param intercept Variable mapped to intercept, or a set value.
#' @param xintercept Variable mapped to xintercept, or a set value.
#' @param yintercept Variable mapped to yintercept, or a set value.
#' @param sample Variable mapped to sample, or a set value.
#' @param angle Variable mapped to angle, or a set value.
#' @param radius Variable mapped to radius, or a set value.
#' @param mapping Additional aesthetic mapping within a [ggplot2::aes] call.
#'
#' @return A complete ggplot2 object
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   gg_blanket(
#'     geom = "point",
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
#' @export
gg_blanket <- function(data,
                       ...,
                       geom = "blank",
                       stat = "identity",
                       position = ggplot2::position_identity(),
                       x = NULL,
                       xmin = NULL,
                       xmax = NULL,
                       xend = NULL,
                       y = NULL,
                       ymin = NULL,
                       ymax = NULL,
                       yend = NULL,
                       z = NULL,
                       col = NULL,
                       alpha = NULL,
                       shape = NULL,
                       linetype = NULL,
                       linewidth = NULL,
                       size = NULL,
                       stroke = NULL,
                       label = NULL,
                       weight = NULL,
                       group = NULL,
                       width = NULL,
                       height = NULL,
                       slope = NULL,
                       intercept = NULL,
                       xintercept = NULL,
                       yintercept = NULL,
                       sample = NULL,
                       angle = NULL,
                       radius = NULL,
                       mapping = NULL
) {

  # Capture all aesthetics using enquos for lazy evaluation
  aesthetics <- rlang::enquos(
    x = x,
    xmin = xmin,
    xmax = xmax,
    xend = xend,
    y = y,
    ymin = ymin,
    ymax = ymax,
    yend = yend,
    z = z,
    col = col,
    alpha = alpha,
    shape = shape,
    linetype = linetype,
    linewidth = linewidth,
    size = size,
    stroke = stroke,
    label = label,
    weight = weight,
    group = group,
    width = width,
    height = height,
    slope = slope,
    intercept = intercept,
    xintercept = xintercept,
    yintercept = yintercept,
    sample = sample,
    angle = angle,
    radius = radius,
    .ignore_empty = "all"  # Ignore missing arguments
  )

  # Remove NULL aesthetics (missing arguments)
  aesthetics <- purrr::discard(aesthetics, rlang::quo_is_missing)

  # Separate fixed values from aesthetic mappings
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics)

  # Handle col -> colour/fill mapping for mapped aesthetics
  final_aesthetics <- handle_col_aesthetic(separated$mapped)

  # Convert geom, stat, position to appropriate objects
  geom_obj <- resolve_geom(geom)
  stat_obj <- resolve_stat(stat)
  position_obj <- resolve_position(position)

  # Combine individual aesthetics with mapping argument
  final_mapping <- combine_aesthetics(final_aesthetics, mapping)

  # Capture additional parameters from ...
  additional_params <- rlang::list2(...)

  # Combine fixed aesthetic values with additional parameters
  all_params <- utils::modifyList(separated$fixed, additional_params)

  # Build the complete ggplot with layer
  ggplot2::ggplot(data, mapping = final_mapping) +
    ggplot2::layer(
      geom = geom_obj,
      stat = stat_obj,
      position = position_obj,
      params = all_params
    )
}

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

    # Remove the col aesthetic as ggplot doesn't recognize it
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

#' Expand nested aes objects within a mapping
#'
#' This handles the case where mapping = aes(my_base, my_colors) where
#' my_base and my_colors are themselves aes() objects.
#'
#' @param mapping An aes() object that may contain other aes objects
#' @return A flattened aes object
#' @keywords internal
expand_nested_aes <- function(mapping) {
  # Start with empty result
  expanded_elements <- list()

  # Get the calling environment (where the aes objects are defined)
  calling_env <- parent.frame(3)  # Go up through combine_aesthetics -> gg_blanket -> user

  # Process each element in the mapping
  for (i in seq_along(mapping)) {
    name <- names(mapping)[i]
    expr <- mapping[[i]]

    # Check if this is an unnamed argument that might be an aes object
    if (is.null(name) || name == "") {
      # Try to evaluate as an aes object
      tryCatch({
        evaluated <- eval(expr, envir = calling_env)

        if (inherits(evaluated, "uneval")) {
          # It's an aes object - merge all its aesthetics
          for (aes_name in names(evaluated)) {
            expanded_elements[[aes_name]] <- evaluated[[aes_name]]
          }
        } else {
          # Not an aes object, but what should we do with unnamed non-aes?
          warning("Unnamed argument in mapping that is not an aes() object")
        }
      }, error = function(e) {
        warning("Could not evaluate unnamed argument in mapping: ", as.character(expr))
      })
    } else {
      # Named argument - treat as regular aesthetic mapping
      expanded_elements[[name]] <- expr
    }
  }

  # If no elements were expanded, return the original mapping
  if (length(expanded_elements) == 0) {
    return(mapping)
  }

  # Convert back to aes structure
  class(expanded_elements) <- "uneval"
  return(expanded_elements)
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

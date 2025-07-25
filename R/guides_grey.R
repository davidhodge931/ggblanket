#' Override legend aesthetic colors
#'
#' @description
#' Override the color of legend elements for any aesthetic. Provides full control
#' over bordered geom appearance in legends.
#'
#' @param aesthetic Character string naming the aesthetic ("shape", "size", "alpha",
#'   "linetype", "linewidth", etc.)
#' @param col Base color for the legend elements. Defaults to "#8991A1".
#' @param colour Direct override for the colour aesthetic in the legend. If NULL,
#'   determined by `col` and border settings.
#' @param fill Direct override for the fill aesthetic in the legend. If NULL,
#'   determined by `col` and border settings.
#' @param bordered Logical. Whether to treat as a bordered geom. If NULL,
#'   automatically determined based on the aesthetic and current geom defaults.
#' @param bordered_colour_by Function to transform the base color for borders.
#'   Defaults to `col_multiply()` for light themes and `col_screen()` for dark themes.
#'   Set to NA to disable border color transformation.
#' @param bordered_fill_by Function to transform the base color for fills.
#'   Defaults to NULL (no transformation). Set to NA to explicitly disable.
#' @param ... Other arguments passed to [ggplot2::guide_legend()].
#'
#' @return A ggplot guides specification.
#' @export
guides_grey <- function(
    aesthetic,
    col = "#8991A1",
    colour = NULL,
    fill = NULL,
    bordered = NULL,
    bordered_colour_by = NULL,
    bordered_fill_by = NULL,
    ...
) {
  # Direct overrides take precedence
  if (!is.null(colour) || !is.null(fill)) {
    override_aes <- list()
    if (!is.null(colour)) override_aes$colour <- colour
    if (!is.null(fill)) override_aes$fill <- fill
  } else {
    # Determine if we should treat as bordered
    if (is.null(bordered)) {
      # Auto-detect based on aesthetic and geom defaults
      if (aesthetic %in% c("shape", "size", "alpha")) {
        shape <- ggplot2::get_geom_defaults("point")$shape
        bordered <- !is.null(shape) && shape %in% 21:25
      } else if (aesthetic == "linewidth") {
        # Linewidth often applies to bordered geoms
        bordered <- TRUE
      } else {
        # linetype and others typically not bordered
        bordered <- FALSE
      }
    }

    # Build override aesthetics
    if (bordered) {
      # Get border transformation functions if not provided
      if (is.null(bordered_colour_by)) {
        current_theme <- ggplot2::theme_get()
        bordered_colour_by <- if (is_panel_dark(theme = current_theme)) {
          col_screen
        } else {
          col_multiply
        }
      }

      # Apply transformations unless explicitly disabled (NA)
      if (is.function(bordered_colour_by)) {
        override_colour <- bordered_colour_by(col)
      } else if (!is.null(bordered_colour_by) && is.na(bordered_colour_by)) {
        override_colour <- col
      } else {
        override_colour <- bordered_colour_by
      }

      # Handle bordered_fill_by - check for NULL first
      if (is.null(bordered_fill_by)) {
        override_fill <- col
      } else if (is.na(bordered_fill_by)) {
        override_fill <- col
      } else if (is.function(bordered_fill_by)) {
        override_fill <- bordered_fill_by(col)
      } else {
        override_fill <- bordered_fill_by
      }

      override_aes <- list(colour = override_colour, fill = override_fill)
    } else {
      # Non-bordered: use col for both colour and fill
      override_aes <- list(colour = col, fill = col)
    }
  }

  # Create the guide
  guide_list <- list()
  guide_list[[aesthetic]] <- ggplot2::guide_legend(
    override.aes = override_aes,
    ...
  )

  do.call(ggplot2::guides, guide_list)
}

# Convenience wrapper functions for common use cases
#' @rdname guides_grey
#' @export
guides_shape_grey <- function(col = "#8991A1", ...) {
  guides_grey("shape", col = col, ...)
}

#' @rdname guides_grey
#' @export
guides_size_grey <- function(col = "#8991A1", ...) {
  guides_grey("size", col = col, ...)
}

#' @rdname guides_grey
#' @export
guides_alpha_grey <- function(col = "#8991A1", ...) {
  guides_grey("alpha", col = col, ...)
}

#' @rdname guides_grey
#' @export
guides_linetype_grey <- function(col = "#8991A1", ...) {
  guides_grey("linetype", col = col, ...)
}

#' @rdname guides_grey
#' @export
guides_linewidth_grey <- function(col = "#8991A1", ...) {
  guides_grey("linewidth", col = col, ...)
}

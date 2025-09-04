#' Override legend aesthetic colours
#'
#' @description
#' Override the colour/fill of legend elements for any aesthetic.
#'
#' @param aesthetic Character string naming the aesthetic (e.g. "alpha", "shape",
#'   "linetype", "linewidth", "size")
#' @param ... Arguments passed to [ggplot2::guide_legend()]. Require named arguments (and support trailing commas).
#' @param col Base hex for the legend colour/fill. Defaults to `slate` (i.e. `"#8991A1"`).
#' @param colour Direct override for the colour aesthetic in the legend. If NULL,
#'   determined by `col` and `colour_border_transform`.
#' @param fill Direct override for the fill aesthetic in the legend. If NULL,
#'   determined by `fill` and `fill_border_transform`.
#' @param border TRUE or FALSE of whether to treat as a border geom.
#' @param colour_border_transform Function to transform the colour and colour_palette for border geoms.
#' @param fill_border_transform Function to transform the fill and fill_palette for border geoms.
#'
#' @return A ggplot guides specification.
#' @export
guides_grey <- function(
    aesthetic,
    ...,
    col = slate,
    colour = NULL,
    fill = NULL,
    border = NULL,
    colour_border_transform = NULL,
    fill_border_transform = NULL
) {
  if (rlang::is_null(colour_border_transform)) {
    colour_border_transform <- getOption("ggblanket.colour_border_transform")
  }
  if (rlang::is_null(fill_border_transform)) {
    fill_border_transform <- getOption("ggblanket.fill_border_transform")
  }

  # Direct overrides take precedence
  if (!rlang::is_null(colour) || !rlang::is_null(fill)) {
    override_aes <- list()
    if (!rlang::is_null(colour)) {
      override_aes$colour <- colour
    }
    if (!rlang::is_null(fill)) override_aes$fill <- fill
  } else {
    # Determine if we should treat as border
    if (rlang::is_null(border)) {
      # Auto-detect based on aesthetic and geom defaults
      if (aesthetic %in% c("shape", "size", "alpha")) {
        shape <- ggplot2::get_geom_defaults("point")$shape
        border <- !rlang::is_null(shape) && shape %in% 21:25
      } else if (aesthetic == "linewidth") {
        # Linewidth often applies to border geoms
        border <- TRUE
      } else {
        # linetype and others typically not border
        border <- FALSE
      }
    }

    # Build override aesthetics
    if (border) {
      # Get border transformation functions if not provided
      if (rlang::is_null(colour_border_transform)) {
        current_theme <- ggplot2::theme_get()
        colour_border_transform <- if (is_panel_dark(theme = current_theme)) {
          blend_screen
        } else {
          blend_multiply
        }
      }

      # Apply transformations unless explicitly disabled (NA)
      if (is.function(colour_border_transform)) {
        override_colour <- colour_border_transform(col)
      } else if (
        !rlang::is_null(colour_border_transform) &&
          is.na(colour_border_transform)
      ) {
        override_colour <- col
      } else {
        override_colour <- colour_border_transform
      }

      # Handle fill_border_transform - check for NULL first
      if (rlang::is_null(fill_border_transform)) {
        override_fill <- col
      } else if (is.na(fill_border_transform)) {
        override_fill <- col
      } else if (is.function(fill_border_transform)) {
        override_fill <- fill_border_transform(col)
      } else {
        override_fill <- fill_border_transform
      }

      override_aes <- list(colour = override_colour, fill = override_fill)
    } else {
      # Non-border: use col for both colour and fill
      override_aes <- list(colour = col, fill = col)
    }
  }

  # Create the guide
  guide_list <- list()
  guide_list[[aesthetic]] <- ggplot2::guide_legend(
    override.aes = override_aes,
    ...
  )

  rlang::exec(ggplot2::guides, !!!guide_list)
}

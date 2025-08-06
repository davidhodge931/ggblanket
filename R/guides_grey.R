#' Override legend aesthetic colours
#'
#' @description
#' Override the colour/fill of legend elements for any aesthetic.
#'
#' @param aesthetic Character string naming the aesthetic (e.g. "alpha", "shape",
#'   "linetype", "linewidth", "size")
#' @param col Base hex for the legend colour/fill. Defaults to "#8991A1".
#' @param colour Direct override for the colour aesthetic in the legend. If NULL,
#'   determined by `col` and `border_colour_transform`.
#' @param fill Direct override for the fill aesthetic in the legend. If NULL,
#'   determined by `fill` and `border_fill_transform`.
#' @param border TRUE or FALSE of whether to treat as a border geom.
#' @param border_colour_transform Function to transform the colour and colour_palette for border geoms.
#' @param border_fill_transform Function to transform the fill and fill_palette for border geoms.
#' @param ... Other arguments passed to [ggplot2::guide_legend()].
#'
#' @return A ggplot guides specification.
#' @export
guides_grey <- function(
  aesthetic,
  col = "#8991A1",
  colour = NULL,
  fill = NULL,
  border = NULL,
  border_colour_transform = NULL,
  border_fill_transform = NULL,
  ...
) {
  if (rlang::is_null(border_colour_transform)) {
    border_colour_transform <- getOption("ggblanket.border_colour_transform")
  }
  if (rlang::is_null(border_fill_transform)) {
    border_fill_transform <- getOption("ggblanket.border_fill_transform")
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
      if (rlang::is_null(border_colour_transform)) {
        current_theme <- ggplot2::theme_get()
        border_colour_transform <- if (is_panel_dark(theme = current_theme)) {
          col_screen
        } else {
          col_multiply
        }
      }

      # Apply transformations unless explicitly disabled (NA)
      if (is.function(border_colour_transform)) {
        override_colour <- border_colour_transform(col)
      } else if (
        !rlang::is_null(border_colour_transform) &&
          is.na(border_colour_transform)
      ) {
        override_colour <- col
      } else {
        override_colour <- border_colour_transform
      }

      # Handle border_fill_transform - check for NULL first
      if (rlang::is_null(border_fill_transform)) {
        override_fill <- col
      } else if (is.na(border_fill_transform)) {
        override_fill <- col
      } else if (is.function(border_fill_transform)) {
        override_fill <- border_fill_transform(col)
      } else {
        override_fill <- border_fill_transform
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

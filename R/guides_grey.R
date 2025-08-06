#' Override legend aesthetic colours
#'
#' @description
#' Override the colour/fill of legend elements for any aesthetic.
#'
#' @param aesthetic Character string naming the aesthetic (e.g. "alpha", "shape",
#'   "linetype", "linewidth", "size")
#' @param col Base hex for the legend colour/fill. Defaults to "#8991A1".
#' @param colour Direct override for the colour aesthetic in the legend. If NULL,
#'   determined by `col` and `bordered_colour`.
#' @param fill Direct override for the fill aesthetic in the legend. If NULL,
#'   determined by `fill` and `bordered_fill`.
#' @param bordered TRUE or FALSE of whether to treat as a bordered geom.
#' @param bordered_colour Function to transform the colour and colour_palette for bordered geoms.
#' @param bordered_fill Function to transform the fill and fill_palette for bordered geoms.
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
    bordered_colour = NULL,
    bordered_fill = NULL,
    ...
) {

  if (rlang::is_null(bordered_colour)) bordered_colour <- getOption("ggblanket.bordered_colour")
  if (rlang::is_null(bordered_fill)) bordered_fill <- getOption("ggblanket.bordered_fill")

  # Direct overrides take precedence
  if (!rlang::is_null(colour) || !rlang::is_null(fill)) {
    override_aes <- list()
    if (!rlang::is_null(colour)) override_aes$colour <- colour
    if (!rlang::is_null(fill)) override_aes$fill <- fill
  } else {
    # Determine if we should treat as bordered
    if (rlang::is_null(bordered)) {
      # Auto-detect based on aesthetic and geom defaults
      if (aesthetic %in% c("shape", "size", "alpha")) {
        shape <- ggplot2::get_geom_defaults("point")$shape
        bordered <- !rlang::is_null(shape) && shape %in% 21:25
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
      if (rlang::is_null(bordered_colour)) {
        current_theme <- ggplot2::theme_get()
        bordered_colour <- if (is_panel_dark(theme = current_theme)) {
          col_screen
        } else {
          col_multiply
        }
      }

      # Apply transformations unless explicitly disabled (NA)
      if (is.function(bordered_colour)) {
        override_colour <- bordered_colour(col)
      } else if (!rlang::is_null(bordered_colour) && is.na(bordered_colour)) {
        override_colour <- col
      } else {
        override_colour <- bordered_colour
      }

      # Handle bordered_fill - check for NULL first
      if (rlang::is_null(bordered_fill)) {
        override_fill <- col
      } else if (is.na(bordered_fill)) {
        override_fill <- col
      } else if (is.function(bordered_fill)) {
        override_fill <- bordered_fill(col)
      } else {
        override_fill <- bordered_fill
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

  rlang::exec(ggplot2::guides, !!!guide_list)
}

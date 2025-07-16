#' #' Add discrete color scale
#' #' @noRd
#' add_col_scale_discrete <- function(
#'     plot, aes_list, data, plot_data, palettes,
#'     col_breaks, col_labels, col_drop, col_legend_ncol,
#'     col_legend_nrow, col_legend_rev, x_symmetric
#' ) {
#'   # Calculate number of colors needed
#'   col_n <- get_col_n(aes_list, data, plot_data)
#'
#'   # Process palettes
#'   colour_palette_processed <- process_discrete_palette(
#'     palettes$colour_palette, col_n
#'   )
#'
#'   fill_palette_processed <- process_discrete_palette(
#'     palettes$fill_palette, col_n
#'   )
#'
#'   # Handle x_symmetric reversal
#'   if (x_symmetric) {
#'     col_legend_rev <- !col_legend_rev
#'     if (!is.null(colour_palette_processed)) {
#'       colour_palette_processed <- rev(colour_palette_processed)
#'     }
#'     if (!is.null(fill_palette_processed)) {
#'       fill_palette_processed <- rev(fill_palette_processed)
#'     }
#'   }
#'
#'   # Apply colour scale
#'   if (!is.null(colour_palette_processed)) {
#'     if (is.vector(colour_palette_processed)) {
#'       plot <- plot +
#'         ggplot2::scale_colour_manual(
#'           values = colour_palette_processed,
#'           breaks = col_breaks,
#'           labels = col_labels,
#'           na.value = palettes$na_colour,
#'           drop = col_drop
#'         )
#'     } else if (is.function(colour_palette_processed)) {
#'       plot <- plot +
#'         ggplot2::discrete_scale(
#'           aesthetics = "colour",
#'           palette = colour_palette_processed,
#'           breaks = col_breaks,
#'           labels = col_labels,
#'           na.value = palettes$na_colour,
#'           drop = col_drop
#'         )
#'     }
#'   }
#'
#'   # Apply fill scale
#'   if (!is.null(fill_palette_processed)) {
#'     if (is.vector(fill_palette_processed)) {
#'       plot <- plot +
#'         ggplot2::scale_fill_manual(
#'           values = fill_palette_processed,
#'           breaks = col_breaks,
#'           labels = col_labels,
#'           na.value = palettes$na_fill,
#'           drop = col_drop
#'         )
#'     } else if (is.function(fill_palette_processed)) {
#'       plot <- plot +
#'         ggplot2::discrete_scale(
#'           aesthetics = "fill",
#'           palette = fill_palette_processed,
#'           breaks = col_breaks,
#'           labels = col_labels,
#'           na.value = palettes$na_fill,
#'           drop = col_drop
#'         )
#'     }
#'   }
#'
#'   # Add guides
#'   plot +
#'     ggplot2::guides(
#'       colour = ggplot2::guide_legend(
#'         reverse = col_legend_rev,
#'         ncol = col_legend_ncol,
#'         nrow = col_legend_nrow
#'       ),
#'       fill = ggplot2::guide_legend(
#'         reverse = col_legend_rev,
#'         ncol = col_legend_ncol,
#'         nrow = col_legend_nrow
#'       )
#'     )
#' }

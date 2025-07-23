#' #' Add color scales
#' #' @noRd
#' add_col_scale <- function(
#'     plot, geom, col_scale_class, border, aes_list, data, plot_data,
#'     plot_build, x_symmetric, col_breaks, col_breaks_n, col_drop,
#'     col_limits_include, col_labels, col_legend_ncol, col_legend_nrow,
#'     col_legend_rev, col_rescale, col_scale_type, col_transform,
#'     colour_palette_discrete, colour_palette_continuous, colour_palette_ordinal,
#'     colour_palette_na, fill_palette_discrete, fill_palette_continuous,
#'     fill_palette_ordinal, fill_palette_na
#' ) {
#'
#'   # Get NA colors with defaults
#'   na_colour <- colour_palette_na %||% "#CDC5BFFF"
#'   na_fill <- fill_palette_na %||% "#CDC5BFFF"
#'
#'   # Get transform and labels
#'   if (is.null(col_transform)) {
#'     # Special handling for color transforms
#'     if (!rlang::quo_is_null(aes_list$col) && col_scale_class == "continuous") {
#'       col_data <- rlang::eval_tidy(aes_list$col, data)
#'       if (inherits(col_data, "hms")) {
#'         col_transform <- "hms"
#'       } else {
#'         col_transform <- get_transform(NULL, col_scale_class)
#'       }
#'     } else {
#'       col_transform <- get_transform(NULL, col_scale_class)
#'     }
#'   }
#'   col_labels <- get_col_label(col_labels, col_scale_class, col_transform)
#'
#'   # Apply scales based on type
#'   if (col_scale_class == "discrete") {
#'     plot <- add_col_scale_discrete(
#'       plot, aes_list, data, plot_data,
#'       colour_palette_discrete, fill_palette_discrete,
#'       na_colour, na_fill,
#'       col_breaks, col_labels, col_drop, col_legend_ncol,
#'       col_legend_nrow, col_legend_rev, x_symmetric
#'     )
#'   } else if (col_scale_class %in% c("continuous", "date", "datetime", "time")) {
#'     plot <- add_col_scale_continuous(
#'       plot, colour_palette_continuous, fill_palette_continuous,
#'       na_colour, na_fill, border, col_breaks, col_breaks_n,
#'       col_labels, col_legend_rev, col_rescale, col_scale_type,
#'       col_transform
#'     )
#'   } else if (col_scale_class == "ordinal") {
#'     plot <- add_col_scale_ordinal(
#'       plot, aes_list, data, plot_data,
#'       colour_palette_ordinal, fill_palette_ordinal,
#'       na_colour, na_fill,
#'       col_breaks, col_labels, col_drop, col_legend_ncol,
#'       col_legend_nrow, col_legend_rev
#'     )
#'   }
#'
#'   # Handle guides for other aesthetics
#'   plot <- add_matching_aesthetic_guides(
#'     plot, plot_build, col_legend_rev,
#'     col_legend_ncol, col_legend_nrow
#'   )
#'
#'   # Expand limits if necessary
#'   if (!is.null(col_limits_include)) {
#'     plot <- plot + ggplot2::expand_limits(
#'       colour = col_limits_include,
#'       fill = col_limits_include
#'     )
#'   }
#'
#'   plot
#' }
#'
#' #' Add discrete color scale
#' #' @noRd
#' add_col_scale_discrete <- function(
#'     plot, aes_list, data, plot_data,
#'     colour_palette, fill_palette,
#'     na_colour, na_fill,
#'     col_breaks, col_labels, col_drop, col_legend_ncol,
#'     col_legend_nrow, col_legend_rev, x_symmetric
#' ) {
#'   # Calculate number of colors needed
#'   col_n <- get_col_n(aes_list, data, plot_data)
#'
#'   # Process palettes
#'   colour_palette_processed <- process_discrete_palette(
#'     colour_palette, col_n
#'   )
#'
#'   fill_palette_processed <- process_discrete_palette(
#'     fill_palette, col_n
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
#'           na.value = na_colour,
#'           drop = col_drop
#'         )
#'     } else if (is.function(colour_palette_processed)) {
#'       plot <- plot +
#'         ggplot2::discrete_scale(
#'           aesthetics = "colour",
#'           palette = colour_palette_processed,
#'           breaks = col_breaks,
#'           labels = col_labels,
#'           na.value = na_colour,
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
#'           na.value = na_fill,
#'           drop = col_drop
#'         )
#'     } else if (is.function(fill_palette_processed)) {
#'       plot <- plot +
#'         ggplot2::discrete_scale(
#'           aesthetics = "fill",
#'           palette = fill_palette_processed,
#'           breaks = col_breaks,
#'           labels = col_labels,
#'           na.value = na_fill,
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
#'
#' #' Add continuous color scale
#' #' @noRd
#' add_col_scale_continuous <- function(
#'     plot, colour_palette, fill_palette,
#'     na_colour, na_fill, border, col_breaks, col_breaks_n,
#'     col_labels, col_legend_rev, col_rescale, col_scale_type,
#'     col_transform
#' ) {
#'   # Process palette functions to get color vectors
#'   colour_palette_values <- process_continuous_palette(colour_palette)
#'   fill_palette_values <- process_continuous_palette(fill_palette)
#'
#'   # Apply scales
#'   if (col_scale_type == "gradient") {
#'     plot <- plot +
#'       ggplot2::scale_colour_gradientn(
#'         colours = colour_palette_values,
#'         values = col_rescale,
#'         breaks = col_breaks,
#'         n.breaks = col_breaks_n,
#'         labels = col_labels,
#'         transform = col_transform,
#'         oob = scales::oob_keep,
#'         na.value = na_colour
#'       ) +
#'       ggplot2::scale_fill_gradientn(
#'         colours = fill_palette_values,
#'         values = col_rescale,
#'         breaks = col_breaks,
#'         n.breaks = col_breaks_n,
#'         labels = col_labels,
#'         transform = col_transform,
#'         oob = scales::oob_keep,
#'         na.value = na_fill
#'       )
#'
#'     # Add guides
#'     if (border) {
#'       plot <- plot +
#'         ggplot2::guides(
#'           colour = ggplot2::guide_none(),
#'           fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
#'         )
#'     } else {
#'       plot <- plot +
#'         ggplot2::guides(
#'           colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
#'           fill = ggplot2::guide_none()
#'         )
#'     }
#'   } else if (col_scale_type == "steps") {
#'     plot <- plot +
#'       ggplot2::scale_colour_stepsn(
#'         colours = colour_palette_values,
#'         values = col_rescale,
#'         breaks = col_breaks,
#'         n.breaks = col_breaks_n,
#'         labels = col_labels,
#'         transform = col_transform,
#'         oob = scales::oob_keep,
#'         na.value = na_colour
#'       ) +
#'       ggplot2::scale_fill_stepsn(
#'         colours = fill_palette_values,
#'         values = col_rescale,
#'         breaks = col_breaks,
#'         n.breaks = col_breaks_n,
#'         labels = col_labels,
#'         transform = col_transform,
#'         oob = scales::oob_keep,
#'         na.value = na_fill
#'       )
#'
#'     # Add guides
#'     if (!identical(colour_palette, fill_palette)) {
#'       plot <- plot +
#'         ggplot2::guides(
#'           colour = ggplot2::guide_none(),
#'           fill = ggplot2::guide_coloursteps(
#'             reverse = col_legend_rev,
#'             theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
#'           )
#'         )
#'     } else {
#'       plot <- plot +
#'         ggplot2::guides(
#'           colour = ggplot2::guide_coloursteps(
#'             reverse = col_legend_rev,
#'             theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
#'           ),
#'           fill = ggplot2::guide_none()
#'         )
#'     }
#'   }
#'
#'   plot
#' }
#'
#' #' Add ordinal color scale
#' #' @noRd
#' add_col_scale_ordinal <- function(
#'     plot, aes_list, data, plot_data,
#'     colour_palette, fill_palette,
#'     na_colour, na_fill,
#'     col_breaks, col_labels, col_drop, col_legend_ncol,
#'     col_legend_nrow, col_legend_rev
#' ) {
#'   # Calculate number of colors needed
#'   col_n <- get_col_n(aes_list, data, plot_data)
#'
#'   # Always reverse legend for ordinal
#'   col_legend_rev <- !col_legend_rev
#'
#'   # Create wrapper functions for ordinal scales
#'   if (is.function(colour_palette)) {
#'     colour_palette_discrete <- create_ordinal_palette_wrapper(colour_palette)
#'
#'     plot <- plot +
#'       ggplot2::discrete_scale(
#'         aesthetics = "colour",
#'         palette = colour_palette_discrete,
#'         breaks = col_breaks,
#'         labels = col_labels,
#'         na.value = na_colour,
#'         drop = col_drop
#'       )
#'   }
#'
#'   if (is.function(fill_palette)) {
#'     fill_palette_discrete <- create_ordinal_palette_wrapper(fill_palette)
#'
#'     plot <- plot +
#'       ggplot2::discrete_scale(
#'         aesthetics = "fill",
#'         palette = fill_palette_discrete,
#'         breaks = col_breaks,
#'         labels = col_labels,
#'         na.value = na_fill,
#'         drop = col_drop
#'       )
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
#'
#' is_border <- function(geom, theme_defaults) {
#'   # Define which geoms are treated as border polygons
#'   border_polygons <- c(
#'     "area", "bar", "boxplot", "col", "crossbar", "density",
#'     "map", "polygon", "rect", "ribbon", "smooth", "sf", "tile",
#'     "violin", "raster", "contour_filled", "density2d_filled",
#'     "bin2d", "hex"
#'   )
#'
#'   # Define point geoms that can be border based on shape
#'   border_points <- c("point", "jitter", "count", "qq", "pointrange")
#'
#'   is_border_polygon <- geom %in% border_polygons
#'
#'   theme_defaults
#'
#'   is_border_point <- geom %in% border_points &&
#'     !is.null(theme_defaults$geom$pointshape) &&
#'     theme_defaults$geom$pointshape %in% 21:25
#'
#'   is_border <- is_border_polygon || is_border_point
#'
#'   return(is_border)
#' }
#'
#' # Title extraction functions ----
#'
#' #' Get title for an aesthetic
#' #' @description Extracts title from data label attribute, build label, or applies titles_case
#' #' @noRd
#' get_aes_title <- function(data, aes_quo, build_label, titles_case, default = NULL) {
#'   # Priority 1: Check for label attribute in data column
#'   if (!rlang::quo_is_null(aes_quo)) {
#'     data_col <- dplyr::pull(data, !!aes_quo)
#'     label_attr <- attr(data_col, "label")
#'     if (!is.null(label_attr)) {
#'       return(label_attr)
#'     }
#'   }
#'
#'   # Priority 2: Use build label with titles_case
#'   if (!is.null(build_label)) {
#'     return(titles_case(rlang::as_name(build_label[1])))
#'   }
#'
#'   # Priority 3: Return default
#'   default
#' }
#'
#' #' Get all titles for a plot
#' #' @description Centralizes all title extraction logic
#' #' @noRd
#' get_plot_titles <- function(
#'     data, aes_list, plot_build, titles_case, stat,
#'     x_title = NULL, y_title = NULL, col_title = NULL
#' ) {
#'   # Get x and y titles
#'   if (stringr::str_detect(stat, "sf")) {
#'     # SF plots don't need axis titles
#'     x_title <- x_title %||% ""
#'     y_title <- y_title %||% ""
#'   } else {
#'     x_title <- x_title %||% get_aes_title(
#'       data, aes_list$x, plot_build$plot$labels$x,
#'       titles_case, titles_case("x")
#'     )
#'
#'     y_title <- y_title %||% get_aes_title(
#'       data, aes_list$y, plot_build$plot$labels$y,
#'       titles_case, titles_case("y")
#'     )
#'   }
#'
#'   # Get col title (handles both colour and fill)
#'   if (is.null(col_title)) {
#'     if (!is.null(plot_build$plot$labels$colour)) {
#'       col_title <- get_aes_title(
#'         data, aes_list$col, plot_build$plot$labels$colour,
#'         titles_case, NULL
#'       )
#'     } else if (!is.null(plot_build$plot$labels$fill)) {
#'       col_title <- get_aes_title(
#'         data, aes_list$col, plot_build$plot$labels$fill,
#'         titles_case, NULL
#'       )
#'     }
#'   }
#'
#'   # Get other aesthetic titles
#'   other_aesthetics <- c("alpha", "shape", "size", "linewidth", "linetype", "stroke", "pattern")
#'   other_titles <- list()
#'
#'   for (aes in other_aesthetics) {
#'     label <- plot_build$plot$labels[[aes]]
#'     if (!is.null(label)) {
#'       # Check if this aesthetic is identical to col
#'       if (is_aes_identical_to_col(plot_build, aes)) {
#'         other_titles[[paste0(aes, "_title")]] <- col_title
#'       } else {
#'         other_titles[[paste0(aes, "_title")]] <- titles_case(rlang::as_name(label[1]))
#'       }
#'     } else {
#'       other_titles[[paste0(aes, "_title")]] <- NULL
#'     }
#'   }
#'
#'   # Return all titles
#'   c(
#'     list(
#'       x = x_title,
#'       y = y_title,
#'       colour = col_title,
#'       fill = col_title
#'     ),
#'     other_titles
#'   )
#' }

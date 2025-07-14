#' # gg_blanket_utils.R - Utility functions for gg_blanket
#'
#' # Core utility functions ----
#'
#' #' Extract geom, stat, and transform names
#' #' @noRd
#' get_ggproto_strings_list <- function(geom = NULL, stat = NULL, transform = NULL) {
#'   list(
#'     geom = extract_ggproto_name(geom, "Geom"),
#'     stat = extract_ggproto_name(stat, "Stat"),
#'     transform = extract_transform_name(transform)
#'   )
#' }
#'
#' #' Extract ggproto name helper
#' #' @noRd
#' extract_ggproto_name <- function(object, prefix) {
#'   if (ggplot2::is_ggproto(object)) {
#'     class(object)[1] |>
#'       stringr::str_remove(prefix) |>
#'       stringr::str_to_lower()
#'   } else if (is.character(object)) {
#'     object
#'   } else {
#'     NULL
#'   }
#' }
#'
#' #' Extract transform name
#' #' @noRd
#' extract_transform_name <- function(transform) {
#'   if (inherits(transform, "transform")) {
#'     transform$name |>
#'       stringr::str_remove("composition") |>
#'       stringr::str_remove_all("[()]") |>
#'       stringr::str_split(",") |>
#'       unlist()
#'   } else if (is.character(transform)) {
#'     transform
#'   } else {
#'     NULL
#'   }
#' }
#'
#' # Plot creation functions ----
#'
#' #' Create base ggplot with aesthetic mappings
#' #'
#' #' @param data A data frame or tibble.
#' #' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' #' @param mapping Additional aesthetic mappings
#' #'
#' #' @noRd
#' create_ggplot <- function(
#'     data,
#'     x = NULL,
#'     xmin = NULL,
#'     xmax = NULL,
#'     xend = NULL,
#'     y = NULL,
#'     ymin = NULL,
#'     ymax = NULL,
#'     yend = NULL,
#'     z = NULL,
#'     col = NULL,
#'     facet = NULL,
#'     facet2 = NULL,
#'     group = NULL,
#'     subgroup = NULL,
#'     sample = NULL,
#'     label = NULL,
#'     text = NULL,
#'     mapping = NULL
#' ) {
#'   # Quote all aesthetics
#'   aes_args <- list(
#'     x = rlang::enquo(x),
#'     y = rlang::enquo(y),
#'     col = rlang::enquo(col),
#'     xmin = rlang::enquo(xmin),
#'     xmax = rlang::enquo(xmax),
#'     xend = rlang::enquo(xend),
#'     ymin = rlang::enquo(ymin),
#'     ymax = rlang::enquo(ymax),
#'     yend = rlang::enquo(yend),
#'     z = rlang::enquo(z),
#'     group = rlang::enquo(group),
#'     subgroup = rlang::enquo(subgroup),
#'     sample = rlang::enquo(sample),
#'     label = rlang::enquo(label),
#'     text = rlang::enquo(text)
#'   )
#'
#'   # Build base aesthetics
#'   base_aes <- ggplot2::aes(
#'     xmin = !!aes_args$xmin,
#'     xmax = !!aes_args$xmax,
#'     xend = !!aes_args$xend,
#'     ymin = !!aes_args$ymin,
#'     ymax = !!aes_args$ymax,
#'     yend = !!aes_args$yend,
#'     z = !!aes_args$z,
#'     group = !!aes_args$group,
#'     subgroup = !!aes_args$subgroup,
#'     sample = !!aes_args$sample,
#'     label = !!aes_args$label,
#'     text = !!aes_args$text
#'   )
#'
#'   # Add x and y if provided
#'   if (!rlang::quo_is_null(aes_args$x)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(x = !!aes_args$x))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_args$y)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(y = !!aes_args$y))
#'   }
#'
#'   # Add col/fill if provided
#'   if (!rlang::quo_is_null(aes_args$col)) {
#'     base_aes <- utils::modifyList(
#'       base_aes,
#'       ggplot2::aes(col = !!aes_args$col, fill = !!aes_args$col)
#'     )
#'   }
#'
#'   # Merge with additional mapping
#'   final_aes <- if (!is.null(mapping)) {
#'     utils::modifyList(base_aes, mapping)
#'   } else {
#'     base_aes
#'   }
#'
#'   # Create plot
#'   data |>
#'     ggplot2::ggplot(mapping = final_aes)
#' }
#'
#' #' Get params based on geom type
#' #' @noRd
#' get_geom_params <- function(geom, ...) {
#'   geom_specific_params <- switch(
#'     geom,
#'     boxplot = list(
#'       median_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth),
#'       whisker_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth),
#'       staple_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth),
#'       outlier_gp = list(stroke = 0)
#'     ),
#'     crossbar = list(
#'       middle_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth)
#'     ),
#'     smooth = list(alpha = NA),
#'     list()  # default empty list
#'   )
#'
#'   rlang::list2(!!!geom_specific_params, ...)
#' }
#'
#' #' Add initial layer to plot
#' #' @noRd
#' add_initial_layer <- function(
#'     plot, geom, stat, position, params,
#'     show_legend, coord, blend
#' ) {
#'   # Determine if using sf
#'   is_sf <- stringr::str_detect(stat, "sf")
#'
#'   # Set default coord if not provided
#'   if (is.null(coord)) {
#'     coord <- if (is_sf) {
#'       ggplot2::coord_sf(clip = "off")
#'     } else {
#'       ggplot2::coord_cartesian(clip = "off")
#'     }
#'   }
#'
#'   # Create layer function
#'   layer_fn <- if (is_sf) ggplot2::layer_sf else ggplot2::layer
#'
#'   # Build layer
#'   layer_call <- layer_fn(
#'     geom = geom,
#'     stat = stat,
#'     position = position,
#'     params = params,
#'     show.legend = show_legend
#'   )
#'
#'   # Add layer with optional blending
#'   if (!is.null(blend)) {
#'     plot + layer_call |> ggblend::blend(blend = blend) + coord
#'   } else {
#'     plot + layer_call + coord
#'   }
#' }
#'
#' # Scale determination functions ----
#'
#' #' Determine scale types from plot build
#' #' @noRd
#' get_scale_class <- function(plot_build, aes_list, data) {
#'   # Extract scale names from plot
#'   plot_scales <- plot_build$plot$scales$scales |>
#'     purrr::map_chr(\(x) {
#'       call_name <- rlang::call_name(x[["call"]])
#'       if (is.null(call_name)) NA_character_ else call_name
#'     })
#'
#'   # Determine scale classes
#'   list(
#'     x_scale_class = determine_scale_class(plot_scales, "x"),
#'     y_scale_class = determine_scale_class(plot_scales, "y"),
#'     col_scale_class = determine_color_scale_class(plot_scales, aes_list$col, data)
#'   )
#' }
#'
#' #' Determine scale class for a given axis
#' #' @noRd
#' determine_scale_class <- function(plot_scales, axis) {
#'   scale_pattern <- paste0("scale_", axis, "_")
#'
#'   dplyr::case_when(
#'     any(stringr::str_detect(plot_scales, paste0(scale_pattern, "discrete"))) ~ "discrete",
#'     any(stringr::str_detect(plot_scales, paste0(scale_pattern, "date"))) ~ "date",
#'     any(stringr::str_detect(plot_scales, paste0(scale_pattern, "datetime"))) ~ "datetime",
#'     any(stringr::str_detect(plot_scales, paste0(scale_pattern, "time"))) ~ "time",
#'     any(stringr::str_detect(plot_scales, paste0(scale_pattern, "continuous"))) ~ "numeric",
#'     TRUE ~ "numeric"
#'   )
#' }
#'
#' #' Determine color scale class
#' #' @noRd
#' determine_color_scale_class <- function(plot_scales, col_quo, data) {
#'   # Check plot scales
#'   scale_class <- dplyr::case_when(
#'     any(plot_scales %in% c("scale_colour_discrete", "scale_fill_discrete")) ~ "discrete",
#'     any(plot_scales %in% c("scale_colour_ordinal", "scale_fill_ordinal")) ~ "ordinal",
#'     any(plot_scales %in% c("scale_colour_date", "scale_fill_date")) ~ "date",
#'     any(plot_scales %in% c("scale_colour_datetime", "scale_fill_datetime")) ~ "datetime",
#'     any(plot_scales %in% c("scale_colour_time", "scale_fill_time")) ~ "time",
#'     any(plot_scales %in% c("scale_colour_continuous", "scale_fill_continuous")) ~ "numeric",
#'     TRUE ~ "numeric"
#'   )
#'
#'   # Special case for hms
#'   if (!rlang::quo_is_null(col_quo)) {
#'     col_data <- rlang::eval_tidy(col_quo, data)
#'     if (inherits(col_data, "hms")) {
#'       scale_class <- "time"
#'     }
#'   }
#'
#'   scale_class
#' }
#'
#' # Default value functions ----
#'
#' #' Get transform based on scale class
#' #' @noRd
#' get_transform <- function(transform = NULL, scale_class = NULL) {
#'   transform %||% switch(
#'     scale_class,
#'     time = "hms",
#'     datetime = "time",
#'     date = "date",
#'     "identity"  # default
#'   )
#' }
#'
#' #' Get titles case function
#' #' @noRd
#' get_titles_case <- function(titles_case = NULL) {
#'   titles_case %||% getOption("ggblanket.titles_case", \(x) x)
#' }
#'
#' #' Get perspective based on scale classes
#' #' @noRd
#' get_perspective <- function(perspective = NULL, x_scale_class, y_scale_class) {
#'   perspective %||% if (y_scale_class == "discrete" && x_scale_class != "discrete") {
#'     "y"
#'   } else {
#'     "x"
#'   }
#' }
#'
#' #' Determine if x should be symmetric
#' #' @noRd
#' is_x_symmetric <- function(
#'     x_symmetric = NULL,
#'     stat,
#'     facet_scales,
#'     x_scale_class,
#'     y_scale_class
#' ) {
#'   if (!is.null(x_symmetric)) {
#'     return(x_symmetric)
#'   }
#'
#'   # Conditions where x should NOT be symmetric
#'   !(stringr::str_detect(stat, "sf") ||
#'       facet_scales %in% c("free", "free_x") ||
#'       !(y_scale_class == "discrete" && x_scale_class != "discrete"))
#' }
#'
#' #' Determine if y should be symmetric
#' #' @noRd
#' is_y_symmetric <- function(
#'     y_symmetric = NULL,
#'     stat,
#'     facet_scales,
#'     x_scale_class,
#'     y_scale_class
#' ) {
#'   if (!is.null(y_symmetric)) {
#'     return(y_symmetric)
#'   }
#'
#'   # Conditions where y should NOT be symmetric
#'   !(stringr::str_detect(stat, "sf") ||
#'       facet_scales %in% c("free", "free_y") ||
#'       (y_scale_class == "discrete" && x_scale_class != "discrete"))
#' }
#'
#' # Input validation functions ----
#'
#' #' Check inputs are valid
#' #' @noRd
#' check_inputs <- function(mapping, x_symmetric, y_symmetric,
#'                          x_transform, y_transform, stat) {
#'   # Check mapping doesn't include facets
#'   if (!is.null(mapping)) {
#'     mapping_names <- names(unlist(mapping))
#'     if (any(mapping_names %in% c("facet", "facet2"))) {
#'       rlang::abort("mapping argument does not support facet or facet2")
#'     }
#'   }
#'
#'   # Check symmetric constraints
#'   both_symmetric <- x_symmetric && y_symmetric
#'   has_transform <- !is.null(x_transform) || !is.null(y_transform)
#'   non_identity_stat <- !identical(stat, "identity")
#'
#'   if (both_symmetric && (has_transform || non_identity_stat)) {
#'     rlang::abort(c(
#'       "Both x_symmetric and y_symmetric are not supported where",
#'       "a positional axis is transformed or the stat is not 'identity'"
#'     ))
#'   }
#' }
#'
#' # Data processing functions ----
#'
#' #' Process data for factors and reversing
#' #' @noRd
#' process_data <- function(data, aes_list, x_symmetric) {
#'   # Get non-NULL aesthetics
#'   active_aes <- list(
#'     aes_list$x, aes_list$xmin, aes_list$xmax, aes_list$xend,
#'     aes_list$y, aes_list$ymin, aes_list$ymax, aes_list$yend,
#'     aes_list$col, aes_list$facet, aes_list$facet2
#'   ) |>
#'     purrr::discard(rlang::quo_is_null)
#'
#'   data |>
#'     dplyr::ungroup() |>
#'     # Convert character/logical to factors
#'     dplyr::mutate(dplyr::across(
#'       c(!!!active_aes) &
#'         (tidyselect::where(is.character) |
#'            tidyselect::where(is.factor) |
#'            tidyselect::where(is.logical)),
#'       labelled::to_factor
#'     )) |>
#'     # Reverse y factors for top-to-bottom reading
#'     dplyr::mutate(dplyr::across(
#'       c(!!aes_list$y, !!aes_list$ymin, !!aes_list$ymax, !!aes_list$yend) &
#'         tidyselect::where(is.factor),
#'       forcats::fct_rev
#'     )) |>
#'     # Handle col factor reversal for flipped plots
#'     reverse_col_if_needed(aes_list, x_symmetric)
#' }
#'
#' #' Reverse col factor if needed
#' #' @noRd
#' reverse_col_if_needed <- function(data, aes_list, x_symmetric) {
#'   if (!rlang::quo_is_null(aes_list$col) && x_symmetric) {
#'     # Check if col and y are different
#'     col_equals_y <- identical(
#'       rlang::eval_tidy(aes_list$y, data),
#'       rlang::eval_tidy(aes_list$col, data)
#'     )
#'
#'     if (!col_equals_y) {
#'       data <- data |>
#'         dplyr::mutate(dplyr::across(
#'           !!aes_list$col & tidyselect::where(is.factor),
#'           forcats::fct_rev
#'         ))
#'     }
#'   }
#'   data
#' }
#'
#' # Facet functions ----
#'
#' #' Get facet layout
#' #' @noRd
#' get_facet_layout <- function(facet_layout, aes_list) {
#'   if (!is.null(facet_layout)) {
#'     return(facet_layout)
#'   }
#'
#'   has_facet <- !rlang::quo_is_null(aes_list$facet)
#'   has_facet2 <- !rlang::quo_is_null(aes_list$facet2)
#'
#'   dplyr::case_when(
#'     has_facet && !has_facet2 ~ "wrap",
#'     !has_facet && has_facet2 ~ "grid",
#'     has_facet && has_facet2 ~ "grid",
#'     TRUE ~ "null"
#'   )
#' }
#'
#' #' Get facet axes default
#' #' @noRd
#' get_facet_axes <- function(facet_axes, x_symmetric) {
#'   facet_axes %||% if (x_symmetric) "all_y" else "all_x"
#' }
#'
#' #' Add facet layer to plot
#' #' @noRd
#' add_facet_layer <- function(
#'     plot, aes_list, data, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes, facet_axis_labels,
#'     facet_nrow, facet_ncol, facet_labels, y_scale_class
#' ) {
#'   # Check if we need to reverse facet
#'   reverse_facet <- y_scale_class == "discrete" &&
#'     identical(
#'       rlang::eval_tidy(aes_list$y, data),
#'       rlang::eval_tidy(aes_list$facet, data)
#'     )
#'
#'   # Choose appropriate faceting function
#'   facet_fn <- if (reverse_facet) {
#'     add_facet_layer_rev
#'   } else {
#'     add_facet_layer_std
#'   }
#'
#'   facet_fn(
#'     plot, aes_list, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes, facet_axis_labels,
#'     facet_nrow, facet_ncol, facet_labels
#'   )
#' }
#'
#' #' Add facet layer with reversed facet
#' #' @noRd
#' add_facet_layer_rev <- function(
#'     plot, aes_list, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes,
#'     facet_axis_labels, facet_nrow, facet_ncol,
#'     facet_labels
#' ) {
#'   # Build facet vars with reversal
#'   facet_vars <- build_facet_vars(aes_list, reverse_facet = TRUE)
#'
#'   add_facet_by_layout(
#'     plot, facet_vars, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes,
#'     facet_axis_labels, facet_nrow, facet_ncol,
#'     facet_labels
#'   )
#' }
#'
#' #' Add facet layer normal (not reversed)
#' #' @noRd
#' add_facet_layer_std <- function(
#'     plot, aes_list, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes,
#'     facet_axis_labels, facet_nrow, facet_ncol,
#'     facet_labels
#' ) {
#'   # Build facet vars without reversal
#'   facet_vars <- build_facet_vars(aes_list, reverse_facet = FALSE)
#'
#'   add_facet_by_layout(
#'     plot, facet_vars, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes,
#'     facet_axis_labels, facet_nrow, facet_ncol,
#'     facet_labels
#'   )
#' }
#'
#' #' Build facet variables
#' #' @noRd
#' build_facet_vars <- function(aes_list, reverse_facet = FALSE) {
#'   has_facet <- !rlang::quo_is_null(aes_list$facet)
#'   has_facet2 <- !rlang::quo_is_null(aes_list$facet2)
#'
#'   list(
#'     facet = if (has_facet && reverse_facet) {
#'       ggplot2::vars(forcats::fct_rev(!!aes_list$facet))
#'     } else if (has_facet) {
#'       ggplot2::vars(!!aes_list$facet)
#'     } else {
#'       NULL
#'     },
#'     facet2 = if (has_facet2) {
#'       ggplot2::vars(!!aes_list$facet2)
#'     } else {
#'       NULL
#'     }
#'   )
#' }
#'
#' #' Add facet by layout type
#' #' @noRd
#' add_facet_by_layout <- function(
#'     plot, facet_vars, facet_layout, facet_scales,
#'     facet_space, facet_drop, facet_axes,
#'     facet_axis_labels, facet_nrow, facet_ncol,
#'     facet_labels
#' ) {
#'   if (facet_layout == "wrap") {
#'     # Combine facet vars for wrap
#'     all_vars <- c(facet_vars$facet, facet_vars$facet2) |>
#'       purrr::compact()
#'
#'     if (length(all_vars) > 0) {
#'       plot + ggplot2::facet_wrap(
#'         facets = all_vars,
#'         scales = facet_scales,
#'         space = facet_space,
#'         drop = facet_drop,
#'         axes = facet_axes,
#'         axis.labels = facet_axis_labels,
#'         nrow = facet_nrow,
#'         ncol = facet_ncol,
#'         labeller = ggplot2::as_labeller(facet_labels)
#'       )
#'     } else {
#'       plot
#'     }
#'   } else if (facet_layout == "grid") {
#'     if (!is.null(facet_vars$facet) || !is.null(facet_vars$facet2)) {
#'       plot + ggplot2::facet_grid(
#'         rows = facet_vars$facet2,
#'         cols = facet_vars$facet,
#'         scales = facet_scales,
#'         space = facet_space,
#'         drop = facet_drop,
#'         axes = facet_axes,
#'         axis.labels = facet_axis_labels,
#'         labeller = ggplot2::as_labeller(facet_labels)
#'       )
#'     } else {
#'       plot
#'     }
#'   } else {
#'     plot
#'   }
#' }
#'
#' # Color calculation functions ----
#'
#' #' Calculate number of colors needed
#' #' @noRd
#' calculate_colour_n <- function(aes_list, data, plot_data) {
#'   # Get factor levels if col is a factor
#'   col_n_factor <- if (!rlang::quo_is_null(aes_list$col)) {
#'     col_data <- rlang::eval_tidy(aes_list$col, data)
#'     if (is.factor(col_data)) length(levels(col_data)) else NA
#'   } else {
#'     NA
#'   }
#'
#'   # Count distinct colors in plot data
#'   colour_n <- plot_data |>
#'     dplyr::select(tidyselect::any_of("colour")) |>
#'     dplyr::filter(.data$colour != "grey50") |>
#'     dplyr::n_distinct()
#'
#'   fill_n <- plot_data |>
#'     dplyr::select(tidyselect::any_of("fill")) |>
#'     dplyr::filter(.data$fill != "grey50") |>
#'     dplyr::n_distinct()
#'
#'   # Return maximum
#'   max(col_n_factor, colour_n, fill_n, na.rm = TRUE)
#' }
#'
#' #' Check if aesthetic matches colour
#' #' @noRd
#' check_aesthetic_matches_colour <- function(plot_build, aesthetic) {
#'   colour_label <- plot_build$plot$labels$colour
#'   fill_label <- plot_build$plot$labels$fill
#'   aes_label <- plot_build$plot$labels[[aesthetic]]
#'
#'   # Check if labels match
#'   (!is.null(colour_label) && !is.null(aes_label) &&
#'       rlang::as_name(colour_label[1]) == rlang::as_name(aes_label[1])) ||
#'     (!is.null(fill_label) && !is.null(aes_label) &&
#'        rlang::as_name(fill_label[1]) == rlang::as_name(aes_label[1]))
#' }
#'
#' # Title extraction functions ----
#'
#' #' Extract title with fallback
#' #' @noRd
#' get_title <- function(data, aes_quo, build_title, titles_case, default = NULL) {
#'   # Try to get label attribute from data
#'   if (!rlang::quo_is_null(aes_quo)) {
#'     data_col <- dplyr::pull(data, !!aes_quo)
#'     label_attr <- attr(data_col, "label")
#'     if (!is.null(label_attr)) {
#'       return(label_attr)
#'     }
#'   }
#'
#'   # Try to get from build title
#'   if (!is.null(build_title)) {
#'     return(purrr::map_chr(rlang::as_name(build_title[1]), titles_case))
#'   }
#'
#'   # Return default
#'   default
#' }
#'
#' #' Extract titles for other aesthetics
#' #' @noRd
#' get_titles2 <- function(plot_build, col_title, titles_case) {
#'   aesthetics <- c("alpha", "shape", "size", "linewidth", "linetype", "pattern")
#'
#'   aesthetics |>
#'     purrr::set_names() |>
#'     purrr::map(\(aes) {
#'       label <- plot_build$plot$labels[[aes]]
#'       if (is.null(label)) {
#'         return(NULL)
#'       }
#'
#'       if (check_aesthetic_matches_colour(plot_build, aes)) {
#'         col_title
#'       } else {
#'         purrr::map_chr(rlang::as_name(label[1]), titles_case)
#'       }
#'     }) |>
#'     purrr::set_names(paste0, "_title")
#' }
#'
#' # Theme modification functions ----
#'
#' #' Get transparency defaults
#' #' @noRd
#' get_perspective_behaviour <- function(
#'     axis_line_transparent,
#'     axis_ticks_transparent,
#'     panel_grid_transparent
#' ) {
#'   list(
#'     axis_line_transparent = axis_line_transparent %||%
#'       getOption("ggblanket.axis_line_transparent", TRUE),
#'     axis_ticks_transparent = axis_ticks_transparent %||%
#'       getOption("ggblanket.axis_ticks_transparent", TRUE),
#'     panel_grid_transparent = panel_grid_transparent %||%
#'       getOption("ggblanket.panel_grid_transparent", TRUE)
#'   )
#' }
#'
#' #' Add transparency based on perspective
#' #' @noRd
#' add_perspective <- function(
#'     plot, perspective, axis_line_transparent,
#'     axis_ticks_transparent, panel_grid_transparent,
#'     x_scale_class, y_scale_class
#' ) {
#'   theme_updates <- list()
#'
#'   if (perspective == "x") {
#'     if (axis_line_transparent) {
#'       theme_updates$axis.line.y <- ggplot2::element_line(colour = "transparent")
#'     }
#'     if (axis_ticks_transparent) {
#'       theme_updates$axis.ticks.y <- ggplot2::element_line(colour = "transparent")
#'     }
#'     if (panel_grid_transparent) {
#'       theme_updates$panel.grid.major.x <- ggplot2::element_line(colour = "transparent")
#'       theme_updates$panel.grid.minor.x <- ggplot2::element_line(colour = "transparent")
#'     }
#'     if (x_scale_class == "discrete") {
#'       theme_updates$axis.ticks.x <- ggplot2::element_line(colour = "transparent")
#'     }
#'   } else if (perspective == "y") {
#'     if (axis_line_transparent) {
#'       theme_updates$axis.line.x <- ggplot2::element_line(colour = "transparent")
#'     }
#'     if (axis_ticks_transparent) {
#'       theme_updates$axis.ticks.x <- ggplot2::element_line(colour = "transparent")
#'     }
#'     if (panel_grid_transparent) {
#'       theme_updates$panel.grid.major.y <- ggplot2::element_line(colour = "transparent")
#'       theme_updates$panel.grid.minor.y <- ggplot2::element_line(colour = "transparent")
#'     }
#'     if (y_scale_class == "discrete") {
#'       theme_updates$axis.ticks.y <- ggplot2::element_line(colour = "transparent")
#'     }
#'   }
#'
#'   if (length(theme_updates) > 0) {
#'     plot + do.call(ggplot2::theme, theme_updates)
#'   } else {
#'     plot
#'   }
#' }
#'
#' # Symmetric scale functions ----
#'
#' #' Create symmetric x scale
#' #' @noRd
#' scale_x_symmetric <- function(
#'     data = NULL,
#'     x = NULL,
#'     symmetric = TRUE,
#'     breaks = NULL,
#'     breaks_n = 6,
#'     expand = NULL,
#'     expand_limits = NULL,
#'     labels = NULL,
#'     position = "bottom",
#'     sec_axis = ggplot2::waiver(),
#'     transform = "identity"
#' ) {
#'   # Get transform
#'   transform <- get_transform(transform = transform)
#'
#'   # Check if symmetric is supported for this transform
#'   if (symmetric) {
#'     if (
#'       any(stringr::str_detect(transform, "log-")) |
#'       any(transform %in% c("log", "log2", "log10"))
#'     ) {
#'       symmetric <- FALSE
#'       rlang::inform("ggblanket does not currently support log symmetric axes")
#'     }
#'   }
#'
#'   if (symmetric) {
#'     x <- rlang::enquo(x)
#'
#'     vctr <- data |>
#'       dplyr::pull(!!x)
#'
#'     if (!is.null(expand_limits)) {
#'       vctr <- c(vctr, expand_limits)
#'     }
#'
#'     # Convert based on transform type
#'     if (any(transform == "hms")) {
#'       vctr <- hms::as_hms(vctr)
#'     } else if (any(transform %in% c("time", "datetime"))) {
#'       vctr <- lubridate::as_datetime(vctr)
#'     } else if (any(transform == "date")) {
#'       vctr <- lubridate::as_date(vctr)
#'     }
#'
#'     range <- range(vctr, na.rm = TRUE)
#'
#'     if (any(transform == "hms")) {
#'       range <- hms::as_hms(range)
#'     }
#'
#'     # Generate breaks
#'     if (is.null(breaks)) {
#'       if (any(transform %in% c("hms", "time", "datetime", "date"))) {
#'         breaks <- scales::breaks_pretty(n = breaks_n)(range)
#'       } else if (
#'         any(stringr::str_detect(transform, "log-")) |
#'         any(transform %in% c("log", "log2", "log10"))
#'       ) {
#'         breaks <- scales::breaks_log(n = breaks_n)(range)
#'       } else {
#'         breaks <- scales::breaks_pretty(n = breaks_n)(range)
#'       }
#'     } else if (is.function(breaks)) {
#'       breaks <- breaks(range)
#'     }
#'
#'     # Set limits to range of breaks
#'     limits <- range(breaks)
#'
#'     if (any(transform %in% "reverse")) {
#'       limits <- rev(limits)
#'     }
#'
#'     # Zero expand for symmetric
#'     if (is.null(expand)) {
#'       expand <- ggplot2::expansion(mult = c(0, 0))
#'     }
#'
#'     # Set labels
#'     if (is.null(labels)) {
#'       if (any(transform == "hms")) {
#'         labels <- scales::label_time()
#'       } else if (any(transform %in% c("time", "datetime", "date"))) {
#'         labels <- scales::label_date_short(leading = "")
#'       } else {
#'         labels <- scales::label_comma(drop0trailing = TRUE)
#'       }
#'     }
#'
#'     scale <- ggplot2::scale_x_continuous(
#'       breaks = breaks,
#'       labels = labels,
#'       limits = limits,
#'       expand = expand,
#'       oob = scales::oob_keep,
#'       transform = transform,
#'       position = position,
#'       sec.axis = sec_axis
#'     )
#'   } else {
#'     # Non-symmetric scale
#'     if (is.null(breaks)) {
#'       if (any(transform %in% c("hms", "time", "datetime", "date"))) {
#'         breaks <- scales::breaks_pretty(n = breaks_n)
#'       } else if (
#'         any(stringr::str_detect(transform, "log-")) |
#'         any(transform %in% c("log", "log2", "log10"))
#'       ) {
#'         breaks <- scales::breaks_log(n = breaks_n)
#'       } else {
#'         breaks <- scales::breaks_pretty(n = breaks_n)
#'       }
#'     }
#'
#'     if (is.null(expand)) {
#'       expand <- ggplot2::expansion(mult = c(0.05, 0.05))
#'     }
#'
#'     if (is.null(labels)) {
#'       if (any(transform == "hms")) {
#'         labels <- scales::label_time()
#'       } else if (any(transform %in% c("time", "datetime", "date"))) {
#'         labels <- scales::label_date_short(leading = "")
#'       } else {
#'         labels <- scales::label_comma(drop0trailing = TRUE)
#'       }
#'     }
#'
#'     scale <- list(
#'       ggplot2::scale_x_continuous(
#'         breaks = breaks,
#'         labels = labels,
#'         expand = expand,
#'         oob = scales::oob_keep,
#'         transform = transform,
#'         position = position,
#'         sec.axis = sec_axis
#'       ),
#'       ggplot2::expand_limits(x = expand_limits)
#'     )
#'   }
#'
#'   return(scale)
#' }
#'
#' #' Create symmetric y scale
#' #' @noRd
#' scale_y_symmetric <- function(
#'     data = NULL,
#'     y = NULL,
#'     symmetric = TRUE,
#'     breaks = NULL,
#'     breaks_n = 6,
#'     expand = NULL,
#'     expand_limits = NULL,
#'     labels = NULL,
#'     position = "left",
#'     sec_axis = ggplot2::waiver(),
#'     transform = "identity"
#' ) {
#'   # Get transform
#'   transform <- get_transform(transform = transform)
#'
#'   # Check if symmetric is supported for this transform
#'   if (symmetric) {
#'     if (
#'       any(stringr::str_detect(transform, "log-")) |
#'       any(transform %in% c("log", "log2", "log10"))
#'     ) {
#'       symmetric <- FALSE
#'       rlang::inform("ggblanket does not currently support log symmetric axes")
#'     }
#'   }
#'
#'   if (symmetric) {
#'     y <- rlang::enquo(y)
#'
#'     vctr <- data |>
#'       dplyr::pull(!!y)
#'
#'     if (!is.null(expand_limits)) {
#'       vctr <- c(vctr, expand_limits)
#'     }
#'
#'     # Convert based on transform type
#'     if (any(transform == "hms")) {
#'       vctr <- hms::as_hms(vctr)
#'     } else if (any(transform %in% c("time", "datetime"))) {
#'       vctr <- lubridate::as_datetime(vctr)
#'     } else if (any(transform == "date")) {
#'       vctr <- lubridate::as_date(vctr)
#'     }
#'
#'     range <- range(vctr, na.rm = TRUE)
#'
#'     if (any(transform == "hms")) {
#'       range <- hms::as_hms(range)
#'     }
#'
#'     # Generate breaks
#'     if (is.null(breaks)) {
#'       if (any(transform %in% c("hms", "time", "datetime", "date"))) {
#'         breaks <- scales::breaks_pretty(n = breaks_n)(range)
#'       } else if (
#'         any(stringr::str_detect(transform, "log-")) |
#'         any(transform %in% c("log", "log2", "log10"))
#'       ) {
#'         breaks <- scales::breaks_log(n = breaks_n)(range)
#'       } else {
#'         breaks <- scales::breaks_pretty(n = breaks_n)(range)
#'       }
#'     } else if (is.function(breaks)) {
#'       breaks <- breaks(range)
#'     }
#'
#'     # Set limits to range of breaks
#'     limits <- range(breaks)
#'
#'     if (any(transform %in% "reverse")) {
#'       limits <- rev(limits)
#'     }
#'
#'     # Zero expand for symmetric
#'     if (is.null(expand)) {
#'       expand <- ggplot2::expansion(mult = c(0, 0))
#'     }
#'
#'     # Set labels
#'     if (is.null(labels)) {
#'       if (any(transform == "hms")) {
#'         labels <- scales::label_time()
#'       } else if (any(transform %in% c("time", "datetime", "date"))) {
#'         labels <- scales::label_date_short(leading = "")
#'       } else {
#'         labels <- scales::label_comma(drop0trailing = TRUE)
#'       }
#'     }
#'
#'     scale <- ggplot2::scale_y_continuous(
#'       breaks = breaks,
#'       labels = labels,
#'       limits = limits,
#'       expand = expand,
#'       oob = scales::oob_keep,
#'       transform = transform,
#'       position = position,
#'       sec.axis = sec_axis
#'     )
#'   } else {
#'     # Non-symmetric scale
#'     if (is.null(breaks)) {
#'       if (any(transform %in% c("hms", "time", "datetime", "date"))) {
#'         breaks <- scales::breaks_pretty(n = breaks_n)
#'       } else if (
#'         any(stringr::str_detect(transform, "log-")) |
#'         any(transform %in% c("log", "log2", "log10"))
#'       ) {
#'         breaks <- scales::breaks_log(n = breaks_n)
#'       } else {
#'         breaks <- scales::breaks_pretty(n = breaks_n)
#'       }
#'     }
#'
#'     if (is.null(expand)) {
#'       expand <- ggplot2::expansion(mult = c(0.05, 0.05))
#'     }
#'
#'     if (is.null(labels)) {
#'       if (any(transform == "hms")) {
#'         labels <- scales::label_time()
#'       } else if (any(transform %in% c("time", "datetime", "date"))) {
#'         labels <- scales::label_date_short(leading = "")
#'       } else {
#'         labels <- scales::label_comma(drop0trailing = TRUE)
#'       }
#'     }
#'
#'     scale <- list(
#'       ggplot2::scale_y_continuous(
#'         breaks = breaks,
#'         labels = labels,
#'         expand = expand,
#'         oob = scales::oob_keep,
#'         transform = transform,
#'         position = position,
#'         sec.axis = sec_axis
#'       ),
#'       ggplot2::expand_limits(y = expand_limits)
#'     )
#'   }
#'
#'   return(scale)
#' }
#'
#' # Null-coalescing operator ----
#' `%||%` <- function(x, y) if (is.null(x)) y else x

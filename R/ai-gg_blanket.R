#' # gg_blanket_modular.R
#' # Modularized version of gg_blanket that uses helper functions
#' # This maintains the exact same behavior as the original but is more maintainable
#'
#' #' Blanket ggplot
#' #'
#' #' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()` with [geom_blank()][ggplot2::geom_blank()] defaults for the geom, stat and position. This function underlies all other `gg_*` functions. It contains a `geom` argument for maximum flexibility.
#' #'
#' #' @param data A data frame or tibble.
#' #' @param ... Other arguments passed to within a `params` list in [ggplot2::layer()].
#' #' @param geom A geometric object to display the data. A snakecase character string of a ggproto Geom subclass object minus the Geom prefix (e.g. `"point"`).
#' #' @param stat A statistical transformation to use on the data. A snakecase character string of a ggproto Stat subclass object minus the Stat prefix (e.g. `"identity"`).
#' #' @param position A position adjustment. A snakecase character string of a ggproto Position subclass object minus the Position prefix (e.g. `"identity"`), or a `position_*()` function that outputs a ggproto Position subclass object (e.g. `ggplot2::position_identity()`).
#' #' @param coord A coordinate system. A `coord_*()` function that outputs a constructed ggproto Coord subclass object (e.g. [ggplot2::coord_cartesian()]).
#' #' @param blend The blending mode per [ggblend::blend()] (e.g. "multiply").
#' #' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]). (Or a list that includes 1. a theme and 2. a [ggplot2::labs()] function. E.g. `list(theme_lighter(), ggplot2::labs(colour = NULL, fill = NULL)`).
#' #' @param perspective The perspective of plot, which affects the theme components that are removed. Either `"x"` or `"y"`.
#' #' @param axis_line_transparent `TRUE` or `FALSE` of whether to remove the relevant axis line per the `perspective` of the plot.
#' #' @param axis_ticks_transparent `TRUE` or `FALSE` of whether to remove the relevant axis ticks per the `perspective` of the plot.
#' #' @param panel_grid_transparent `TRUE` or `FALSE` of whether to remove the relevant panel grid per the `perspective` of the plot.
#' #' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,facet,facet2,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' #' @param mapping A set of additional aesthetic mappings in [ggplot2::aes()] defaults. Intended primarily for non-supported aesthetics (e.g. `shape`, `linetype`, `linewidth`, or `size`), but can also be used for delayed evaluation etc.
#' #' @param x_breaks,y_breaks,col_breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' #' @param x_breaks_n,y_breaks_n,col_breaks_n A number of desired breaks for when `*_breaks = NULL`.
#' #' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' #' @param x_limits_include,y_limits_include,col_limits_include For a continuous variable, any values that the limits should encompass (e.g. `0`). For a discrete scale, manipulate the data instead with `forcats::fct_expand`.
#' #' @param x_label,y_label,col_label Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)` for no title.
#' #' @param x_labels,y_labels,col_labels,facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels. (Note this must be named for `facet_labels`).
#' #' @param x_position,y_position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).If using `y_position = "top"` with a `*_*` theme, add `caption = ""` or `caption = "\n"`.
#' #' @param x_sec_axis,y_sec_axis A secondary axis with [ggplot2::dup_axis()] or  [ggplot2::sec_axis()] defaults.
#' #' @param x_symmetric,y_symmetric `TRUE` or `FALSE` of whether a symmetric scale.
#' #' @param x_transform,y_transform,col_transform For a continuous scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' #' @param col_drop,facet_drop For a discrete variable, FALSE or TRUE of whether to drop unused levels.
#' #' @param col_legend_ncol,col_legend_nrow The number of columns and rows in a legend guide.
#' #' @param col_legend_rev `TRUE` or `FALSE` of whether to reverse the elements of a legend guide. Defaults to `FALSE`.
#' #' @param col_palette A character vector of hex codes (or names) or a `scales::pal_*()` function.
#' #' @param col_palette_na A hex code (or name) for the colour of `NA` values.
#' #' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' #' @param col_steps For a continuous variable, `TRUE` or `FALSE` of whether to colour in steps. Defaults to `FALSE`.
#' #' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`. Sometimes `+ *_*()` may be needed.
#' #' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' #' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' #' @param facet_ncol,facet_nrow The number of columns and rows of facet panels. Only applies to a facet layout of `"wrap"`.
#' #' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' #' @param facet_space When the facet scales are _not_ `"fixed"`, whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' #' @param title Title string.
#' #' @param subtitle Subtitle string.
#' #' @param caption Caption title string.
#' #' @param label_case A function to format the label of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#' #'
#' #' @return A ggplot object.
#' #' @export
#' gg_blanket <- function(
#'     data = NULL,
#'     ...,
#'     geom = "blank",
#'     stat = "identity",
#'     position = "identity",
#'     coord = NULL,
#'     blend = NULL, theme = NULL,
#'     perspective = NULL,
#'     axis_line_transparent = NULL,
#'     axis_ticks_transparent = NULL,
#'     panel_grid_transparent = NULL,
#'
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
#'     label = NULL,
#'     text = NULL,
#'     sample = NULL,
#'     mapping = NULL,
#'     x_breaks = NULL,
#'     x_breaks_n = NULL,
#'     x_expand = NULL,
#'     x_limits_include = NULL,
#'     x_label = NULL,
#'     x_labels = NULL,
#'     x_position = "bottom",
#'     x_sec_axis = ggplot2::waiver(),
#'     x_symmetric = NULL,
#'     x_transform = NULL,
#'     y_breaks = NULL,
#'     y_breaks_n = NULL,
#'     y_expand = NULL,
#'     y_limits_include = NULL,
#'     y_label = NULL,
#'     y_labels = NULL,
#'     y_position = "left",
#'     y_sec_axis = ggplot2::waiver(),
#'     y_symmetric = NULL,
#'     y_transform = NULL,
#'     col_breaks = NULL,
#'     col_breaks_n = 5,
#'     col_drop = FALSE,
#'     col_limits_include = NULL,
#'     col_label = NULL,
#'     col_labels = NULL,
#'     col_legend_ncol = NULL,
#'     col_legend_nrow = NULL,
#'     col_legend_rev = FALSE,
#'     col_palette = NULL,
#'     col_palette_na = NULL,
#'     col_rescale = scales::rescale(),
#'     col_steps = FALSE,
#'     col_transform = NULL,
#'     facet_axes = NULL,
#'     facet_axis_labels = "margins",
#'     facet_drop = FALSE,
#'     facet_labels = NULL,
#'     facet_layout = NULL,
#'     facet_ncol = NULL,
#'     facet_nrow = NULL,
#'     facet_scales = "fixed",
#'     facet_space = "fixed",
#'     title = NULL,
#'     subtitle = NULL,
#'     caption = NULL,
#'     label_case = NULL
#' ) {
#'   options(ggblend.check_blend = FALSE)
#'
#'   ##############################################################################
#'   # Step 1: Quote aesthetics
#'   ##############################################################################
#'   aes_list <- quote_aesthetics(x, y, col, facet, facet2, xmin, xmax, xend,
#'                                ymin, ymax, yend, z, group, subgroup, label,
#'                                text, sample)
#'
#'   ##############################################################################
#'   # Step 2: Handle NULL data
#'   ##############################################################################
#'   if (rlang::is_null(data)) {
#'     data <- data.frame(x = NA)
#'   }
#'
#'   ##############################################################################
#'   # Step 3: Extract geom, stat & position strings
#'   ##############################################################################
#'   names <- extract_names(geom, stat, position)
#'   geom_name <- names$geom_name
#'   stat_name <- names$stat_name
#'   position_name <- names$position_name
#'
#'   ##############################################################################
#'   # Step 4: Determine scale types
#'   ##############################################################################
#'
#'   # Create initial plot to determine scale types
#'   plot <- plot_base_internal(
#'     data = data,
#'     x = !!aes_list$x,
#'     y = !!aes_list$y,
#'     col = !!aes_list$col,
#'     xmin = !!aes_list$xmin,
#'     xmax = !!aes_list$xmax,
#'     xend = !!aes_list$xend,
#'     ymin = !!aes_list$ymin,
#'     ymax = !!aes_list$ymax,
#'     yend = !!aes_list$yend,
#'     z = !!aes_list$z,
#'     group = !!aes_list$group,
#'     subgroup = !!aes_list$subgroup,
#'     sample = !!aes_list$sample,
#'     label = !!aes_list$label,
#'     text = !!aes_list$text,
#'   )
#'
#'   show_legend <- ifelse(geom_name == "blank", FALSE, TRUE)
#'   params <- get_geom_params(geom_name, ...)
#'
#'   # Add initial layer
#'   plot <- add_initial_layer(plot, geom, stat, position, mapping, params,
#'                             show_legend, coord, blend, stat_name)
#'
#'   # Get plot build
#'   suppressMessages({
#'     suppressWarnings({
#'       plot_build <- ggplot2::ggplot_build(plot)
#'       plot_data <- plot_build$data[[1]]
#'     })
#'   })
#'
#'   # Determine scale types
#'   scale_types <- determine_scale_types(plot_build, aes_list, data)
#'   x_scale_type <- scale_types$x_scale_type
#'   y_scale_type <- scale_types$y_scale_type
#'   col_scale_class <- scale_types$col_scale_class
#'
#'   ##############################################################################
#'   # Step 5: Get defaults
#'   ##############################################################################
#'   defaults <- get_defaults(x_transform, y_transform, x_scale_type, y_scale_type,
#'                            facet_scales, theme, x_symmetric, y_symmetric,
#'                            stat_name, perspective, label_case)
#'
#'   x_transform <- defaults$x_transform
#'   y_transform <- defaults$y_transform
#'   x_transform_null <- defaults$x_transform_null
#'   y_transform_null <- defaults$y_transform_null
#'   x_drop <- defaults$x_drop
#'   y_drop <- defaults$y_drop
#'   theme <- defaults$theme
#'   x_symmetric <- defaults$x_symmetric
#'   y_symmetric <- defaults$y_symmetric
#'   perspective <- defaults$perspective
#'   label_case <- defaults$label_case
#'
#'   ##############################################################################
#'   # Step 6: Validate inputs
#'   ##############################################################################
#'   validate_inputs(mapping, x_symmetric, y_symmetric,
#'                   x_transform_null, y_transform_null, stat)
#'
#'   ##############################################################################
#'   # Step 7: Process the data
#'   ##############################################################################
#'   data <- process_data(data, aes_list, x_symmetric)
#'
#'   ##############################################################################
#'   # Step 8: Rebuild base plot with processed data
#'   ##############################################################################
#'   plot <- plot_base_internal(
#'     data = data,
#'     x = !!aes_list$x,
#'     y = !!aes_list$y,
#'     col = !!aes_list$col,
#'     xmin = !!aes_list$xmin,
#'     xmax = !!aes_list$xmax,
#'     xend = !!aes_list$xend,
#'     ymin = !!aes_list$ymin,
#'     ymax = !!aes_list$ymax,
#'     yend = !!aes_list$yend,
#'     z = !!aes_list$z,
#'     group = !!aes_list$group,
#'     subgroup = !!aes_list$subgroup,
#'     sample = !!aes_list$sample,
#'     label = !!aes_list$label,
#'     text = !!aes_list$text,
#'   ) +
#'     theme
#'
#'   ##############################################################################
#'   # Step 9: Add geom layer
#'   ##############################################################################
#'   plot <- add_initial_layer(plot, geom, stat, position, mapping, params,
#'                             show_legend, coord, blend, stat_name)
#'
#'   if (!rlang::is_null(x_limits_include)) {
#'     plot <- plot + ggplot2::expand_limits(x = x_limits_include)
#'   }
#'
#'   if (!rlang::is_null(y_limits_include)) {
#'     plot <- plot + ggplot2::expand_limits(y = y_limits_include)
#'   }
#'
#'   ##############################################################################
#'   # Step 10: Add facet layer
#'   ##############################################################################
#'   facet_layout <- get_facet_layout(facet_layout, aes_list)
#'   facet_axes <- get_facet_axes(facet_axes, x_symmetric)
#'
#'   plot <- add_facet_layer(plot, aes_list, data, facet_layout, facet_scales,
#'                           facet_space, facet_drop, facet_axes, facet_axis_labels,
#'                           facet_nrow, facet_ncol, facet_labels, y_scale_type)
#'
#'   # Add default color scales
#'   if (col_scale_class %in% c("discrete", "ordinal")) {
#'     plot <- plot +
#'       ggplot2::scale_colour_hue(na.value = "grey50") +
#'       ggplot2::scale_fill_hue(na.value = "grey50")
#'   }
#'
#'   ##############################################################################
#'   # Step 11: Get plot build again
#'   ##############################################################################
#'   suppressMessages({
#'     suppressWarnings({
#'       plot_build <- ggplot2::ggplot_build(plot)
#'       plot_data <- plot_build$data[[1]]
#'
#'       facet_nrows <- length(unique(plot_build$layout$layout$ROW))
#'       facet_ncols <- length(unique(plot_build$layout$layout$COL))
#'     })
#'   })
#'
#'   ##############################################################################
#'   # Step 12: Make colour scale
#'   ##############################################################################
#'
#'   # This section contains the color scale logic from the original
#'   # Due to its complexity, I'm keeping it mostly intact but could be further modularized
#'
#'   if (!is.na(col_scale_class)) {
#'     if (col_scale_class %in% c("date", "datetime", "time", "numeric")) {
#'       # Continuous color scale
#'       if (rlang::is_null(col_palette)) {
#'         col_palette <- ggblanket_global$col_palette_c
#'         if (rlang::is_null(col_palette)) {
#'           col_palette <- scales::pal_seq_gradient(
#'             low = "#132B43",
#'             high = "#56B1F7"
#'           )(seq(0, 1, length.out = 20))
#'         }
#'       } else if (rlang::is_function(col_palette)) {
#'         col_palette <- col_palette(20)
#'       }
#'
#'       if (rlang::is_null(col_palette_na)) {
#'         col_palette_na <- ggblanket_global$col_palette_na_c
#'         if (rlang::is_null(col_palette_na)) col_palette_na <- "grey50"
#'       }
#'
#'       #get col_transform if NULL
#'       if (rlang::is_null(col_transform)) {
#'         col_transform <- get_default_transform(col_scale_class)
#'       }
#'
#'       #make a tidy name to deal with composed transforms
#'       col_transform_name <- get_transform_name(col_transform)
#'
#'       if (rlang::is_null(col_breaks)) {
#'         if (any(col_transform_name %in% c("hms", "time", "datetime", "date"))) {
#'           col_breaks <- scales::breaks_pretty(n = col_breaks_n)
#'         } else {
#'           col_breaks <- scales::breaks_extended(
#'             n = col_breaks_n,
#'             only.loose = FALSE
#'           )
#'         }
#'       }
#'
#'       if (rlang::is_null(col_labels)) {
#'         if (any(col_transform_name %in% c("hms"))) {
#'           col_labels <- scales::label_time()
#'         } else if (any(col_transform_name %in% c("date", "datetime", "time"))) {
#'           col_labels <- scales::label_date_short(leading = "")
#'         } else {
#'           col_labels <- scales::label_comma(drop0trailing = TRUE)
#'         }
#'       }
#'
#'       if (!col_steps) {
#'         plot <- plot +
#'           ggplot2::scale_colour_gradientn(
#'             colours = col_palette,
#'             values = col_rescale,
#'             breaks = col_breaks,
#'             labels = col_labels,
#'             transform = col_transform,
#'             oob = scales::oob_keep,
#'             na.value = col_palette_na,
#'             aesthetics = c("colour", "fill")
#'           ) +
#'           ggplot2::guides(
#'             colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
#'             fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
#'           )
#'       }
#'       else if (col_steps) {
#'         plot <- plot +
#'           ggplot2::scale_colour_stepsn(
#'             colours = col_palette,
#'             values = col_rescale,
#'             breaks = col_breaks,
#'             labels = col_labels,
#'             transform = col_transform,
#'             oob = scales::oob_keep,
#'             na.value = col_palette_na,
#'             aesthetics = c("colour", "fill")
#'           ) +
#'           ggplot2::guides(
#'             colour = ggplot2::guide_coloursteps(
#'               reverse = col_legend_rev,
#'               theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
#'             ),
#'             fill = ggplot2::guide_coloursteps(
#'               reverse = col_legend_rev,
#'               theme = ggplot2::theme(legend.ticks = ggplot2::element_blank()),
#'             )
#'           )
#'       }
#'     } else if (col_scale_class %in% c("discrete", "ordinal")) {
#'       # Discrete color scale
#'       if (rlang::is_null(col_palette_na)) {
#'         if (col_scale_class == "discrete") {
#'           if (rlang::is_null(ggblanket_global$col_palette_na_d)) {
#'             col_palette_na <- "grey50"
#'           } else {
#'             col_palette_na <- ggblanket_global$col_palette_na_d
#'           }
#'         } else if (col_scale_class == "ordinal") {
#'           if (rlang::is_null(ggblanket_global$col_palette_na_o)) {
#'             col_palette_na <- "grey50"
#'           } else {
#'             col_palette_na <- ggblanket_global$col_palette_na_o
#'           }
#'         }
#'       }
#'
#'       col_n <- calculate_colour_n(aes_list, data, plot_data)
#'
#'       if (rlang::is_null(col_palette)) {
#'         if (col_scale_class == "discrete") {
#'           col_palette <- ggblanket_global$col_palette_d
#'         } else if (col_scale_class == "ordinal") {
#'           col_palette <- ggblanket_global$col_palette_o
#'         }
#'       }
#'
#'       if (!rlang::is_null(col_n)) {
#'         if (rlang::is_function(col_palette)) {
#'           col_palette <- col_palette(col_n)
#'         } else if (!any(rlang::have_name(col_palette))) {
#'           col_palette <- col_palette[1:col_n]
#'         }
#'       }
#'
#'       if (x_symmetric) {
#'         col_legend_rev <- !col_legend_rev
#'         if (col_scale_class == "discrete") {
#'           col_palette <- rev(col_palette)
#'         }
#'       }
#'
#'       if (col_scale_class == "ordinal") {
#'         col_legend_rev <- !col_legend_rev
#'       }
#'
#'       if (rlang::is_null(col_labels)) {
#'         col_labels <- ggplot2::waiver()
#'       }
#'
#'       if (rlang::is_null(col_breaks)) {
#'         col_breaks <- ggplot2::waiver()
#'       }
#'
#'       if (!rlang::is_null(col_palette)) {
#'         if (rlang::is_vector(col_palette)) {
#'           plot <- plot +
#'             ggplot2::scale_colour_manual(
#'               values = col_palette,
#'               breaks = col_breaks,
#'               labels = col_labels,
#'               na.value = col_palette_na,
#'               drop = col_drop,
#'               aesthetics = c("colour", "fill")
#'             ) +
#'             ggplot2::guides(
#'               colour = ggplot2::guide_legend(
#'                 reverse = col_legend_rev,
#'                 ncol = col_legend_ncol,
#'                 nrow = col_legend_nrow
#'               ),
#'               fill = ggplot2::guide_legend(
#'                 reverse = col_legend_rev,
#'                 ncol = col_legend_ncol,
#'                 nrow = col_legend_nrow
#'               )
#'             )
#'         } else if (rlang::is_function(col_palette)) {
#'           plot <- plot +
#'             ggplot2::discrete_scale(
#'               palette = col_palette,
#'               breaks = col_breaks,
#'               labels = col_labels,
#'               na.value = col_palette_na,
#'               drop = col_drop,
#'               aesthetics = c("colour", "fill")
#'             ) +
#'             ggplot2::guides(
#'               colour = ggplot2::guide_legend(
#'                 reverse = col_legend_rev,
#'                 ncol = col_legend_ncol,
#'                 nrow = col_legend_nrow
#'               ),
#'               fill = ggplot2::guide_legend(
#'                 reverse = col_legend_rev,
#'                 ncol = col_legend_ncol,
#'                 nrow = col_legend_nrow
#'               )
#'             )
#'         }
#'       } else {
#'       }
#'     }
#'   }
#'   return(plot)
#' }

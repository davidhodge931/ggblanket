#' Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()` with [geom_blank()][ggplot2::geom_blank()] defaults for the geom, stat and position.
#'
#' This function underlies all other `gg_*` functions.
#'
#' It additionally contains `geom` and `col_border` arguments.
#'
#' This function contains the additional geom
#'
#' @param data A data frame or tibble.
#' @param ... Other arguments passed to within a `params` list in [ggplot2::layer()].
#' @param geom A geometric object to display the data. A snakecase character string of a ggproto Geom subclass object minus the Geom prefix (e.g. `"point"`).
#' @param stat A statistical transformation to use on the data. A snakecase character string of a ggproto Stat subclass object minus the Stat prefix (e.g. `"identity"`).
#' @param position A position adjustment. A snakecase character string of a ggproto Position subclass object minus the Position prefix (e.g. `"identity"`), or a `position_*()` function that outputs a ggproto Position subclass object (e.g. `ggplot2::position_identity()`).
#' @param coord A coordinate system. A `coord_*()` function that outputs a constructed ggproto Coord subclass object (e.g. [ggplot2::coord_cartesian()]).
#' @param blend The blending mode per [ggblend::blend()] (e.g. "multiply").
#' @param perspective The perspective of plot, which affects the theme components that are removed. Either `"x"` or `"y"`.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether to remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether to remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether to remove the relevant panel grid per the `perspective` of the plot.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,facet,facet2,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' @param mapping A set of additional aesthetic mappings in [ggplot2::aes()] defaults. Intended primarily for non-supported aesthetics (e.g. `shape`, `linetype`, `linewidth`, or `size`), but can also be used for delayed evaluation etc.
#' @param x_breaks,y_breaks,col_breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param x_breaks_n,y_breaks_n,col_breaks_n A number of desired breaks.
#' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param x_limits_include,y_limits_include,col_limits_include For a continuous variable, any values that the limits should encompass (e.g. `0`). For a discrete scale, manipulate the data instead with `forcats::fct_expand`.
#' @param x_title,y_title,col_title Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)` for no title.
#' @param x_labels,y_labels,col_labels,facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels. (Note this must be named for `facet_labels`).
#' @param x_position,y_position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).If using `y_position = "top"` with a `*_*` theme, add `caption = ""` or `caption = "\n"`.
#' @param x_sec_axis,y_sec_axis A secondary axis with [ggplot2::dup_axis()] or  [ggplot2::sec_axis()] defaults.
#' @param x_symmetric,y_symmetric `TRUE` or `FALSE` of whether a symmetric scale.
#' @param x_transform,y_transform,col_transform For a continuous scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param col_border `TRUE` or `FALSE` of whether the geom is to be treated as if it has borders.
#' @param col_drop,facet_drop For a discrete variable, FALSE or TRUE of whether to drop unused levels.
#' @param col_legend_ncol,col_legend_nrow The number of columns and rows in a legend guide.
#' @param col_legend_rev `TRUE` or `FALSE` of whether to reverse the elements of a legend guide. Defaults to `FALSE`.
#' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' @param col_scale_type Either "gradient" or "steps". Defaults to "gradient".
#' @param colour_palette,fill_palette A character vector of hex codes (or names) or a `scales::pal_*()` function.
#' @param colour_palette_na,fill_palette_na A hex code (or name) for the `NA` values.
#' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`. Sometimes `+ *_*()` may be needed.
#' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' @param facet_ncol,facet_nrow The number of columns and rows of facet panels. Only applies to a facet layout of `"wrap"`.
#' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param facet_space When the facet scales are _not_ `"fixed"`, whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param caption Caption title string.
#' @param titles_case A function to format the title of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#'
#' @return A ggplot object.
#' @export
gg_blanket <- function(
    data = NULL,
    ...,
    geom = "blank",
    stat = "identity",
    position = "identity",
    coord = NULL,
    blend = NULL,

    perspective = NULL,
    axis_line_transparent = NULL,
    axis_ticks_transparent = NULL,
    panel_grid_transparent = NULL,
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
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    subgroup = NULL,
    label = NULL,
    text = NULL,
    sample = NULL,
    mapping = NULL,
    x_breaks = NULL,
    x_breaks_n = NULL,
    x_expand = NULL,
    x_limits_include = NULL,
    x_title = NULL,
    x_labels = NULL,
    x_position = "bottom",
    x_sec_axis = ggplot2::waiver(),
    x_symmetric = NULL,
    x_transform = NULL,
    y_breaks = NULL,
    y_breaks_n = NULL,
    y_expand = NULL,
    y_limits_include = NULL,
    y_title = NULL,
    y_labels = NULL,
    y_position = "left",
    y_sec_axis = ggplot2::waiver(),
    y_symmetric = NULL,
    y_transform = NULL,
    col_border = NULL,
    col_breaks = ggplot2::waiver(),
    col_breaks_n = NULL,
    col_drop = FALSE,
    col_limits_include = NULL,
    col_title = NULL,
    col_labels = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_rescale = scales::rescale(),
    col_scale_type = "gradient",
    col_transform = NULL,
    colour_palette = NULL,
    colour_palette_na = NULL,
    fill_palette = NULL,
    fill_palette_na = NULL,
    facet_axes = NULL,
    facet_axis_labels = "margins",
    facet_drop = FALSE,
    facet_labels = NULL,
    facet_layout = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    titles_case = NULL
) {

  ##############################################################################
  # Step 1: Quote aesthetics
  ##############################################################################
  aes_list <- list(
    x = rlang::enquo(x),
    y = rlang::enquo(y),
    col = rlang::enquo(col),
    facet = rlang::enquo(facet),
    facet2 = rlang::enquo(facet2),
    xmin = rlang::enquo(xmin),
    xmax = rlang::enquo(xmax),
    xend = rlang::enquo(xend),
    ymin = rlang::enquo(ymin),
    ymax = rlang::enquo(ymax),
    yend = rlang::enquo(yend),
    z = rlang::enquo(z),
    group = rlang::enquo(group),
    subgroup = rlang::enquo(subgroup),
    label = rlang::enquo(label),
    text = rlang::enquo(text),
    sample = rlang::enquo(sample)
  )

  ##############################################################################
  # Step 2: Handle NULL data
  ##############################################################################
  if (rlang::is_null(data)) {
    data <- data.frame(x = NA)
  }

  ##############################################################################
  # Step 3: Extract geom, stat & position strings
  ##############################################################################
  ggproto_names <- get_geom_stat_position_names(geom, stat, position)
  geom_name <- ggproto_names$geom_name
  stat_name <- ggproto_names$stat_name
  position_name <- ggproto_names$position_name

  if (geom_name %in% c("bin2d", "hex")) {
    if (is.null(mapping)) {
      mapping <- ggplot2::aes(colour = ggplot2::after_stat(.data$count))
    }
    else if (!is.null(mapping)) {
      if (!"colour" %in% names(mapping)) {
        mapping <- utils::modifyList(mapping, ggplot2::aes(colour = ggplot2::after_stat(.data$count)))
      }
    }
  }

  if (geom_name %in% c("contour_filled", "density2d_filled")) {
    if (is.null(mapping)) {
      mapping <- ggplot2::aes(colour = ggplot2::after_stat(.data$level))
    }
    else if (!is.null(mapping)) {
      if (!"colour" %in% names(mapping)) {
        mapping <- utils::modifyList(mapping, ggplot2::aes(colour = ggplot2::after_stat(.data$level)))
      }
    }
  }

  ##############################################################################
  # Step 4: Determine scale types
  ##############################################################################

  # Create initial plot to determine scale types
  plot <- create_ggplot(
    data = data,
    x = !!aes_list$x,
    y = !!aes_list$y,
    col = !!aes_list$col,
    xmin = !!aes_list$xmin,
    xmax = !!aes_list$xmax,
    xend = !!aes_list$xend,
    ymin = !!aes_list$ymin,
    ymax = !!aes_list$ymax,
    yend = !!aes_list$yend,
    z = !!aes_list$z,
    group = !!aes_list$group,
    subgroup = !!aes_list$subgroup,
    sample = !!aes_list$sample,
    label = !!aes_list$label,
    text = !!aes_list$text,
    mapping = mapping
  )

  show_legend <- ifelse(geom_name %in% c("blank", "abline"), FALSE, TRUE)

  params <- get_geom_params(geom_name, ...)

  # Add initial layer
  plot <- add_initial_layer(plot, geom, stat, position, params,
                            show_legend, coord, blend, stat_name)

  # Get plot build
  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]
    })
  })

  # Determine scale types
  scale_classs <- get_scale_class(plot_build, aes_list, data)
  x_scale_class <- scale_classs$x_scale_class
  y_scale_class <- scale_classs$y_scale_class
  col_scale_class <- scale_classs$col_scale_class

  ##############################################################################
  # Step 5: Get defaults
  ##############################################################################
  theme <- ggplot2::get_theme()

  defaults <- get_other_defaults(x_transform, y_transform, x_scale_class, y_scale_class,
                                 facet_scales, x_symmetric, y_symmetric,
                                 stat_name, perspective, titles_case)

  x_transform <- defaults$x_transform
  y_transform <- defaults$y_transform
  x_transform_null <- defaults$x_transform_null
  y_transform_null <- defaults$y_transform_null

  x_drop <- defaults$x_drop
  y_drop <- defaults$y_drop

  x_symmetric <- defaults$x_symmetric
  y_symmetric <- defaults$y_symmetric

  perspective <- defaults$perspective
  titles_case <- defaults$titles_case

  ##############################################################################
  # Step 6: Validate inputs
  ##############################################################################
  check_inputs(mapping, x_symmetric, y_symmetric,
               x_transform_null, y_transform_null, stat)

  ##############################################################################
  # Step 7: Process the data
  ##############################################################################
  data <- process_data(data, aes_list, x_symmetric)

  ##############################################################################
  # Step 8: Rebuild base plot with processed data
  ##############################################################################

  plot <- create_ggplot(
    data = data,
    x = !!aes_list$x,
    y = !!aes_list$y,
    col = !!aes_list$col,
    xmin = !!aes_list$xmin,
    xmax = !!aes_list$xmax,
    xend = !!aes_list$xend,
    ymin = !!aes_list$ymin,
    ymax = !!aes_list$ymax,
    yend = !!aes_list$yend,
    z = !!aes_list$z,
    group = !!aes_list$group,
    subgroup = !!aes_list$subgroup,
    sample = !!aes_list$sample,
    label = !!aes_list$label,
    text = !!aes_list$text,
    mapping = mapping
  ) +
    theme

  ##############################################################################
  # Step 9: Add geom layer
  ##############################################################################
  plot <- add_initial_layer(plot, geom, stat, position, params,
                            show_legend, coord, blend, stat_name)

  if (!rlang::is_null(x_limits_include)) {
    plot <- plot + ggplot2::expand_limits(x = x_limits_include)
  }

  if (!rlang::is_null(y_limits_include)) {
    plot <- plot + ggplot2::expand_limits(y = y_limits_include)
  }

  ##############################################################################
  # Step 10: Add facet layer
  ##############################################################################
  facet_layout <- get_facet_layout(facet_layout, aes_list)
  facet_axes <- get_facet_axes(facet_axes, x_symmetric)

  plot <- add_facet_layer(plot, aes_list, data, facet_layout, facet_scales,
                          facet_space, facet_drop, facet_axes, facet_axis_labels,
                          facet_nrow, facet_ncol, facet_labels, y_scale_class)

  # Add default color scales
  if (col_scale_class %in% c("discrete", "ordinal")) {
    plot <- plot +
      ggplot2::scale_colour_hue(na.value = "grey50") +
      ggplot2::scale_fill_hue(na.value = "grey50")
  }

  ##############################################################################
  # Step 11: Get plot build again
  ##############################################################################
  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]

      facet_nrows <- length(unique(plot_build$layout$layout$ROW))
      facet_ncols <- length(unique(plot_build$layout$layout$COL))
    })
  })

  ##############################################################################
  # Step 12: Make colour scale
  ##############################################################################

  if (!is.na(col_scale_class)) {
    # Get theme palettes
    theme_palettes <- ggplot2::get_theme()

    # Define geom categories (matching update_geom_col)
    border_polygon_geoms <- c("area", "bar", "boxplot", "col", "crossbar", "density",
                              "map", "polygon", "rect", "ribbon", "smooth", "sf", "tile",
                              "violin", "raster", "contour_filled", "density2d_filled",
                              "bin2d", "hex")

    border_point_geoms <- c("point", "jitter", "count", "qq")

    # Determine if this geom needs special palette handling
    if (rlang::is_null(col_border)) {
      col_border <- (geom_name %in% border_polygon_geoms) ||
        ((geom_name %in% c("point", "jitter", "count", "qq", "pointrange")) &&
           (ggplot2::get_geom_defaults(geom_name)$shape %in% 21:25))
    }

    if (col_scale_class %in% c("discrete")) {
      if (rlang::is_null(colour_palette)) {
        if (col_border && !rlang::is_null(getOption("ggblanket.colour_palette_d_border"))) {
          colour_palette <- getOption("ggblanket.colour_palette_d_border")
        } else if (!rlang::is_null(theme_palettes$palette.colour.discrete)) {
          colour_palette <- theme_palettes$palette.colour.discrete
        } else {
          colour_palette <- scales::pal_hue()
        }
      }

      if (rlang::is_null(fill_palette)) {
        if (col_border && !rlang::is_null(getOption("ggblanket.fill_palette_d_border"))) {
          fill_palette <- getOption("ggblanket.fill_palette_d_border")
        } else if (!rlang::is_null(theme_palettes$palette.fill.discrete)) {
          fill_palette <- theme_palettes$palette.fill.discrete
        } else {
          fill_palette <- scales::pal_hue()
        }
      }
    }
    else if (col_scale_class %in% c("numeric", "date", "datetime", "time")) {
      if (rlang::is_null(colour_palette)) {
        if (col_border && !rlang::is_null(getOption("ggblanket.colour_palette_c_border"))) {
          colour_palette <- getOption("ggblanket.colour_palette_c_border")
        } else if (!rlang::is_null(theme_palettes$palette.colour.continuous)) {
          colour_palette <- theme_palettes$palette.colour.continuous
        } else {
          colour_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")
        }
      }
      if (rlang::is_null(fill_palette)) {
        if (col_border && !rlang::is_null(getOption("ggblanket.fill_palette_c_border"))) {
          fill_palette <- getOption("ggblanket.fill_palette_c_border")
        } else if (!rlang::is_null(theme_palettes$palette.fill.continuous)) {
          fill_palette <- theme_palettes$palette.fill.continuous
        } else {
          fill_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")
        }
      }
    }
    else if (col_scale_class %in% c("ordinal")) {
      if (rlang::is_null(colour_palette)) {
        if (col_border && !rlang::is_null(getOption("ggblanket.colour_palette_c_border"))) {
          colour_palette <- getOption("ggblanket.colour_palette_c_border")
        } else if (!rlang::is_null(theme_palettes$palette.colour.continuous)) {
          colour_palette <- theme_palettes$palette.colour.continuous
        } else {
          colour_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")
        }
      }
      if (rlang::is_null(fill_palette)) {
        if (col_border && !rlang::is_null(getOption("ggblanket.fill_palette_c_border"))) {
          fill_palette <- getOption("ggblanket.fill_palette_c_border")
        } else if (!rlang::is_null(theme_palettes$palette.fill.continuous)) {
          fill_palette <- theme_palettes$palette.fill.continuous
        } else {
          fill_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")
        }
      }
    }

    # Determine NA colors
    # If user provided NA colors, use them. Otherwise, get from global options based on geom type
    if (!is.null(colour_palette_na)) {
      na_colour <- colour_palette_na
    } else {
      if (col_border) {
        na_colour <- getOption("ggblanket.colour_palette_na_border", "#CDC5BFFF")
      } else {
        na_colour <- getOption("ggblanket.colour_palette_na", "#CDC5BFFF")
      }
    }

    if (!is.null(fill_palette_na)) {
      na_fill <- fill_palette_na
    } else {
      if (col_border) {
        na_fill <- getOption("ggblanket.fill_palette_na_border", "#CDC5BFFF")
      } else {
        na_fill <- getOption("ggblanket.fill_palette_na", "#CDC5BFFF")
      }
    }

    # Get other defaults (transform, breaks, labels, etc.)
    # Get transform
    if (rlang::is_null(col_transform)) {
      col_transform <- get_transform_default(col_scale_class)
    }
    col_transform_name <- get_transform_name(col_transform)

    # Get labels
    if (rlang::is_null(col_labels)) {
      if (col_scale_class %in% c("discrete", "ordinal")) {
        col_labels <- ggplot2::waiver()
      } else if (any(col_transform_name %in% c("hms"))) {
        col_labels <- scales::label_time()
      } else if (any(col_transform_name %in% c("date", "datetime", "time"))) {
        col_labels <- scales::label_date_short(leading = "")
      } else {
        col_labels <- scales::label_comma(drop0trailing = TRUE)
      }
    }

    # Add the scales and guides
    if (col_scale_class == "discrete") {
      # Calculate number of colors needed
      col_n <- calculate_colour_n(aes_list, data, plot_data)

      # Process palettes for discrete scales
      if (!rlang::is_null(colour_palette)) {
        if (rlang::is_function(colour_palette)) {
          if (!rlang::is_null(col_n)) {
            colour_palette_values <- colour_palette(col_n)
          } else {
            colour_palette_values <- colour_palette
          }
        } else if (!any(rlang::have_name(colour_palette))) {
          if (!rlang::is_null(col_n)) {
            colour_palette_values <- colour_palette[1:col_n]
          } else {
            colour_palette_values <- colour_palette
          }
        } else {
          colour_palette_values <- colour_palette
        }
      } else {
        colour_palette_values <- NULL
      }

      if (!rlang::is_null(fill_palette)) {
        if (rlang::is_function(fill_palette)) {
          if (!rlang::is_null(col_n)) {
            fill_palette_values <- fill_palette(col_n)
          } else {
            fill_palette_values <- fill_palette
          }
        } else if (!any(rlang::have_name(fill_palette))) {
          if (!rlang::is_null(col_n)) {
            fill_palette_values <- fill_palette[1:col_n]
          } else {
            fill_palette_values <- fill_palette
          }
        } else {
          fill_palette_values <- fill_palette
        }
      } else {
        fill_palette_values <- NULL
      }

      # Handle x_symmetric reversal
      if (x_symmetric) {
        col_legend_rev <- !col_legend_rev
        if (!rlang::is_null(colour_palette_values)) {
          colour_palette_values <- rev(colour_palette_values)
        }
        if (!rlang::is_null(fill_palette_values)) {
          fill_palette_values <- rev(fill_palette_values)
        }
      }

      # Apply discrete scales
      if (!rlang::is_null(colour_palette_values)) {
        if (rlang::is_vector(colour_palette_values)) {
          plot <- plot +
            ggplot2::scale_colour_manual(
              values = colour_palette_values,
              breaks = col_breaks,
              labels = col_labels,
              na.value = na_colour,
              drop = col_drop
            )
        } else if (rlang::is_function(colour_palette)) {
          plot <- plot +
            ggplot2::discrete_scale(
              aesthetics = "colour",
              palette = colour_palette,
              breaks = col_breaks,
              labels = col_labels,
              na.value = na_colour,
              drop = col_drop
            )
        }
      }

      if (!rlang::is_null(fill_palette_values)) {
        if (rlang::is_vector(fill_palette_values)) {
          plot <- plot +
            ggplot2::scale_fill_manual(
              values = fill_palette_values,
              breaks = col_breaks,
              labels = col_labels,
              na.value = na_fill,
              drop = col_drop
            )
        } else if (rlang::is_function(fill_palette)) {
          plot <- plot +
            ggplot2::discrete_scale(
              aesthetics = "fill",
              palette = fill_palette,
              breaks = col_breaks,
              labels = col_labels,
              na.value = na_fill,
              drop = col_drop
            )
        }
      }

      # Add guides
      plot <- plot +
        ggplot2::guides(
          colour = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          ),
          fill = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          )
        )

    }
    else if (col_scale_class %in% c("numeric", "date", "datetime", "time")) {
      # Process palette functions to get color vectors
      if (rlang::is_function(colour_palette)) {
        # Handle different types of palette functions
        tryCatch({
          # Try standard discrete palette approach
          test_colors <- colour_palette(20)
          if (length(test_colors) == 20) {
            colour_palette_values <- test_colors
          } else {
            # Try gradient approach
            colour_palette_values <- colour_palette(seq(0, 1, length.out = 20))
          }
        }, error = function(e) {
          # Fallback to gradient approach
          tryCatch({
            colour_palette_values <- colour_palette(seq(0, 1, length.out = 20))
          }, error = function(e2) {
            warning("Could not determine palette function type, using default colors")
            colour_palette_values <- scales::viridis_pal()(20)
          })
        })
      } else {
        colour_palette_values <- colour_palette
      }

      if (rlang::is_function(fill_palette)) {
        # Handle different types of palette functions
        tryCatch({
          # Try standard discrete palette approach
          test_colors <- fill_palette(20)
          if (length(test_colors) == 20) {
            fill_palette_values <- test_colors
          } else {
            # Try gradient approach
            fill_palette_values <- fill_palette(seq(0, 1, length.out = 20))
          }
        }, error = function(e) {
          # Fallback to gradient approach
          tryCatch({
            fill_palette_values <- fill_palette(seq(0, 1, length.out = 20))
          }, error = function(e2) {
            warning("Could not determine palette function type, using default colors")
            fill_palette_values <- scales::viridis_pal()(20)
          })
        })
      } else {
        fill_palette_values <- fill_palette
      }

      # Apply continuous scales
      if (col_scale_type == "gradient") {
        plot <- plot +
          ggplot2::scale_colour_gradientn(
            colours = colour_palette_values,
            values = col_rescale,
            breaks = col_breaks,
            n.breaks = col_breaks_n,
            labels = col_labels,
            transform = col_transform,
            oob = scales::oob_keep,
            na.value = na_colour
          ) +
          ggplot2::scale_fill_gradientn(
            colours = fill_palette_values,
            values = col_rescale,
            breaks = col_breaks,
            n.breaks = col_breaks_n,
            labels = col_labels,
            transform = col_transform,
            oob = scales::oob_keep,
            na.value = na_fill
          )

        if (col_border) {
          plot <- plot +
            ggplot2::guides(
              colour = ggplot2::guide_none(),
              fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
            )
        }
        else {
          plot <- plot +
            ggplot2::guides(
              colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
              fill = ggplot2::guide_none()
            )
        }
      }
      else if (col_scale_type == "steps") {
        plot <- plot +
          ggplot2::scale_colour_stepsn(
            colours = colour_palette_values,
            values = col_rescale,
            breaks = col_breaks,
            n.breaks = col_breaks_n,
            labels = col_labels,
            transform = col_transform,
            oob = scales::oob_keep,
            na.value = na_colour
          ) +
          ggplot2::scale_fill_stepsn(
            colours = fill_palette_values,
            values = col_rescale,
            breaks = col_breaks,
            n.breaks = col_breaks_n,
            labels = col_labels,
            transform = col_transform,
            oob = scales::oob_keep,
            na.value = na_fill
          )

        if (!identical(colour_palette, fill_palette)) {
          plot <- plot +
            ggplot2::guides(
              colour = ggplot2::guide_none(),
              fill = ggplot2::guide_coloursteps(
                reverse = col_legend_rev,
                theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
              )
            )
        }
        else {
          plot <- plot +
            ggplot2::guides(
              colour = ggplot2::guide_coloursteps(
                reverse = col_legend_rev,
                theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
              ),
              fill = ggplot2::guide_none()
            )
        }
      }
    }
    else if (col_scale_class == "ordinal") {
      # Calculate number of colors needed
      col_n <- calculate_colour_n(aes_list, data, plot_data)

      # Convert vector palettes to functions for ordinal
      if (rlang::is_vector(colour_palette)) {
        colour_palette <- scales::pal_gradient_n(colours = colour_palette)
      }
      if (rlang::is_vector(fill_palette)) {
        fill_palette <- scales::pal_gradient_n(colours = fill_palette)
      }

      # Always reverse legend for ordinal
      col_legend_rev <- !col_legend_rev

      # Create wrapper functions for ordinal scales
      if (rlang::is_function(colour_palette)) {
        colour_palette_discrete <- function(n) {
          if (n == 0) return(character(0))
          tryCatch({
            colours <- colour_palette(n)
            if (length(colours) == n) {
              return(colours)
            } else {
              return(colour_palette(seq(0, 1, length.out = n)))
            }
          }, error = function(e) {
            tryCatch({
              return(colour_palette(seq(0, 1, length.out = n)))
            }, error = function(e2) {
              warning("Palette function failed, using default colors")
              return(scales::viridis_pal()(n))
            })
          })
        }

        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "colour",
            palette = colour_palette_discrete,
            breaks = col_breaks,
            labels = col_labels,
            na.value = na_colour,
            drop = col_drop
          )
      }

      if (rlang::is_function(fill_palette)) {
        fill_palette_discrete <- function(n) {
          if (n == 0) return(character(0))
          tryCatch({
            colours <- fill_palette(n)
            if (length(colours) == n) {
              return(colours)
            } else {
              return(fill_palette(seq(0, 1, length.out = n)))
            }
          }, error = function(e) {
            tryCatch({
              return(fill_palette(seq(0, 1, length.out = n)))
            }, error = function(e2) {
              warning("Palette function failed, using default colors")
              return(scales::viridis_pal()(n))
            })
          })
        }

        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "fill",
            palette = fill_palette_discrete,
            breaks = col_breaks,
            labels = col_labels,
            na.value = na_fill,
            drop = col_drop
          )
      }

      # Add guides
      plot <- plot +
        ggplot2::guides(
          colour = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          ),
          fill = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          )
        )
    }

    # Handle guides for other aesthetics that match color
    for (aes in c("alpha", "shape", "size", "linewidth", "linetype", "pattern")) {
      if (!rlang::is_null(plot_build$plot$labels[[aes]])) {
        if (check_aesthetic_matches_colour(plot_build, aes)) {
          plot <- plot +
            ggplot2::guides(
              !!aes := ggplot2::guide_legend(
                reverse = col_legend_rev,
                ncol = col_legend_ncol,
                nrow = col_legend_nrow
              )
            )
        }
      }
    }

    # Expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        colour = col_limits_include,
        fill = col_limits_include
      )
  }

  ##############################################################################
  # Step 13: Add positional scales
  ##############################################################################

  # Make x scale
  if (x_scale_class == "discrete") {
    if (rlang::is_null(x_expand)) {
      x_expand <- ggplot2::waiver()
    }
    if (rlang::is_null(x_labels)) {
      x_labels <- ggplot2::waiver()
    }
    if (rlang::is_null(x_breaks)) {
      x_breaks <- ggplot2::waiver()
    }

    plot <- plot +
      ggplot2::scale_x_discrete(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        drop = x_drop,
        position = x_position
      )
  } else {
    if (stringr::str_detect(stat_name, "sf")) {
      if (rlang::is_null(x_breaks)) {
        x_breaks <- ggplot2::waiver()
      }
      if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    }

    if (rlang::is_null(x_breaks_n)) {
      if (facet_ncols == 1) {
        x_breaks_n <- 6
      } else {
        x_breaks_n <- 4
      }
    }

    if (x_symmetric) {
      data_x <- plot_data |>
        dplyr::select(tidyselect::matches(stringr::regex(
          "^(?!xid|xbin)x.*"
        ))) |>
        tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "x") |>
        dplyr::filter(!is.na(.data$x))

      plot <- plot +
        scale_x_symmetric(
          data = data_x,
          x = x,
          symmetric = TRUE,
          breaks = x_breaks,
          breaks_n = x_breaks_n,
          expand = x_expand,
          expand_limits = x_limits_include,
          labels = x_labels,
          position = x_position,
          sec_axis = x_sec_axis,
          transform = x_transform
        )
    } else {
      plot <- plot +
        scale_x_symmetric(
          symmetric = FALSE,
          breaks = x_breaks,
          breaks_n = x_breaks_n,
          expand = x_expand,
          expand_limits = x_limits_include,
          labels = x_labels,
          position = x_position,
          sec_axis = x_sec_axis,
          transform = x_transform
        )
    }
  }

  # Make y scale
  if (y_scale_class == "discrete") {
    if (rlang::is_null(y_expand)) {
      y_expand <- ggplot2::waiver()
    }
    if (rlang::is_null(y_labels)) {
      y_labels <- ggplot2::waiver()
    }
    if (rlang::is_null(y_breaks)) {
      y_breaks <- ggplot2::waiver()
    }

    plot <- plot +
      ggplot2::scale_y_discrete(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels,
        drop = y_drop,
        position = y_position
      )
  } else {
    if (stringr::str_detect(stat_name, "sf")) {
      if (rlang::is_null(y_breaks)) {
        y_breaks <- ggplot2::waiver()
      }
      if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    }

    if (rlang::is_null(y_breaks_n)) {
      if (facet_nrows == 1) {
        y_breaks_n <- 6
      } else {
        y_breaks_n <- 4
      }
    }

    if (y_symmetric) {
      data_y <- plot_data |>
        dplyr::select(tidyselect::matches(stringr::regex(
          "^(?!yid|ybin)y.*"
        ))) |>
        tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "y") |>
        dplyr::filter(!is.na(.data$y))

      plot <- plot +
        scale_y_symmetric(
          data = data_y,
          y = y,
          symmetric = TRUE,
          breaks = y_breaks,
          breaks_n = y_breaks_n,
          expand = y_expand,
          expand_limits = y_limits_include,
          labels = y_labels,
          position = y_position,
          sec_axis = y_sec_axis,
          transform = y_transform
        )
    } else {
      plot <- plot +
        scale_y_symmetric(
          symmetric = FALSE,
          breaks = y_breaks,
          breaks_n = y_breaks_n,
          expand = y_expand,
          expand_limits = y_limits_include,
          labels = y_labels,
          position = y_position,
          sec_axis = y_sec_axis,
          transform = y_transform
        )
    }
  }

  #############################################################################
  # Step 14: Get titles
  #############################################################################

  if (rlang::is_null(x_title)) {
    if (stringr::str_detect(stat_name, "sf")) {
      x_title <- ""
    } else {
      x_title <- get_title(
        data, aes_list$x, plot_build$plot$labels$x,
        titles_case,
        purrr::map_chr("x", titles_case)
      )
    }
  }

  if (rlang::is_null(y_title)) {
    if (stringr::str_detect(stat_name, "sf")) {
      y_title <- ""
    } else {
      y_title <- get_title(
        data, aes_list$y, plot_build$plot$labels$y,
        titles_case, purrr::map_chr("y", titles_case)
      )
    }
  }

  if (rlang::is_null(col_title)) {
    if (!rlang::is_null(plot_build$plot$labels$colour)) {
      col_title <- get_title(
        data, aes_list$col, plot_build$plot$labels$colour,
        titles_case, NULL
      )
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill)) {
      col_title <- get_title(
        data, aes_list$col, plot_build$plot$labels$fill,
        titles_case, NULL
      )
    }
  }

  # Get labels for other aesthetics
  other_titles <- get_titles2(plot_build, col_title, titles_case)

  # Apply labels
  plot <- plot +
    ggplot2::labs(
      x = x_title,
      y = y_title,
      colour = col_title,
      fill = col_title,
      alpha = other_titles$alpha_title,
      shape = other_titles$shape_title,
      size = other_titles$size_title,
      linewidth = other_titles$linewidth_title,
      linetype = other_titles$linetype_title,
      pattern = other_titles$pattern_title
    )

  # Add title, subtitle, caption
  if (!rlang::is_null(title)) {
    plot <- plot +
      ggplot2::labs(
        title = title
      )
  }
  if (!rlang::is_null(subtitle)) {
    plot <- plot +
      ggplot2::labs(
        subtitle = subtitle
      )
  }
  if (!rlang::is_null(caption)) {
    plot <- plot +
      ggplot2::labs(
        caption = caption
      )
  }

  ##############################################################################
  # Step 15: Apply theme transparency
  ##############################################################################

  transparency <- get_perspective_behaviour(axis_line_transparent,
                                            axis_ticks_transparent,
                                            panel_grid_transparent)

  plot <- add_perspective(
    plot, perspective,
    transparency$axis_line_transparent,
    transparency$axis_ticks_transparent,
    transparency$panel_grid_transparent,
    x_scale_class, y_scale_class
  )

  ##############################################################################
  # Step X: Apply theme transparency
  ##############################################################################

  is_shape_mapping <- !is.null(mapping) && "shape" %in% names(mapping)

  if (is_shape_mapping) {
    plot <- plot +
      ggplot2::scale_shape_manual(values = 21:25)
  }

  return(plot)
}

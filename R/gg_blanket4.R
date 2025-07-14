#' Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()`
#' with [geom_blank()][ggplot2::geom_blank()] defaults for the geom, stat and position.
#'
#' This function underlies all other `gg_*` functions.
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

  # Step 1: Quote aesthetics
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

  # Step 2: Handle NULL data
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }

  # Step 3: Extract geom, stat & position strings
  ggproto_strings <- get_ggproto_strings_list(geom = geom, stat = stat, transform = transform)
  geom <- ggproto_strings$geom
  stat <- ggproto_strings$stat
  transform <- ggproto_strings$transform

  # Update mapping for specific geoms
  if (geom %in% c("bin2d", "hex")) {
    default_aes <- ggplot2::aes(colour = ggplot2::after_stat(.data$count))
    if (is.null(mapping)) {
      mapping <- default_aes
    } else if (!"colour" %in% names(mapping)) {
      mapping <- utils::modifyList(mapping, default_aes)
    }
  }

  if (geom %in% c("contour_filled", "density2d_filled")) {
    default_aes <- ggplot2::aes(colour = ggplot2::after_stat(.data$level))
    if (is.null(mapping)) {
      mapping <- default_aes
    } else if (!"colour" %in% names(mapping)) {
      mapping <- utils::modifyList(mapping, default_aes)
    }
  }

  # Step 4: Determine scale types
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

  show_legend <- !(geom %in% c("blank", "abline"))
  params <- get_geom_params(geom, ...)

  # Add initial layer
  plot <- add_initial_layer(plot, geom, stat, position, params,
                            show_legend, coord, blend)

  # Get plot build
  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]
    })
  })

  # Determine scale types
  scale_class <- get_scale_class(plot_build, aes_list, data)

  x_scale_class <- scale_class$x_scale_class
  y_scale_class <- scale_class$y_scale_class
  col_scale_class <- scale_class$col_scale_class

  # Step 5: Get defaults
  x_drop <- facet_scales %in% c("free_x", "free")
  y_drop <- facet_scales %in% c("free_y", "free")

  x_transform <- get_transform(x_transform, scale_class = x_scale_class)
  y_transform <- get_transform(y_transform, scale_class = y_scale_class)

  x_symmetric <- is_x_symmetric(x_symmetric,
                                stat = stat,
                                facet_scales = facet_scales,
                                x_scale_class = x_scale_class,
                                y_scale_class = y_scale_class)

  y_symmetric <- is_y_symmetric(y_symmetric,
                                stat = stat,
                                facet_scales = facet_scales,
                                x_scale_class = x_scale_class,
                                y_scale_class = y_scale_class)

  titles_case <- get_titles_case(titles_case = titles_case)

  perspective <- get_perspective(perspective = perspective,
                                 x_scale_class = x_scale_class,
                                 y_scale_class = y_scale_class)

  # Step 6: Check inputs
  check_inputs(mapping, x_symmetric, y_symmetric,
               x_transform, y_transform, stat)

  # Step 7: Process the data
  data <- process_data(data, aes_list, x_symmetric)

  # Step 8: Rebuild base plot with processed data
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

  # Step 9: Add geom layer
  plot <- add_initial_layer(plot, geom, stat, position, params,
                            show_legend, coord, blend)

  if (!is.null(x_limits_include)) {
    plot <- plot + ggplot2::expand_limits(x = x_limits_include)
  }

  if (!is.null(y_limits_include)) {
    plot <- plot + ggplot2::expand_limits(y = y_limits_include)
  }

  # Step 10: Add facet layer
  facet_layout <- get_facet_layout(facet_layout, aes_list)
  facet_axes <- get_facet_axes(facet_axes, x_symmetric)

  plot <- add_facet_layer(plot, aes_list, data, facet_layout, facet_scales,
                          facet_space, facet_drop, facet_axes, facet_axis_labels,
                          facet_nrow, facet_ncol, facet_labels, y_scale_class)

  # Step 11: Get plot build again
  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]

      facet_nrows <- length(unique(plot_build$layout$layout$ROW))
      facet_ncols <- length(unique(plot_build$layout$layout$COL))
    })
  })

  # Step 12: Make colour scale
  if (!is.na(col_scale_class)) {
    plot <- add_color_scales(
      plot = plot,
      geom = geom,
      col_scale_class = col_scale_class,
      col_border = col_border,
      aes_list = aes_list,
      data = data,
      plot_data = plot_data,
      plot_build = plot_build,
      x_symmetric = x_symmetric,
      col_breaks = col_breaks,
      col_breaks_n = col_breaks_n,
      col_drop = col_drop,
      col_limits_include = col_limits_include,
      col_labels = col_labels,
      col_legend_ncol = col_legend_ncol,
      col_legend_nrow = col_legend_nrow,
      col_legend_rev = col_legend_rev,
      col_rescale = col_rescale,
      col_scale_type = col_scale_type,
      col_transform = col_transform,
      colour_palette = colour_palette,
      colour_palette_na = colour_palette_na,
      fill_palette = fill_palette,
      fill_palette_na = fill_palette_na
    )
  }

  # Step 13: Add positional scales
  # Make x scale
  if (x_scale_class == "discrete") {
    if (is.null(x_expand)) x_expand <- ggplot2::waiver()
    if (is.null(x_labels)) x_labels <- ggplot2::waiver()
    if (is.null(x_breaks)) x_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_x_discrete(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        drop = x_drop,
        position = x_position
      )
  } else {
    plot <- plot |>
      add_continuous_x_scale(
        stat = stat,
        x_breaks = x_breaks,
        x_breaks_n = x_breaks_n %||% if (facet_ncols == 1) 6 else 4,
        x_labels = x_labels,
        x_expand = x_expand,
        x_limits_include = x_limits_include,
        x_position = x_position,
        x_sec_axis = x_sec_axis,
        x_symmetric = x_symmetric,
        x_transform = x_transform,
        plot_data = plot_data
      )
  }

  # Make y scale
  if (y_scale_class == "discrete") {
    if (is.null(y_expand)) y_expand <- ggplot2::waiver()
    if (is.null(y_labels)) y_labels <- ggplot2::waiver()
    if (is.null(y_breaks)) y_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_y_discrete(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels,
        drop = y_drop,
        position = y_position
      )
  } else {
    plot <- plot |>
      add_continuous_y_scale(
        stat = stat,
        y_breaks = y_breaks,
        y_breaks_n = y_breaks_n %||% if (facet_nrows == 1) 6 else 4,
        y_labels = y_labels,
        y_expand = y_expand,
        y_limits_include = y_limits_include,
        y_position = y_position,
        y_sec_axis = y_sec_axis,
        y_symmetric = y_symmetric,
        y_transform = y_transform,
        plot_data = plot_data
      )
  }

  # Step 14: Get titles
  x_title <- x_title %||% get_axis_title(
    data, aes_list$x, plot_build$plot$labels$x,
    titles_case, stat, "x"
  )

  y_title <- y_title %||% get_axis_title(
    data, aes_list$y, plot_build$plot$labels$y,
    titles_case, stat, "y"
  )

  if (is.null(col_title)) {
    col_title <- get_col_title(
      data, aes_list$col, plot_build$plot$labels,
      titles_case
    )
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
      pattern = other_titles$pattern_title,
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # Step 15: Apply theme transparency
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

  # Handle shape mapping
  if (!is.null(mapping) && "shape" %in% names(mapping)) {
    plot <- plot + ggplot2::scale_shape_manual(values = 21:25)
  }

  return(plot)
}

# Helper functions for scales ----

#' Add continuous x scale
#' @noRd
add_continuous_x_scale <- function(
    plot, stat, x_breaks, x_breaks_n, x_labels, x_expand,
    x_limits_include, x_position, x_sec_axis, x_symmetric,
    x_transform, plot_data
) {
  # Handle sf special case
  if (stringr::str_detect(stat, "sf")) {
    x_breaks <- x_breaks %||% ggplot2::waiver()
    x_labels <- x_labels %||% ggplot2::waiver()
  }

  if (x_symmetric) {
    data_x <- plot_data |>
      dplyr::select(tidyselect::matches(stringr::regex("^(?!xid|xbin)x.*"))) |>
      tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "x") |>
      dplyr::filter(!is.na(.data$x))

    plot +
      scale_x_symmetric(
        data = data_x,
        x = NULL,  # Not needed since data already has 'x' column
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
    plot +
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

#' Add continuous y scale
#' @noRd
add_continuous_y_scale <- function(
    plot, stat, y_breaks, y_breaks_n, y_labels, y_expand,
    y_limits_include, y_position, y_sec_axis, y_symmetric,
    y_transform, plot_data
) {
  # Handle sf special case
  if (stringr::str_detect(stat, "sf")) {
    y_breaks <- y_breaks %||% ggplot2::waiver()
    y_labels <- y_labels %||% ggplot2::waiver()
  }

  if (y_symmetric) {
    data_y <- plot_data |>
      dplyr::select(tidyselect::matches(stringr::regex("^(?!yid|ybin)y.*"))) |>
      tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "y") |>
      dplyr::filter(!is.na(.data$y))

    plot +
      scale_y_symmetric(
        data = data_y,
        y = NULL,  # Not needed since data already has 'y' column
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
    plot +
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

#' Get axis title
#' @noRd
get_axis_title <- function(data, aes_quo, build_label, titles_case, stat, axis) {
  if (stringr::str_detect(stat, "sf")) {
    return("")
  }

  get_title(
    data, aes_quo, build_label,
    titles_case,
    purrr::map_chr(axis, titles_case)
  )
}

#' Get color title
#' @noRd
get_col_title <- function(data, col_quo, labels, titles_case) {
  if (!is.null(labels$colour)) {
    get_title(data, col_quo, labels$colour, titles_case, NULL)
  } else if (!is.null(labels$fill)) {
    get_title(data, col_quo, labels$fill, titles_case, NULL)
  } else {
    NULL
  }
}

#' Add color scales
#' @noRd
add_color_scales <- function(
    plot, geom, col_scale_class, col_border, aes_list, data, plot_data,
    plot_build, x_symmetric, col_breaks, col_breaks_n, col_drop,
    col_limits_include, col_labels, col_legend_ncol, col_legend_nrow,
    col_legend_rev, col_rescale, col_scale_type, col_transform,
    colour_palette, colour_palette_na, fill_palette, fill_palette_na
) {
  # Get theme palettes
  theme_palettes <- ggplot2::get_theme()

  # Determine col_border
  if (is.null(col_border)) {
    border_polygon_geoms <- c(
      "area", "bar", "boxplot", "col", "crossbar", "density",
      "map", "polygon", "rect", "ribbon", "smooth", "sf", "tile",
      "violin", "raster", "contour_filled", "density2d_filled",
      "bin2d", "hex"
    )

    col_border <- (geom %in% border_polygon_geoms) ||
      ((geom %in% c("point", "jitter", "count", "qq", "pointrange")) &&
         (ggplot2::get_geom_defaults(geom)$shape %in% 21:25))
  }

  # Get palettes and NA colors
  palettes <- get_color_palettes(
    col_scale_class, col_border, theme_palettes,
    colour_palette, fill_palette,
    colour_palette_na, fill_palette_na
  )

  # Get transform and labels
  col_transform <- get_transform(col_transform, scale_class = col_scale_class)
  col_labels <- get_col_labels(col_labels, col_scale_class, col_transform)

  # Apply scales based on type
  if (col_scale_class == "discrete") {
    plot <- add_discrete_color_scale(
      plot, aes_list, data, plot_data, palettes,
      col_breaks, col_labels, col_drop, col_legend_ncol,
      col_legend_nrow, col_legend_rev, x_symmetric
    )
  } else if (col_scale_class %in% c("numeric", "date", "datetime", "time")) {
    plot <- add_continuous_color_scale(
      plot, palettes, col_border, col_breaks, col_breaks_n,
      col_labels, col_legend_rev, col_rescale, col_scale_type,
      col_transform
    )
  } else if (col_scale_class == "ordinal") {
    plot <- add_ordinal_color_scale(
      plot, aes_list, data, plot_data, palettes,
      col_breaks, col_labels, col_drop, col_legend_ncol,
      col_legend_nrow, col_legend_rev
    )
  }

  # Handle guides for other aesthetics
  plot <- add_matching_aesthetic_guides(
    plot, plot_build, col_legend_rev,
    col_legend_ncol, col_legend_nrow
  )

  # Expand limits if necessary
  if (!is.null(col_limits_include)) {
    plot <- plot + ggplot2::expand_limits(
      colour = col_limits_include,
      fill = col_limits_include
    )
  }

  plot
}

#' Get color palettes
#' @noRd
get_color_palettes <- function(
    col_scale_class, col_border, theme_palettes,
    colour_palette, fill_palette,
    colour_palette_na, fill_palette_na
) {
  if (col_scale_class == "discrete") {
    # Discrete palettes
    if (is.null(colour_palette)) {
      if (col_border && !is.null(getOption("ggblanket.colour_palette_d_border"))) {
        colour_palette <- getOption("ggblanket.colour_palette_d_border")
      } else if (!is.null(theme_palettes$palette.colour.discrete)) {
        colour_palette <- theme_palettes$palette.colour.discrete
      } else {
        colour_palette <- scales::pal_hue()
      }
    }

    if (is.null(fill_palette)) {
      if (col_border && !is.null(getOption("ggblanket.fill_palette_d_border"))) {
        fill_palette <- getOption("ggblanket.fill_palette_d_border")
      } else if (!is.null(theme_palettes$palette.fill.discrete)) {
        fill_palette <- theme_palettes$palette.fill.discrete
      } else {
        fill_palette <- scales::pal_hue()
      }
    }
  } else {
    # Continuous palettes
    if (is.null(colour_palette)) {
      if (col_border && !is.null(getOption("ggblanket.colour_palette_c_border"))) {
        colour_palette <- getOption("ggblanket.colour_palette_c_border")
      } else if (!is.null(theme_palettes$palette.colour.continuous)) {
        colour_palette <- theme_palettes$palette.colour.continuous
      } else {
        colour_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")
      }
    }

    if (is.null(fill_palette)) {
      if (col_border && !is.null(getOption("ggblanket.fill_palette_c_border"))) {
        fill_palette <- getOption("ggblanket.fill_palette_c_border")
      } else if (!is.null(theme_palettes$palette.fill.continuous)) {
        fill_palette <- theme_palettes$palette.fill.continuous
      } else {
        fill_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")
      }
    }
  }

  # NA colors
  na_colour <- colour_palette_na %||%
    if (col_border) {
      getOption("ggblanket.colour_palette_na_border", "#CDC5BFFF")
    } else {
      getOption("ggblanket.colour_palette_na", "#CDC5BFFF")
    }

  na_fill <- fill_palette_na %||%
    if (col_border) {
      getOption("ggblanket.fill_palette_na_border", "#CDC5BFFF")
    } else {
      getOption("ggblanket.fill_palette_na", "#CDC5BFFF")
    }

  list(
    colour_palette = colour_palette,
    fill_palette = fill_palette,
    na_colour = na_colour,
    na_fill = na_fill
  )
}

#' Get color labels
#' @noRd
get_col_labels <- function(col_labels, col_scale_class, col_transform) {
  if (!is.null(col_labels)) {
    return(col_labels)
  }

  if (col_scale_class %in% c("discrete", "ordinal")) {
    ggplot2::waiver()
  } else if (col_transform == "hms") {
    scales::label_time()
  } else if (col_transform %in% c("date", "datetime", "time")) {
    scales::label_date_short(leading = "")
  } else {
    scales::label_comma(drop0trailing = TRUE)
  }
}

#' Add discrete color scale
#' @noRd
add_discrete_color_scale <- function(
    plot, aes_list, data, plot_data, palettes,
    col_breaks, col_labels, col_drop, col_legend_ncol,
    col_legend_nrow, col_legend_rev, x_symmetric
) {
  # Calculate number of colors needed
  col_n <- calculate_colour_n(aes_list, data, plot_data)

  # Process palettes
  colour_palette_values <- process_discrete_palette(
    palettes$colour_palette, col_n
  )

  fill_palette_values <- process_discrete_palette(
    palettes$fill_palette, col_n
  )

  # Handle x_symmetric reversal
  if (x_symmetric) {
    col_legend_rev <- !col_legend_rev
    if (!is.null(colour_palette_values)) {
      colour_palette_values <- rev(colour_palette_values)
    }
    if (!is.null(fill_palette_values)) {
      fill_palette_values <- rev(fill_palette_values)
    }
  }

  # Apply scales
  if (!is.null(colour_palette_values)) {
    if (is.vector(colour_palette_values)) {
      plot <- plot +
        ggplot2::scale_colour_manual(
          values = colour_palette_values,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_colour,
          drop = col_drop
        )
    } else if (is.function(palettes$colour_palette)) {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "colour",
          palette = palettes$colour_palette,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_colour,
          drop = col_drop
        )
    }
  }

  if (!is.null(fill_palette_values)) {
    if (is.vector(fill_palette_values)) {
      plot <- plot +
        ggplot2::scale_fill_manual(
          values = fill_palette_values,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_fill,
          drop = col_drop
        )
    } else if (is.function(palettes$fill_palette)) {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "fill",
          palette = palettes$fill_palette,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_fill,
          drop = col_drop
        )
    }
  }

  # Add guides
  plot +
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

#' Process discrete palette
#' @noRd
process_discrete_palette <- function(palette, col_n) {
  if (is.null(palette)) {
    return(NULL)
  }

  if (is.function(palette)) {
    if (!is.null(col_n)) {
      palette(col_n)
    } else {
      palette
    }
  } else if (!any(rlang::have_name(palette))) {
    if (!is.null(col_n)) {
      palette[1:col_n]
    } else {
      palette
    }
  } else {
    palette
  }
}

#' Add continuous color scale
#' @noRd
add_continuous_color_scale <- function(
    plot, palettes, col_border, col_breaks, col_breaks_n,
    col_labels, col_legend_rev, col_rescale, col_scale_type,
    col_transform
) {
  # Process palette functions to get color vectors
  colour_palette_values <- process_continuous_palette(palettes$colour_palette)
  fill_palette_values <- process_continuous_palette(palettes$fill_palette)

  # Apply scales
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
        na.value = palettes$na_colour
      ) +
      ggplot2::scale_fill_gradientn(
        colours = fill_palette_values,
        values = col_rescale,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = palettes$na_fill
      )

    # Add guides
    if (col_border) {
      plot <- plot +
        ggplot2::guides(
          colour = ggplot2::guide_none(),
          fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
        )
    } else {
      plot <- plot +
        ggplot2::guides(
          colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
          fill = ggplot2::guide_none()
        )
    }
  } else if (col_scale_type == "steps") {
    plot <- plot +
      ggplot2::scale_colour_stepsn(
        colours = colour_palette_values,
        values = col_rescale,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = palettes$na_colour
      ) +
      ggplot2::scale_fill_stepsn(
        colours = fill_palette_values,
        values = col_rescale,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = palettes$na_fill
      )

    # Add guides
    if (!identical(palettes$colour_palette, palettes$fill_palette)) {
      plot <- plot +
        ggplot2::guides(
          colour = ggplot2::guide_none(),
          fill = ggplot2::guide_coloursteps(
            reverse = col_legend_rev,
            theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
          )
        )
    } else {
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

  plot
}

#' Process continuous palette
#' @noRd
process_continuous_palette <- function(palette) {
  if (is.function(palette)) {
    tryCatch({
      # Try standard discrete palette approach
      test_colors <- palette(20)
      if (length(test_colors) == 20) {
        test_colors
      } else {
        # Try gradient approach
        palette(seq(0, 1, length.out = 20))
      }
    }, error = function(e) {
      # Fallback to gradient approach
      tryCatch({
        palette(seq(0, 1, length.out = 20))
      }, error = function(e2) {
        warning("Could not determine palette function type, using default colors")
        scales::viridis_pal()(20)
      })
    })
  } else {
    palette
  }
}

#' Add ordinal color scale
#' @noRd
add_ordinal_color_scale <- function(
    plot, aes_list, data, plot_data, palettes,
    col_breaks, col_labels, col_drop, col_legend_ncol,
    col_legend_nrow, col_legend_rev
) {
  # Calculate number of colors needed
  col_n <- calculate_colour_n(aes_list, data, plot_data)

  # Convert vector palettes to functions for ordinal
  if (is.vector(palettes$colour_palette)) {
    colour_palette <- scales::pal_gradient_n(colours = palettes$colour_palette)
  } else {
    colour_palette <- palettes$colour_palette
  }

  if (is.vector(palettes$fill_palette)) {
    fill_palette <- scales::pal_gradient_n(colours = palettes$fill_palette)
  } else {
    fill_palette <- palettes$fill_palette
  }

  # Always reverse legend for ordinal
  col_legend_rev <- !col_legend_rev

  # Create wrapper functions for ordinal scales
  if (is.function(colour_palette)) {
    colour_palette_discrete <- create_ordinal_palette_wrapper(colour_palette)

    plot <- plot +
      ggplot2::discrete_scale(
        aesthetics = "colour",
        palette = colour_palette_discrete,
        breaks = col_breaks,
        labels = col_labels,
        na.value = palettes$na_colour,
        drop = col_drop
      )
  }

  if (is.function(fill_palette)) {
    fill_palette_discrete <- create_ordinal_palette_wrapper(fill_palette)

    plot <- plot +
      ggplot2::discrete_scale(
        aesthetics = "fill",
        palette = fill_palette_discrete,
        breaks = col_breaks,
        labels = col_labels,
        na.value = palettes$na_fill,
        drop = col_drop
      )
  }

  # Add guides
  plot +
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

#' Create ordinal palette wrapper
#' @noRd
create_ordinal_palette_wrapper <- function(palette) {
  function(n) {
    if (n == 0) return(character(0))
    tryCatch({
      colours <- palette(n)
      if (length(colours) == n) {
        return(colours)
      } else {
        return(palette(seq(0, 1, length.out = n)))
      }
    }, error = function(e) {
      tryCatch({
        return(palette(seq(0, 1, length.out = n)))
      }, error = function(e2) {
        warning("Palette function failed, using default colors")
        return(scales::viridis_pal()(n))
      })
    })
  }
}

#' Add matching aesthetic guides
#' @noRd
add_matching_aesthetic_guides <- function(
    plot, plot_build, col_legend_rev,
    col_legend_ncol, col_legend_nrow
) {
  aesthetics <- c("alpha", "shape", "size", "linewidth", "linetype", "pattern")

  for (aes in aesthetics) {
    if (!is.null(plot_build$plot$labels[[aes]])) {
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

  plot
}

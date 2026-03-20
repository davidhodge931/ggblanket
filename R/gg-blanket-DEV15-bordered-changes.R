# Updated gg_blanket function with palette wrappers
gg_blanket <- function(data,
                       ...,
                       # geom
                       geom = "blank",
                       stat = "identity",
                       position = ggplot2::position_identity(),
                       blend = NULL,
                       # annotate under geom
                       annotate = NULL,
                       # colour/linewidth defaults & default colour scale
                       is_bordered_colour = NULL,
                       is_bordered_linewidth = NULL,
                       # theme
                       focus = NULL,
                       polish = NULL,
                       # aesthetics
                       x = NULL,
                       xmin = NULL,
                       xmax = NULL,
                       xend = NULL,
                       xintercept = NULL,
                       y = NULL,
                       ymin = NULL,
                       ymax = NULL,
                       yend = NULL,
                       yintercept = NULL,
                       z = NULL,
                       fill = NULL,
                       colour = NULL,
                       alpha = NULL,
                       shape = NULL,
                       linetype = NULL,
                       linewidth = NULL,
                       size = NULL,
                       stroke = NULL,
                       label = NULL,
                       weight = NULL,
                       group = NULL,
                       width = NULL,
                       height = NULL,
                       slope = NULL,
                       intercept = NULL,
                       sample = NULL,
                       angle = NULL,
                       radius = NULL,
                       mapping = ggplot2::aes(),
                       # x scale
                       x_type = NULL,
                       x_temporal = NULL,
                       x_breaks = NULL,
                       x_drop = TRUE,
                       x_expand = NULL,
                       x_guide = ggplot2::waiver(),
                       x_labels = NULL,
                       x_limits = NULL,
                       x_minor_breaks = ggplot2::waiver(),
                       x_name = ggplot2::waiver(),
                       x_oob = scales::oob_censor,
                       x_palette = seq_len,
                       x_position = "bottom",
                       x_sec_axis = ggplot2::waiver(),
                       x_transform = NULL,
                       # y scale
                       y_type = NULL,
                       y_temporal = NULL,
                       y_breaks = NULL,
                       y_drop = TRUE,
                       y_expand = NULL,
                       y_guide = ggplot2::waiver(),
                       y_labels = NULL,
                       y_limits = NULL,
                       y_minor_breaks = ggplot2::waiver(),
                       y_name = ggplot2::waiver(),
                       y_oob = scales::oob_censor,
                       y_palette = seq_len,
                       y_position = "left",
                       y_sec_axis = ggplot2::waiver(),
                       y_transform = NULL,
                       # fill scale
                       fill_type = NULL,
                       fill_breaks = ggplot2::waiver(),
                       fill_drop = TRUE,
                       fill_guide = NULL,
                       fill_labels = NULL,
                       fill_limits = NULL,
                       fill_name = ggplot2::waiver(),
                       fill_oob = scales::oob_censor,
                       fill_rescaler = scales::rescale,
                       fill_palette = NULL,
                       fill_transform = "identity",
                       # colour scale
                       colour_type = NULL,
                       colour_breaks = NULL,
                       colour_drop = NULL,
                       colour_guide = NULL,
                       colour_labels = NULL,
                       colour_limits = NULL,
                       colour_name = NULL,
                       colour_oob = NULL,
                       colour_rescaler = NULL,
                       colour_palette = NULL,
                       colour_transform = NULL,
                       # alpha scale
                       alpha_type = NULL,
                       alpha_breaks = ggplot2::waiver(),
                       alpha_drop = TRUE,
                       alpha_guide = NULL,
                       alpha_labels = NULL,
                       alpha_limits = NULL,
                       alpha_name = ggplot2::waiver(),
                       alpha_oob = scales::oob_censor,
                       alpha_palette = NULL,
                       alpha_transform = "identity",
                       # size scale
                       size_type = NULL,
                       size_breaks = ggplot2::waiver(),
                       size_drop = TRUE,
                       size_guide = NULL,
                       size_labels = NULL,
                       size_limits = NULL,
                       size_name = ggplot2::waiver(),
                       size_oob = scales::oob_censor,
                       size_palette = NULL,
                       size_transform = "identity",
                       # linewidth scale
                       linewidth_type = NULL,
                       linewidth_breaks = ggplot2::waiver(),
                       linewidth_drop = TRUE,
                       linewidth_guide = NULL,
                       linewidth_labels = NULL,
                       linewidth_limits = NULL,
                       linewidth_name = ggplot2::waiver(),
                       linewidth_oob = scales::oob_censor,
                       linewidth_palette = NULL,
                       linewidth_transform = "identity",
                       # linetype scale
                       linetype_type = NULL,
                       linetype_breaks = ggplot2::waiver(),
                       linetype_drop = TRUE,
                       linetype_guide = NULL,
                       linetype_labels = NULL,
                       linetype_limits = NULL,
                       linetype_name = ggplot2::waiver(),
                       linetype_palette = NULL,
                       # shape scale
                       shape_type = NULL,
                       shape_breaks = ggplot2::waiver(),
                       shape_drop = TRUE,
                       shape_guide = NULL,
                       shape_labels = NULL,
                       shape_limits = NULL,
                       shape_name = ggplot2::waiver(),
                       shape_palette = NULL,
                       #facet
                       facet_facets = NULL,
                       facet_rows = NULL,
                       facet_cols = NULL,
                       facet_axes = "margins",
                       facet_axis_labels = "all",
                       facet_drop = TRUE,
                       facet_labeller = "label_value",
                       facet_ncol = NULL,
                       facet_nrow = NULL,
                       facet_scales = "fixed",
                       facet_space = "fixed",
                       #coord
                       coord_xlim = NULL,
                       coord_ylim = NULL,
                       coord_clip = "on",
                       coord_reverse = "none",
                       coord_ratio = NULL,
                       #titles
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       #intialise
                       ggplot = ggplot2::ggplot

) {

  #get options
  current_theme <- ggplot2::get_theme() %||% theme_lighter()

  polish <- polish %||% get_polish() %||% polish_modern

  bordered_colour <- get_bordered_colour() %||% {
    \(x) if (is_panel_dark()) blend_screen(x) else blend_multiply(x)
  }

  ### get geom and stat names
  geom_info <- get_geom_info(geom)
  geom_fn <- geom_info$fn
  geom_str <- geom_info$str

  if (inherits(stat, "Stat")) {
    stat_str <- class(stat)[1] |> stringr::str_remove("^Stat") |> snakecase::to_snake_case()
  } else stat_str <- stat

  ### make aesthetics list
  aesthetics <- rlang::enquos(
    x = x,
    xmin = xmin,
    xmax = xmax,
    xend = xend,
    y = y,
    ymin = ymin,
    ymax = ymax,
    yend = yend,
    z = z,
    fill = fill,
    colour = colour,
    alpha = alpha,
    shape = shape,
    linetype = linetype,
    linewidth = linewidth,
    size = size,
    stroke = stroke,
    label = label,
    weight = weight,
    group = group,
    width = width,
    height = height,
    slope = slope,
    intercept = intercept,
    xintercept = xintercept,
    yintercept = yintercept,
    sample = sample,
    angle = angle,
    radius = radius,
    .ignore_empty = "all",
    .ignore_null = "all"
  )

  ### identify whether aesthetics mapped/fixed
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics, data)

  is_fill_mapped <- "fill" %in% names(separated$mapped) || "fill" %in% names(mapping)
  is_colour_mapped <- "colour" %in% names(separated$mapped) || "colour" %in% names(mapping)
  is_shape_mapped <- "shape" %in% names(separated$mapped) || "shape" %in% names(mapping)
  is_linewidth_mapped <- "linewidth" %in% names(separated$mapped) || "linewidth" %in% names(mapping)

  is_fill_fixed <- "fill" %in% names(separated$fixed)
  is_colour_fixed <- "colour" %in% names(separated$fixed)
  is_shape_fixed <- "shape" %in% names(separated$fixed)
  is_linewidth_fixed <- "linewidth" %in% names(separated$fixed)

  ### identify if bordered geom
  shape <- separated$fixed[["shape"]]
  if (rlang::is_null(shape)) shape <- current_theme$geom@pointshape

  # Determine if geom supports bordered styling (has both fill and colour)
  is_geom_bordered <- if (geom_str == "sf") {
    if (inherits(data, "sf")) {
      geom_type <- sf::st_geometry_type(data, by_geometry = FALSE)
      !any(geom_type %in% c("LINESTRING", "MULTILINESTRING", "CIRCULARSTRING", "COMPOUNDCURVE", "CURVE"))
    } else {
      FALSE
    }
  } else {
    geom_info$is_bordered
  }

  # Set defaults based on geom capability
  is_bordered_colour <- is_bordered_colour %||% is_geom_bordered
  is_bordered_linewidth <- is_bordered_linewidth %||% is_geom_bordered

  ### ensure colour is inherited from fill
  if (is_fill_mapped & !is_colour_mapped & !is_colour_fixed) {
    fill_aesthetic <- separated$mapped$fill %||% mapping$fill
    separated$mapped$colour <- fill_aesthetic
    is_colour_mapped <- TRUE
  }

  # Check if colour and fill are mapped to the same variable
  colour_fill_same <- FALSE
  if (is_colour_mapped && is_fill_mapped) {
    fill_var <- rlang::as_label(separated$mapped$fill %||% mapping$fill)
    colour_var <- rlang::as_label(separated$mapped$colour %||% mapping$colour)
    colour_fill_same <- identical(fill_var, colour_var)
  }

  ### compute fixed colour and linewidth based on if bordered geom
  computed_colour <- NULL
  if (!is_colour_mapped) {
    computed_colour <- separated$fixed[["colour"]] %||% separated$fixed[["fill"]] %||% current_theme$geom@fill
    if (is_bordered_colour) computed_colour <- bordered_colour(computed_colour)
  }

  computed_linewidth <- NULL
  if (!is_linewidth_mapped) {
    if (is_bordered_linewidth) computed_linewidth <- separated$fixed[["linewidth"]] %||% current_theme$geom@borderwidth
    else computed_linewidth <- separated$fixed[["linewidth"]] %||% current_theme$geom@linewidth
  }

  # Update separated$fixed for computed colour and linewidth
  separated$fixed <- separated$fixed[!names(separated$fixed) %in% c("colour", "linewidth")]

  # Combine fixed aesthetic values with additional parameters
  all_params <- utils::modifyList(separated$fixed, rlang::list2(...))

  # Add colour and linewidth back as params if they were computed
  if (!rlang::is_null(computed_colour)) {
    all_params$colour <- computed_colour
  }
  if (!rlang::is_null(computed_linewidth)) {
    all_params$linewidth <- computed_linewidth
  }

  # Combine individual aesthetics with mapping argument
  final_mapping <- combine_aesthetics(separated$mapped, mapping)

  ### aesthetics
  plot <- data |> ggplot2::ggplot(mapping = final_mapping)

  ### annotate
  if (!rlang::is_null(annotate)) {
    plot <- plot + annotate
  }

  ### layer
  if (!rlang::is_null(blend)) {
    plot <- plot +
      rlang::exec(
        geom_fn,
        stat = stat,
        position = position,
        !!!all_params
      ) |> blend()
  }
  else {
    plot <- plot +
      rlang::exec(
        geom_fn,
        stat = stat,
        position = position,
        !!!all_params
      )
  }

  ### coord
  if (!rlang::is_null(title)) {
    plot <- plot + ggplot2::labs(title = title)
  }
  if (!rlang::is_null(subtitle)) {
    plot <- plot + ggplot2::labs(subtitle = subtitle)
  }
  if (!rlang::is_null(caption)) {
    plot <- plot + ggplot2::labs(caption = caption)
  }

  coord <- get_coord(
    stat_str = stat_str,
    coord_xlim = coord_xlim,
    coord_ylim = coord_ylim,
    coord_clip = coord_clip,
    coord_reverse = coord_reverse,
    coord_ratio = coord_ratio
  )

  plot <- plot +
    coord

  #### facet
  if (!rlang::is_null(facet_facets)) {
    plot <- plot +
      ggplot2::facet_wrap(
        facets = facet_facets,
        nrow = facet_nrow,
        ncol = facet_ncol,
        scales = facet_scales,
        drop = facet_drop,
        axes = facet_axes,
        axis.labels = facet_axis_labels
      )
  } else if (!rlang::is_null(facet_rows) | !rlang::is_null(facet_cols)) {
    plot <- plot +
      ggplot2::facet_grid(
        rows = facet_rows,
        cols = facet_cols,
        scales = facet_scales,
        space = facet_space,
        drop = facet_drop,
        axes = facet_axes,
        axis.labels = facet_axis_labels
      )
  }

  if (!rlang::is_null(x_limits)) {
    if (is.function(x_limits)) {
      plot <- plot +
        scale_x_continuous(limits = x_limits)
    }
    else {
      plot <- plot +
        xlim(x_limits)
    }
  }

  if (!rlang::is_null(y_limits)) {
    if (is.function(y_limits)) {
      plot <- plot +
        scale_y_continuous(limits = y_limits)
    }
    else {
      plot <- plot +
        ylim(y_limits)
    }
  }

  ### identify scales and focus
  built <- ggplot2::ggplot_build(plot)
  nrows <- length(unique(built$layout$layout$ROW))
  ncols <- length(unique(built$layout$layout$COL))

  scale_info <- identify_scale(built)

  x_type <- x_type %||% scale_info$x$type
  y_type <- y_type %||% scale_info$y$type
  fill_type <- fill_type %||% scale_info$fill$type
  colour_type <- colour_type %||% fill_type %||% scale_info$colour$type
  alpha_type <- alpha_type %||% scale_info$alpha$type
  size_type <- size_type %||% scale_info$size$type
  linewidth_type <- linewidth_type %||% scale_info$linewidth$type
  linetype_type <- linetype_type %||% scale_info$linetype$type
  shape_type <- shape_type %||% scale_info$shape$type

  x_temporal <- x_temporal %||% scale_info$x$temporal
  y_temporal <- y_temporal %||% scale_info$y$temporal

  focus <- focus %||% get_focus(built = built, x_type = x_type, y_type = y_type)

  ### Add x scale
  if (x_type == "discrete") {
    plot <- plot +
      ggplot2::scale_x_discrete(
        breaks = x_breaks %||% ggplot2::waiver(),
        minor_breaks = x_minor_breaks %||% ggplot2::waiver(),
        drop = x_drop,
        expand = x_expand %||% ggplot2::waiver(),
        guide = x_guide,
        labels = x_labels %||% ggplot2::waiver(),
        limits = x_limits,
        name = x_name,
        palette = x_palette,
        position = x_position,
        sec.axis = x_sec_axis
      )
  }
  else if (x_type == "continuous") {
    plot <- plot +
      ggplot2::scale_x_continuous(
        breaks = x_breaks %||% if (!is.na(x_temporal)) {
          if (ncols == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
        } else {
          if (ncols == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
        },
        minor_breaks = x_minor_breaks %||% ggplot2::waiver(),
        expand = x_expand %||% get_expand(scale_info$x$limits),
        guide = x_guide,
        labels = x_labels %||% get_labels(stat_str, x_temporal),
        limits = x_limits,
        name = x_name,
        oob = x_oob,
        position = x_position,
        sec.axis = x_sec_axis,
        transform = x_transform %||% get_transform(x_temporal)
      )
  }
  else if (x_type == "binned") {
    plot <- plot +
      ggplot2::scale_x_binned(
        breaks = x_breaks %||% if (!is.na(x_temporal)) {
          if (ncols == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
        } else {
          if (ncols == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
        },
        expand = x_expand %||% get_expand(scale_info$x$limits),
        guide = x_guide,
        labels = x_labels %||% get_labels(stat_str, x_temporal),
        limits = x_limits,
        name = x_name,
        oob = x_oob,
        position = x_position,
        transform = x_transform %||% get_transform(x_temporal)
      )
  }

  # Add y scale
  if (y_type == "discrete") {
    plot <- plot +
      ggplot2::scale_y_discrete(
        breaks = y_breaks %||% ggplot2::waiver(),
        minor_breaks = y_minor_breaks %||% ggplot2::waiver(),
        drop = y_drop,
        expand = y_expand %||% ggplot2::waiver(),
        guide = y_guide,
        labels = y_labels %||% ggplot2::waiver(),
        limits = y_limits,
        name = y_name,
        palette = y_palette,
        position = y_position,
        sec.axis = y_sec_axis
      )
  }
  else if (y_type == "continuous") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        breaks = y_breaks %||% if (!is.na(y_temporal)) {
          if (nrows == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
        } else {
          if (nrows == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
        },
        minor_breaks = y_minor_breaks %||% ggplot2::waiver(),
        expand = y_expand %||% get_expand(scale_info$y$limits),
        guide = y_guide,
        labels = y_labels %||% get_labels(stat_str, y_temporal),
        limits = y_limits,
        name = y_name,
        oob = y_oob,
        position = y_position,
        sec.axis = y_sec_axis,
        transform = y_transform %||% get_transform(y_temporal)
      )
  }
  else if (y_type == "binned") {
    plot <- plot +
      ggplot2::scale_y_binned(
        breaks = y_breaks %||% if (!is.na(y_temporal)) {
          if (nrows == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
        } else {
          if (nrows == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
        },
        expand = y_expand %||% get_expand(scale_info$y$limits),
        guide = y_guide,
        labels = y_labels %||% get_labels(stat_str, y_temporal),
        limits = y_limits,
        name = y_name,
        oob = y_oob,
        position = y_position,
        transform = y_transform %||% get_transform(y_temporal)
      )
  }
  fill_na <- jumble::grey
  fill_override <- jumble::slate

  if (is_bordered_colour) {
    colour_na <- bordered_colour(fill_na)
    colour_override <- bordered_colour(fill_override)
  }
  else {
    colour_na <- fill_na
    colour_override <- fill_override
  }

  # Add fill scale
  if (!rlang::is_null(fill_type)) {
    if (fill_type == "discrete") {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "fill",
          palette = as_discrete_palette(fill_palette),
          breaks = fill_breaks %||% ggplot2::waiver(),
          drop = fill_drop,
          guide = fill_guide %||% ggplot2::guide_legend(),
          labels = fill_labels %||% ggplot2::waiver(),
          limits = fill_limits,
          name = fill_name,
          na.value = fill_na
        )
    }
    else if (fill_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "fill",
          palette = as_continuous_palette(fill_palette),
          breaks = fill_breaks %||% ggplot2::waiver(),
          guide = fill_guide %||% ggplot2::guide_legend(),
          labels = fill_labels %||% scales::label_number(),
          limits = fill_limits,
          name = fill_name,
          oob = fill_oob,
          rescaler = fill_rescaler,
          transform = fill_transform,
          na.value = fill_na
        )
    }
    else if (fill_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "fill",
          palette = as_continuous_palette(fill_palette),
          breaks = fill_breaks %||% ggplot2::waiver(),
          guide = fill_guide %||% ggplot2::guide_bins(),
          labels = fill_labels %||% scales::label_number(),
          limits = fill_limits,
          name = fill_name,
          oob = fill_oob,
          rescaler = fill_rescaler,
          transform = fill_transform,
          na.value = fill_na
        )
    }

    plot <- plot +
      ggplot2::theme(geom = ggplot2::element_geom(fill = fill_override))
  }

  # Add colour scale
  if (!rlang::is_null(colour_type)) {
    if (colour_type == "discrete") {
      if (is_bordered_colour) {
        colour_palette <- colour_palette %||%
          bordered_colour(fill_palette) %||%
          bordered_colour(current_theme$palette.fill.discrete)
      }
      else {
        colour_palette <- colour_palette %||%
          fill_palette %||%
          current_theme$palette.fill.discrete
      }

      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "colour",
          palette = as_discrete_palette(colour_palette),
          breaks = colour_breaks %||% fill_breaks,
          drop = colour_drop  %||% fill_drop,
          guide = colour_guide  %||%  fill_guide %||% ggplot2::guide_legend(),
          labels = colour_labels %||% fill_labels %||% ggplot2::waiver(),
          limits = colour_limits %||% fill_limits,
          name = colour_name %||% fill_name,
          na.value = colour_na
        )
    }
    else if (colour_type %in% c("continuous", "binned")) {
      if (is_bordered_colour) {
        colour_palette <- colour_palette %||%
          bordered_colour(fill_palette) %||%
          bordered_colour(current_theme$palette.fill.continuous)
      }
      else {
        colour_palette <- colour_palette %||%
          fill_palette %||%
          current_theme$palette.fill.continuous
      }

      if (colour_type == "continuous") {
        plot <- plot +
          ggplot2::continuous_scale(
            aesthetics = "colour",
            palette = as_continuous_palette(colour_palette),
            breaks = colour_breaks %||% fill_breaks,
            guide = colour_guide %||% fill_guide %||% ggplot2::guide_legend(),
            labels = colour_labels %||% fill_labels %||% scales::label_number(),
            limits = colour_limits %||% fill_limits,
            name = colour_name %||% fill_name,
            oob = colour_oob %||% fill_oob,
            rescaler = colour_rescaler %||% fill_rescaler,
            transform = colour_transform %||% fill_transform,
            na.value = colour_na
          )
      }
      else if (colour_type == "binned") {
        plot <- plot +
          ggplot2::binned_scale(
            aesthetics = "colour",
            palette = as_continuous_palette(colour_palette),
            breaks = colour_breaks %||% fill_breaks,
            guide = colour_guide %||% fill_guide %||% ggplot2::guide_bins(),
            labels = colour_labels %||% fill_labels %||% scales::label_number(),
            limits = colour_limits %||% fill_limits,
            name = colour_name %||% fill_name,
            oob = colour_oob %||% fill_oob,
            rescaler = colour_rescaler %||% fill_rescaler,
            transform = colour_transform %||% fill_transform,
            na.value = colour_na
          )
      }
    }

    plot <- plot +
      ggplot2::theme(geom = ggplot2::element_geom(colour = colour_override))
  }

  # Add alpha scale
  if (!rlang::is_null(alpha_type)) {
    if (alpha_type == "discrete") {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "alpha",
          palette = as_discrete_palette(alpha_palette),
          breaks = alpha_breaks,
          drop = alpha_drop,
          guide = alpha_guide %||% ggplot2::guide_legend(),
          labels = alpha_labels %||% ggplot2::waiver(),
          limits = alpha_limits,
          name = alpha_name
        )
    }
    else if (alpha_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "alpha",
          palette = as_continuous_palette(alpha_palette),
          breaks = alpha_breaks,
          guide = alpha_guide %||% ggplot2::guide_legend(),
          labels = alpha_labels %||% scales::label_number(),
          limits = alpha_limits,
          name = alpha_name,
          oob = alpha_oob,
          transform = alpha_transform
        )
    }
    else if (alpha_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "alpha",
          palette = as_continuous_palette(alpha_palette),
          breaks = alpha_breaks,
          guide = alpha_guide %||% ggplot2::guide_bins(),
          labels = alpha_labels %||% scales::label_number(),
          limits = alpha_limits,
          name = alpha_name,
          oob = alpha_oob,
          transform = alpha_transform
        )
    }
  }

  # Add size scale
  if (!rlang::is_null(size_type)) {
    if (size_type == "discrete") {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "size",
          palette = as_discrete_palette(size_palette),
          breaks = size_breaks,
          drop = size_drop,
          guide = size_guide %||% ggplot2::guide_legend(),
          labels = size_labels %||% ggplot2::waiver(),
          limits = size_limits,
          name = size_name
        )
    }
    else if (size_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "size",
          palette = as_continuous_palette(size_palette),
          breaks = size_breaks,
          guide = size_guide %||% ggplot2::guide_legend(),
          labels = size_labels %||% scales::label_number(),
          limits = size_limits,
          name = size_name,
          oob = size_oob,
          transform = size_transform
        )
    }
    else if (size_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "size",
          palette = as_continuous_palette(size_palette),
          breaks = size_breaks,
          guide = size_guide %||% ggplot2::guide_bins(),
          labels = size_labels %||% scales::label_number(),
          limits = size_limits,
          name = size_name,
          oob = size_oob,
          transform = size_transform
        )
    }
  }

  # Add linewidth scale
  if (!rlang::is_null(linewidth_type)) {
    if (linewidth_type == "discrete") {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "linewidth",
          palette = as_discrete_palette(linewidth_palette),
          breaks = linewidth_breaks,
          drop = linewidth_drop,
          guide = linewidth_guide %||% ggplot2::guide_legend(),
          labels = linewidth_labels %||% ggplot2::waiver(),
          limits = linewidth_limits,
          name = linewidth_name
        )
    }
    else if (linewidth_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "linewidth",
          palette = as_continuous_palette(linewidth_palette),
          breaks = linewidth_breaks,
          guide = linewidth_guide %||% ggplot2::guide_legend(),
          labels = linewidth_labels %||% scales::label_number(),
          limits = linewidth_limits,
          name = linewidth_name,
          oob = linewidth_oob,
          transform = linewidth_transform
        )
    }
    else if (linewidth_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "linewidth",
          palette = as_continuous_palette(linewidth_palette),
          breaks = linewidth_breaks,
          guide = linewidth_guide %||% ggplot2::guide_bins(),
          labels = linewidth_labels %||% scales::label_number(),
          limits = linewidth_limits,
          name = linewidth_name,
          oob = linewidth_oob,
          transform = linewidth_transform
        )
    }
  }

  # Add linetype scale  (discrete only)
  if (!rlang::is_null(linetype_type)) {
    if (linetype_type == "discrete") {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "linetype",
          palette = as_discrete_palette(linetype_palette),
          breaks = linetype_breaks,
          drop = linetype_drop,
          guide = linetype_guide %||% ggplot2::guide_legend(),
          labels = linetype_labels %||% ggplot2::waiver(),
          limits = linetype_limits,
          name = linetype_name
        )
    }
  }

  # Add shape scale  (discrete only)
  if (!rlang::is_null(shape_type)) {
    if (shape_type == "discrete") {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "shape",
          palette = as_discrete_palette(shape_palette),
          breaks = shape_breaks,
          drop = shape_drop,
          guide = shape_guide %||% ggplot2::guide_legend(),
          labels = shape_labels %||% ggplot2::waiver(),
          limits = shape_limits,
          name = shape_name
        )
    }
  }

  ### theme
  plot <- plot +
    polish(focus = focus, x_type = x_type, y_type = y_type, geom = geom_str)

  return(plot)
}

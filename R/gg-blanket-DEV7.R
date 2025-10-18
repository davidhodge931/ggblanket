# Fixed gg_blanket function with improved mapping handling
gg_blanket <- function(data,
                       ...,
                       geom = "blank",
                       stat = "identity",
                       position = ggplot2::position_identity(),
                       annotate = NULL,
                       blend = NULL,
                       # ggblanket-specific
                       border = NULL,
                       bordercolour_transform = \(x) if (is_panel_dark()) blend_screen(x) else blend_multiply(x),
                       aspect = NULL,
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
                       x_drop = FALSE,
                       x_expand = NULL,
                       x_guide = ggplot2::waiver(),
                       x_labels = NULL,
                       x_limits = NULL,
                       x_minor_breaks = ggplot2::waiver(),
                       x_oob = scales::oob_keep,
                       x_palette = seq_len,
                       x_position = "bottom",
                       x_sec_axis = ggplot2::waiver(),
                       x_title = ggplot2::waiver(),
                       x_transform = NULL,
                       # y scale
                       y_type = NULL,
                       y_temporal = NULL,
                       y_breaks = NULL,
                       y_drop = FALSE,
                       y_expand = NULL,
                       y_guide = ggplot2::waiver(),
                       y_labels = NULL,
                       y_limits = NULL,
                       y_minor_breaks = ggplot2::waiver(),
                       y_oob = scales::oob_keep,
                       y_palette = seq_len,
                       y_position = "left",
                       y_sec_axis = ggplot2::waiver(),
                       y_title = ggplot2::waiver(),
                       y_transform = NULL,
                       # fill scale
                       fill_type = NULL,
                       fill_temporal = NULL,
                       fill_breaks = ggplot2::waiver(),
                       fill_drop = FALSE,
                       fill_guide = NULL,
                       fill_labels = NULL,
                       fill_limits = NULL,
                       fill_oob = scales::oob_keep,
                       fill_rescaler = scales::rescale,
                       fill_palette = NULL,
                       fill_title = ggplot2::waiver(),
                       fill_transform = "identity",
                       # colour scaleS
                       colour_type = NULL,
                       colour_temporal = NULL,
                       colour_breaks = NULL,
                       colour_drop = NULL,
                       colour_guide = NULL,
                       colour_labels = NULL,
                       colour_limits = NULL,
                       colour_oob = NULL,
                       colour_rescaler = NULL,
                       colour_palette = NULL,
                       colour_title = ggplot2::waiver(),
                       colour_transform = NULL,
                       #facet
                       facet_facets = NULL,
                       facet_rows = NULL,
                       facet_cols = NULL,
                       facet_axes = "margins",
                       facet_axis_labels = "all",
                       facet_drop = FALSE,
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
                       coord_ratio = NULL

) {

  # Capture all aesthetics using enquos for lazy evaluation
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

  # Get a list of fixed aesthetics and mapped aesthetic
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics, data)

  # Check for colour/fill in BOTH individual aesthetics AND mapping
  is_fill_mapped <- "fill" %in% names(separated$mapped) || "fill" %in% names(mapping)
  is_fill_fixed <- "fill" %in% names(separated$fixed)
  is_colour_mapped <- "colour" %in% names(separated$mapped) || "colour" %in% names(mapping)
  is_colour_fixed <- "colour" %in% names(separated$fixed)
  is_shape_mapped <- "shape" %in% names(separated$mapped) || "shape" %in% names(mapping)
  is_shape_fixed <- "shape" %in% names(separated$fixed)
  is_linewidth_mapped <- "linewidth" %in% names(separated$mapped) || "linewidth" %in% names(mapping)
  is_linewidth_fixed <- "linewidth" %in% names(separated$fixed)

  # Make colour mapped to fill, if fill is mapped and colour not specified
  if (is_fill_mapped & !is_colour_mapped & !is_colour_fixed) {
    # Get fill from either separated$mapped or mapping
    fill_aesthetic <- separated$mapped$fill %||% mapping$fill
    separated$mapped$colour <- fill_aesthetic
    is_colour_mapped <- TRUE
  }

  # Get geom/stat strings
  if (inherits(geom, "Geom")) {
    geom_str <- class(geom)[1] |> stringr::str_remove("^Geom") |> snakecase::to_snake_case()
  } else geom_str <- geom

  if (inherits(stat, "Stat")) {
    stat_str <- class(stat)[1] |> stringr::str_remove("^Stat") |> snakecase::to_snake_case()
  } else stat_str <- stat

  # Get border
  shape <- separated$fixed[["shape"]]
  if (rlang::is_null(shape)) shape <- ggplot2::get_theme()$geom@pointshape

  if (rlang::is_null(border)) {
    border <- is_geom_border(geom_str = geom_str, shape = shape)
  }

  # get colour param
  computed_colour <- NULL
  if (!is_colour_mapped) {
    computed_colour <- separated$fixed[["colour"]] %||% separated$fixed[["fill"]] %||% ggplot2::get_theme()$geom@fill
    if (border) computed_colour <- bordercolour_transform(computed_colour)
  }

  # get linewidth param
  computed_linewidth <- NULL
  if (!is_linewidth_mapped) {
    if (border) computed_linewidth <- separated$fixed[["linewidth"]] %||% ggplot2::get_theme()$geom@borderwidth
    else computed_linewidth <- separated$fixed[["linewidth"]] %||% ggplot2::get_theme()$geom@linewidth
  }

  # Remove colour and linewidth from separated$fixed
  separated$fixed <- separated$fixed[!names(separated$fixed) %in% c("colour", "linewidth")]

  # Combine individual aesthetics with mapping argument
  final_mapping <- combine_aesthetics(separated$mapped, mapping)

  # Capture additional parameters from ...
  additional_params <- rlang::list2(...)

  # Combine fixed aesthetic values with additional parameters
  all_params <- utils::modifyList(separated$fixed, additional_params)

  # Add colour and linewidth back as params if they were computed
  if (!rlang::is_null(computed_colour)) {
    all_params$colour <- computed_colour
  }
  if (!rlang::is_null(computed_linewidth)) {
    all_params$linewidth <- computed_linewidth
  }

  # Build initial plot
  plot <- ggplot2::ggplot(data, mapping = final_mapping)

  # Add any annotate layers under
  if (!rlang::is_null(annotate)) {
    plot <- plot + annotate
  }

  # Add the layer
  if (rlang::is_null(blend)) {
    if (is_stat_sf(stat_str)) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom,
          stat = stat,
          position = position,
          params = all_params
        )
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = all_params
        )
    }
  }
  else {
    if (is_stat_sf(stat_str)) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom,
          stat = stat,
          position = position,
          params = all_params
        ) |> ggblend::blend(blend = blend)
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = all_params
        ) |> ggblend::blend(blend = blend)
    }
  }

  # Build and identify scales
  built <- ggplot2::ggplot_build(plot)
  scale_info <- identify(built)

  x_type <- x_type %||% scale_info$x$type
  y_type <- y_type %||% scale_info$y$type
  x_temporal <- x_temporal %||% scale_info$x$temporal
  y_temporal <- y_temporal %||% scale_info$y$temporal
  fill_type <- fill_type %||% scale_info$fill$type
  fill_temporal <- fill_temporal %||% scale_info$fill$temporal
  colour_type <- colour_type %||% scale_info$colour$type %||% scale_info$fill$type
  colour_temporal <- colour_temporal %||% scale_info$colour$temporal %||% scale_info$fill$temporal

  aspect <- aspect %||% get_aspect(built = built, x_type = x_type, y_type = y_type)

  coord <- get_coord(
    stat_str = stat_str,
    coord_xlim = coord_xlim,
    coord_ylim = coord_ylim,
    coord_clip = coord_clip,
    coord_reverse = coord_reverse,
    coord_ratio = coord_ratio
  )

  # Add x scale based on type
  if (x_type == "discrete") {
    plot <- plot +
      ggplot2::scale_x_discrete(
        breaks = x_breaks %||% ggplot2::waiver(),
        minor_breaks = x_minor_breaks %||% ggplot2::waiver(),
        drop = x_drop,
        expand = x_expand %||% ggplot2::waiver(),
        guide = x_guide,
        labels = x_labels %||% ggplot2::waiver(),
        limits = get_limits(x_limits),
        continuous.limits = get_limits_continuous(x_limits),
        palette = x_palette,
        position = x_position,
        sec.axis = x_sec_axis
      )
  }
  else if (x_type == "continuous") {
    plot <- plot +
      ggplot2::scale_x_continuous(
        breaks = x_breaks %||% ggplot2::waiver(),
        minor_breaks = x_minor_breaks %||% ggplot2::waiver(),
        expand = x_expand %||% get_expand(scale_info$x$limits),
        guide = x_guide,
        labels = x_labels %||% get_labels(stat_str, x_temporal),
        limits = x_limits,
        oob = x_oob,
        position = x_position,
        sec.axis = x_sec_axis,
        transform = x_transform %||% get_transform(x_temporal)
      )
  }
  else if (x_type == "binned") {
    plot <- plot +
      ggplot2::scale_x_binned(
        breaks = x_breaks %||% ggplot2::waiver(),
        expand = x_expand %||% get_expand(scale_info$x$limits),
        guide = x_guide,
        labels = x_labels %||% get_labels(stat_str, x_temporal),
        limits = x_limits,
        oob = x_oob,
        position = x_position,
        transform = x_transform %||% get_transform(x_temporal)
      )
  }

  # Add y scale based on type
  if (y_type == "discrete") {
    plot <- plot +
      ggplot2::scale_y_discrete(
        breaks = y_breaks %||% ggplot2::waiver(),
        minor_breaks = y_minor_breaks %||% ggplot2::waiver(),
        drop = y_drop,
        expand = y_expand %||% ggplot2::waiver(),
        guide = y_guide,
        labels = y_labels %||% ggplot2::waiver(),
        limits = get_limits(y_limits),
        continuous.limits = get_limits_continuous(y_limits),
        palette = y_palette,
        position = y_position,
        sec.axis = y_sec_axis
      )
  }
  else if (y_type == "continuous") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        breaks = y_breaks %||% ggplot2::waiver(),
        minor_breaks = y_minor_breaks %||% ggplot2::waiver(),
        expand = y_expand %||% get_expand(scale_info$y$limits),
        guide = y_guide,
        labels = y_labels %||% get_labels(stat_str, y_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        sec.axis = y_sec_axis,
        transform = y_transform %||% get_transform(y_temporal)
      )
  }
  else if (y_type == "binned") {
    plot <- plot +
      ggplot2::scale_y_binned(
        breaks = y_breaks %||% ggplot2::waiver(),
        expand = y_expand %||% get_expand(scale_info$y$limits),
        guide = y_guide,
        labels = y_labels %||% get_labels(stat_str, y_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        transform = y_transform %||% get_transform(y_temporal)
      )
  }

  fill_na <- oat
  if (border) colour_na <- bordercolour_transform(fill_na)
  else colour_na <- fill_na

  fill_override <- slate
  if (border) colour_override <- bordercolour_transform(fill_override)
  else colour_override <- fill_override

    # Add fill scale based on type
  if (!rlang::is_null(fill_type)) {
      if (fill_type == "discrete") {
        plot <- plot +
          ggplot2::scale_fill_discrete(
            palette = fill_palette %||% ggplot2::get_theme()$palette.fill.discrete,
            breaks = fill_breaks %||% ggplot2::waiver(),
            drop = fill_drop,
            guide = fill_guide %||% ggplot2::guide_legend(),
            labels = fill_labels %||% ggplot2::waiver(),
            limits = get_limits(fill_limits),
            na.value = fill_na
          )
      }
      else if (fill_type == "continuous") {
        plot <- plot +
          ggplot2::scale_fill_continuous(
            palette = fill_palette %||% ggplot2::get_theme()$palette.fill.continuous,
            breaks = fill_breaks %||% ggplot2::waiver(),
            guide = fill_guide %||% ggplot2::guide_colourbar(),
            labels = fill_labels %||% scales::label_comma(),
            limits = fill_limits,
            rescaler = fill_rescaler,
            transform = fill_transform %||% get_transform(fill_temporal %||% NA_character_),
            na.value = fill_na
          )
      } else if (fill_type == "binned") {
        plot <- plot +
          ggplot2::scale_fill_binned(
            palette = fill_palette %||% ggplot2::get_theme()$palette.fill.continuous,
            breaks = fill_breaks %||% ggplot2::waiver(),
            guide = fill_guide %||% ggplot2::guide_bins(),
            labels = fill_labels %||% scales::label_comma(),
            limits = fill_limits,
            rescaler = fill_rescaler,
            transform = fill_transform %||% get_transform(fill_temporal %||% NA_character_),
            na.value = fill_na
          )
      }
    plot <- plot +
      ggplot2::theme(geom = ggplot2::element_geom(fill = fill_override))
    }

  # Add colour scale based on type - with fill fallbacks
  if (!rlang::is_null(colour_type)) {
    if (colour_type == "discrete") {
      if (border) {
        colour_palette <- colour_palette %||% bordercolour_transform(fill_palette) %||%
          bordercolour_transform(ggplot2::get_theme()$palette.fill.discrete)
      }
      else {
        colour_palette <- colour_palette %||% fill_palette %||%
          ggplot2::get_theme()$palette.fill.discrete
      }

      plot <- plot +
        ggplot2::scale_colour_discrete(
          palette = colour_palette ,
          breaks = colour_breaks %||% fill_breaks,
          drop = colour_drop  %||% fill_drop,
          guide = colour_guide  %||%  fill_guide %||% ggplot2::guide_legend(),
          labels = colour_labels %||% fill_labels %||% ggplot2::waiver(),
          limits = colour_limits %||% fill_limits,
          na.value = colour_na
        )
    }
    else if (colour_type == "continuous") {
      if (border) {
        colour_palette <- colour_palette %||% bordercolour_transform(fill_palette) %||%
          bordercolour_transform(ggplot2::get_theme()$palette.fill.continuous)
      }
      else {
        colour_palette <- colour_palette %||% fill_palette %||%
          ggplot2::get_theme()$palette.fill.continuous
      }

      plot <- plot +
        ggplot2::scale_colour_continuous(
          palette = colour_palette ,
          breaks = colour_breaks %||% fill_breaks,
          guide = colour_guide %||% if (border) ggplot2::guide_none() else fill_guide %||% ggplot2::guide_colorsteps(),
          labels = colour_labels %||% fill_labels %||% scales::label_comma(),
          limits = colour_limits %||% fill_limits,
          rescaler = colour_rescaler %||% fill_rescaler,
          transform = colour_transform %||% fill_transform,
          na.value = colour_na
        )
    } else if (colour_type == "binned") {
      if (border) {
        colour_palette <- colour_palette %||% bordercolour_transform(fill_palette) %||%
          bordercolour_transform(ggplot2::get_theme()$palette.fill.continuous)
      }
      else {
        colour_palette <- colour_palette %||% fill_palette %||%
          ggplot2::get_theme()$palette.fill.continuous
      }

      plot <- plot +
        ggplot2::scale_colour_binned(
          palette = colour_palette ,
          breaks = colour_breaks %||% fill_breaks,
          guide = colour_guide %||% if (border) ggplot2::guide_none() else fill_guide %||% ggplot2::guide_bins(),
          labels = colour_labels %||% fill_labels %||% scales::label_comma(),
          limits = colour_limits %||% fill_limits,
          rescaler = colour_rescaler %||% fill_rescaler,
          transform = colour_transform %||% fill_transform,
          na.value = colour_na
        )
    }

    plot <- plot +
      ggplot2::theme(geom = ggplot2::element_geom(colour = colour_override))
  }

  if (ggplot2::is_waiver(x_title)) x_title <- snakecase::to_sentence_case
  if (ggplot2::is_waiver(y_title)) y_title <- snakecase::to_sentence_case
  if (ggplot2::is_waiver(fill_title)) fill_title <- snakecase::to_sentence_case
  if (ggplot2::is_waiver(colour_title)) colour_title <- fill_title

  plot <- plot +
    coord +
    theme_to_aspect(aspect = aspect) +
    ggplot2::labs(
      x = x_title,
      y = y_title,
      fill = fill_title,
      colour = colour_title,
    )

  # Add faceting if specified
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

  return(plot)
}

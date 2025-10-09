# Fixed gg_blanket function with separate colour and fill
gg_blanket <- function(data,
                       ...,
                       geom = "blank",
                       stat = "identity",
                       position = ggplot2::position_identity(),
                       coord = NULL,
                       aspect = NULL,
                       blend = NULL,
                       annotate = NULL,
                       # aesthetics
                       x = NULL,
                       xmin = NULL,
                       xmax = NULL,
                       xend = NULL,
                       y = NULL,
                       ymin = NULL,
                       ymax = NULL,
                       yend = NULL,
                       z = NULL,
                       colour = NULL,
                       fill = NULL,
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
                       xintercept = NULL,
                       yintercept = NULL,
                       sample = NULL,
                       angle = NULL,
                       radius = NULL,
                       mapping = NULL,
                       # x scale arguments
                       x_scale_type = NULL,
                       x_scale_temporal = NULL,
                       x_breaks = NULL,
                       x_breaks_minor = ggplot2::waiver(),
                       x_breaks_n = 6,
                       x_drop = FALSE,
                       x_expand = NULL,
                       x_guide = ggplot2::waiver(),
                       x_labels = NULL,
                       x_limits = NULL,
                       x_oob = scales::oob_keep,
                       x_palette = seq_len,
                       x_position = "bottom",
                       x_sec_axis = ggplot2::waiver(),
                       x_title = snakecase::to_sentence_case,
                       x_transform = NULL,
                       # y scale arguments
                       y_scale_type = NULL,
                       y_scale_temporal = NULL,
                       y_breaks = NULL,
                       y_breaks_minor = ggplot2::waiver(),
                       y_breaks_n = 6,
                       y_drop = FALSE,
                       y_expand = NULL,
                       y_guide = ggplot2::waiver(),
                       y_labels = NULL,
                       y_limits = NULL,
                       y_oob = scales::oob_keep,
                       y_palette = seq_len,
                       y_position = "left",
                       y_sec_axis = ggplot2::waiver(),
                       y_title = snakecase::to_sentence_case,
                       y_transform = NULL,
                       # colour scale arguments
                       colour_scale_type = NULL,
                       colour_scale_temporal = NULL,
                       colour_breaks = NULL,
                       colour_breaks_n = NULL,
                       colour_drop = NULL,
                       colour_expand = NULL,
                       colour_guide = NULL,
                       colour_labels = NULL,
                       colour_limits = NULL,
                       colour_oob = NULL,
                       colour_rescaler = NULL,
                       colour_palette = NULL,
                       colour_title = NULL,
                       colour_transform = NULL,
                       # fill scale arguments
                       fill_scale_type = NULL,
                       fill_scale_temporal = NULL,
                       fill_breaks = NULL,
                       fill_breaks_n = NULL,
                       fill_drop = NULL,
                       fill_expand = NULL,
                       fill_guide = NULL,
                       fill_labels = NULL,
                       fill_limits = NULL,
                       fill_oob = NULL,
                       fill_rescaler = NULL,
                       fill_palette = NULL,
                       fill_title = NULL,
                       fill_transform = NULL
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
    colour = colour,
    fill = fill,
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
    .ignore_empty = "all"
  )

  # Remove NULL aesthetics (missing arguments)
  aesthetics <- Filter(Negate(rlang::quo_is_missing), aesthetics)

  # Separate fixed values from aesthetic mappings BEFORE inheritance
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics)

  # Store flags for whether colour/fill were originally provided
  has_colour_mapped <- "colour" %in% names(separated$mapped)
  has_colour_fixed <- "colour" %in% names(separated$fixed)
  has_fill_mapped <- "fill" %in% names(separated$mapped)
  has_fill_fixed <- "fill" %in% names(separated$fixed)

  # Handle colour inheriting from fill
  # If colour is not provided at all, inherit from fill
  if (!has_colour_mapped && !has_colour_fixed) {
    if (has_fill_mapped) {
      separated$mapped[["colour"]] <- separated$mapped[["fill"]]
    } else if (has_fill_fixed) {
      separated$fixed[["colour"]] <- separated$fixed[["fill"]]
    }
  }

  # Convert geom, stat, position to appropriate objects
  geom_obj <- resolve_geom(geom)
  stat_obj <- resolve_stat(stat)
  position_obj <- resolve_position(position)

  # Combine individual aesthetics with mapping argument
  final_mapping <- combine_aesthetics(separated$mapped, mapping)

  # Capture additional parameters from ...
  additional_params <- rlang::list2(...)

  # Combine fixed aesthetic values with additional parameters
  all_params <- utils::modifyList(separated$fixed, additional_params)

  # Build initial plot
  plot <- ggplot2::ggplot(data, mapping = final_mapping)

  # Add any annotate layers under
  if (!rlang::is_null(annotate)) {
    plot <- plot + annotate
  }

  # Add the layer
  if (is.null(blend)) {
    if (is_stat_sf(stat)) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom_obj,
          stat = stat_obj,
          position = position_obj,
          params = all_params
        )
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom_obj,
          stat = stat_obj,
          position = position_obj,
          params = all_params
        )
    }
  }
  else {
    if (is_stat_sf(stat)) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom_obj,
          stat = stat_obj,
          position = position_obj,
          params = all_params
        ) |> ggblend::blend(blend = blend)
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom_obj,
          stat = stat_obj,
          position = position_obj,
          params = all_params
        ) |> ggblend::blend(blend = blend)
    }
  }

  # Build and identify scales
  built <- ggplot2::ggplot_build(plot)
  scale_info <- identify_scale(built)

  x_scale_type <- x_scale_type %||% scale_info$x$type
  y_scale_type <- y_scale_type %||% scale_info$y$type
  x_scale_temporal <- x_scale_temporal %||% scale_info$x$temporal
  y_scale_temporal <- y_scale_temporal %||% scale_info$y$temporal
  colour_scale_type <- colour_scale_type %||% scale_info$colour$type
  colour_scale_temporal <- colour_scale_temporal %||% scale_info$colour$temporal
  fill_scale_type <- fill_scale_type %||% scale_info$fill$type
  fill_scale_temporal <- fill_scale_temporal %||% scale_info$fill$temporal

  # If colour was inherited from fill (not explicitly provided), inherit all scale arguments
  if (!is.null(scale_info$colour) && !is.null(scale_info$fill)) {
    if (!has_colour_mapped && !has_colour_fixed && (has_fill_mapped || has_fill_fixed)) {
      # Colour was inherited from fill, so inherit all scale arguments
      colour_scale_type <- colour_scale_type %||% fill_scale_type
      colour_scale_temporal <- colour_scale_temporal %||% fill_scale_temporal
      colour_breaks <- colour_breaks %||% fill_breaks
      colour_breaks_n <- colour_breaks_n %||% fill_breaks_n
      colour_drop <- colour_drop %||% fill_drop
      colour_expand <- colour_expand %||% fill_expand
      colour_guide <- colour_guide %||% fill_guide
      colour_labels <- colour_labels %||% fill_labels
      colour_limits <- colour_limits %||% fill_limits
      colour_oob <- colour_oob %||% fill_oob
      colour_rescaler <- colour_rescaler %||% fill_rescaler
      colour_palette <- colour_palette %||% fill_palette
      colour_title <- colour_title %||% fill_title
      colour_transform <- colour_transform %||% fill_transform
    }
  }

  # Apply defaults for any remaining NULL values
  colour_breaks_n <- colour_breaks_n %||% 6
  colour_drop <- colour_drop %||% FALSE
  colour_oob <- colour_oob %||% scales::oob_keep
  colour_rescaler <- colour_rescaler %||% scales::rescale
  colour_title <- colour_title %||% snakecase::to_sentence_case

  fill_breaks_n <- fill_breaks_n %||% 6
  fill_drop <- fill_drop %||% FALSE
  fill_oob <- fill_oob %||% scales::oob_keep
  fill_rescaler <- fill_rescaler %||% scales::rescale
  fill_title <- fill_title %||% snakecase::to_sentence_case

  aspect <- aspect %||% get_aspect(built)
  coord <- coord %||% get_coord(stat, aspect)

  # Add x scale based on type
  if (x_scale_type == "discrete") {
    plot <- plot +
      ggplot2::scale_x_discrete(
        breaks = x_breaks %||% ggplot2::waiver(),
        minor_breaks = x_breaks_minor %||% ggplot2::waiver(),
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
  } else if (x_scale_type == "continuous") {
    plot <- plot +
      ggplot2::scale_x_continuous(
        breaks = x_breaks %||% ggplot2::waiver(),
        minor_breaks = x_breaks_minor %||% ggplot2::waiver(),
        n.breaks = x_breaks_n,
        expand = x_expand %||% get_expand(scale_info$x$limits),
        guide = x_guide,
        labels = x_labels %||% get_labels(stat, x_scale_temporal),
        limits = x_limits,
        oob = x_oob,
        position = x_position,
        sec.axis = x_sec_axis,
        transform = x_transform %||% get_transform(x_scale_temporal)
      )
  } else if (x_scale_type == "binned") {
    plot <- plot +
      ggplot2::scale_x_binned(
        breaks = x_breaks %||% ggplot2::waiver(),
        n.breaks = x_breaks_n,
        expand = x_expand %||% get_expand(scale_info$x$limits),
        guide = x_guide,
        labels = x_labels %||% get_labels(stat, x_scale_temporal),
        limits = x_limits,
        oob = x_oob,
        position = x_position,
        transform = x_transform %||% get_transform(x_scale_temporal)
      )
  }

  # Add y scale based on type
  if (y_scale_type == "discrete") {
    plot <- plot +
      ggplot2::scale_y_discrete(
        breaks = y_breaks %||% ggplot2::waiver(),
        minor_breaks = y_breaks_minor %||% ggplot2::waiver(),
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
  } else if (y_scale_type == "continuous") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        breaks = y_breaks %||% ggplot2::waiver(),
        minor_breaks = y_breaks_minor %||% ggplot2::waiver(),
        n.breaks = y_breaks_n,
        expand = y_expand %||% get_expand(scale_info$y$limits),
        guide = y_guide,
        labels = y_labels %||% get_labels(stat, y_scale_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        sec.axis = y_sec_axis,
        transform = y_transform %||% get_transform(y_scale_temporal)
      )
  } else if (y_scale_type == "binned") {
    plot <- plot +
      ggplot2::scale_y_binned(
        breaks = y_breaks %||% ggplot2::waiver(),
        n.breaks = y_breaks_n,
        expand = y_expand %||% get_expand(scale_info$y$limits),
        guide = y_guide,
        labels = y_labels %||% get_labels(stat, y_scale_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        transform = y_transform %||% get_transform(y_scale_temporal)
      )
  }

  # Add colour scale based on type
  if (!is.null(colour_scale_type)) {
    if (colour_scale_type == "discrete") {
      if (rlang::is_null(colour_guide)) colour_guide <- ggplot2::guide_legend()
      plot <- plot +
        ggplot2::scale_colour_discrete(
          palette = colour_palette,
          breaks = colour_breaks %||% ggplot2::waiver(),
          drop = colour_drop,
          guide = colour_guide,
          labels = colour_labels %||% ggplot2::waiver(),
          limits = get_limits(colour_limits)
        )
    }
    else if (colour_scale_type == "continuous") {
      if (rlang::is_null(colour_guide)) colour_guide <- ggplot2::guide_colorbar()
      plot <- plot +
        ggplot2::scale_colour_continuous(
          breaks = colour_breaks %||% ggplot2::waiver(),
          n.breaks = colour_breaks_n,
          guide = colour_guide,
          labels = colour_labels %||% ggplot2::waiver(),
          limits = colour_limits,
          rescaler = colour_rescaler,
          transform = colour_transform %||% get_transform(colour_scale_temporal %||% NA_character_)
        )
    } else if (colour_scale_type == "binned") {
      if (rlang::is_null(colour_guide)) colour_guide <- ggplot2::guide_bins()
      plot <- plot +
        ggplot2::scale_colour_binned(
          breaks = colour_breaks %||% ggplot2::waiver(),
          n.breaks = colour_breaks_n,
          guide = colour_guide,
          labels = colour_labels %||% ggplot2::waiver(),
          limits = colour_limits,
          rescaler = colour_rescaler,
          transform = colour_transform %||% get_transform(colour_scale_temporal %||% NA_character_)
        )
    }
  }

  # Add fill scale based on type
  if (!is.null(fill_scale_type)) {
    if (fill_scale_type == "discrete") {
      if (rlang::is_null(fill_guide)) fill_guide <- ggplot2::guide_legend()
      plot <- plot +
        ggplot2::scale_fill_discrete(
          palette = fill_palette,
          breaks = fill_breaks %||% ggplot2::waiver(),
          drop = fill_drop,
          guide = fill_guide,
          labels = fill_labels %||% ggplot2::waiver(),
          limits = get_limits(fill_limits)
        )
    }
    else if (fill_scale_type == "continuous") {
      if (rlang::is_null(fill_guide)) fill_guide <- ggplot2::guide_colorbar()
      plot <- plot +
        ggplot2::scale_fill_continuous(
          breaks = fill_breaks %||% ggplot2::waiver(),
          n.breaks = fill_breaks_n,
          guide = fill_guide,
          labels = fill_labels %||% ggplot2::waiver(),
          limits = fill_limits,
          rescaler = fill_rescaler,
          transform = fill_transform %||% get_transform(fill_scale_temporal %||% NA_character_)
        )
    } else if (fill_scale_type == "binned") {
      if (rlang::is_null(fill_guide)) fill_guide <- ggplot2::guide_bins()
      plot <- plot +
        ggplot2::scale_fill_binned(
          breaks = fill_breaks %||% ggplot2::waiver(),
          n.breaks = fill_breaks_n,
          guide = fill_guide,
          labels = fill_labels %||% ggplot2::waiver(),
          limits = fill_limits,
          rescaler = fill_rescaler,
          transform = fill_transform %||% get_transform(fill_scale_temporal %||% NA_character_)
        )
    }
  }

  plot <- plot +
    coord +
    theme_to_aspect(aspect = aspect) +
    ggplot2::labs(
      x = x_title,
      y = y_title,
      colour = colour_title,
      fill = fill_title
    )

  plot
}

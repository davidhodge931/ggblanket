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
                       border_pop = \(x) scales::col_darker(x),
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
  # aesthetics <- Filter(Negate(rlang::quo_is_missing), aesthetics)
  aesthetics <- purrr::discard(aesthetics, rlang::quo_is_missing)

  # Separate fixed values from aesthetic mappings BEFORE inheritance
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics)

  # Store flags for whether colour/fill were originally provided
  has_colour_mapped <- "colour" %in% names(separated$mapped)
  has_colour_fixed <- "colour" %in% names(separated$fixed)
  has_fill_mapped <- "fill" %in% names(separated$mapped)
  has_fill_fixed <- "fill" %in% names(separated$fixed)

  # Need to add fixed fill and colour values to the ... in params
  # If not provided, need to get them both from the theme (e.g. ggplot2::get_theme()$geom@colour)
  # Once we have them, if they are within a border geom, we need to apply border_pop. Same logic rules as for the colour_palette

  # if (!has_colour_mapped & !has_colour_fixed) {
  #   colour <- ggplot2::get_theme()$geom@colour
  #   has_colour_fixed <- TRUE
  # }
  # if (!has_fill_mapped & !has_fill_fixed) {
  #   fill <- ggplot2::get_theme()$geom@fill
  #   has_fill_fixed <- TRUE
  # }

  # Convert geom, stat, position to appropriate objects
  geom_obj <- resolve_geom(geom)
  stat_obj <- resolve_stat(stat)

  geom_str <- class(geom_obj)[1] |>
    stringr::str_remove("^Geom") |>
    snakecase::to_snake_case()

  stat_str <- class(stat_obj)[1] |>
    stringr::str_remove("^Stat") |>
    snakecase::to_snake_case()

  ###############################################################################
  # Determine if we should apply border_pop to colour
  # This happens when:
  # 1. border_pop is provided
  # 2. colour was not explicitly provided (inherited from fill)
  # 3. geom is a border geom
  apply_border_pop_to_colour <- FALSE

  if (!is.null(border_pop) && !has_colour_mapped && !has_colour_fixed) {
    # Check if this is a border geom
    # Get shape from fixed aesthetics if available for point geoms
    if (is_geom_border(geom_str, separated$fixed[["shape"]])) {
      apply_border_pop_to_colour <- TRUE

      # For fixed fill values, apply border_pop directly to colour
      if (has_fill_fixed) {
        separated$fixed[["colour"]] <- border_pop(separated$fixed[["fill"]])
      } else if (has_fill_mapped) {
        # For mapped fill, inherit mapping (border_pop applied to palette later)
        separated$mapped[["colour"]] <- separated$mapped[["fill"]]
      }
    }
  }

  # Handle colour inheriting from fill (when border_pop not applied or no border geom)
  if (!has_colour_mapped && !has_colour_fixed) {
    if (has_fill_mapped && !"colour" %in% names(separated$mapped)) {
      separated$mapped[["colour"]] <- separated$mapped[["fill"]]
    } else if (has_fill_fixed && !"colour" %in% names(separated$fixed)) {
      separated$fixed[["colour"]] <- separated$fixed[["fill"]]
    }
  }

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
    if (is_stat_sf(stat_str)) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom_obj,
          stat = stat_obj,
          position = position,
          params = all_params
        )
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom_obj,
          stat = stat_obj,
          position = position,
          params = all_params
        )
    }
  }
  else {
    if (is_stat_sf(stat_str)) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom_obj,
          stat = stat_obj,
          position = position,
          params = all_params
        ) |> ggblend::blend(blend = blend)
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom_obj,
          stat = stat_obj,
          position = position,
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
      colour_transform <- colour_transform %||% fill_transform
      colour_title <- colour_title %||% fill_title

      # Handle colour palette
      if (apply_border_pop_to_colour) {
        # Border geom: derive colour_palette with border_pop applied

        # Get base palette - either user-provided fill_palette or from theme
        if (!is.null(fill_palette)) {
          base_palette <- fill_palette
        } else {
          # Get palette from theme based on scale type
          current_theme <- ggplot2::theme_get()
          if (fill_scale_type == "discrete") {
            base_palette <- current_theme$palette.fill.discrete
          } else if (fill_scale_type == "continuous") {
            base_palette <- current_theme$palette.fill.continuous
          } else {
            base_palette <- NULL
          }
        }

        # Apply border_pop to the base palette
        if (!is.null(base_palette)) {
          if (is.function(base_palette)) {
            colour_palette <- function(n) {
              colors <- base_palette(n)
              border_pop(colors)
            }
          } else if (is.character(base_palette)) {
            colour_palette <- border_pop(base_palette)
          }
        }
      } else {
        # Not a border geom: inherit fill_palette as-is
        # Don't need to get from theme since colour inherits from fill there too
        colour_palette <- colour_palette %||% fill_palette
      }
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

# Fixed set_blanket function
set_blanket <- function(
    #base
  theme = NULL,
  #geom
  fill = ifelse(is_panel_dark(), ocean, blue),
  shape = 21,
  linewidth = 0.66,
  size = 1.5,
  stroke = 0.5,
  #palette
  fill_palette = NULL,
  shape_palette = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype_palette = scales::pal_manual(1:6),
  #border
  border_geoms = NULL,
  border_linewidth = 0.25
) {

  #base
  base_theme <- if (!rlang::is_null(theme)) theme else ggplot2::theme_get()

  #geom - get all geom names
  all_geoms <- ls(pattern = "^Geom", envir = asNamespace("ggplot2"))
  all_geoms <- tolower(sub("^Geom", "", all_geoms))
  all_geoms <- all_geoms[all_geoms != ""]  # Remove empty string

  #border
  if (rlang::is_null(border_geoms)) {
    # Geoms with filled areas that benefit from thinner borders
    border_polygons <- c(
      "area", "bar", "bin2d", "boxplot", "col", "contourfilled",
      "crossbar", "density", "density2d", "density2dfilled", "dotplot",
      "hex", "map", "polygon", "raster", "rect", "ribbon", "sf",
      "tile", "violin"
    )

    if (shape %in% 21:25) {
      border_points <- c("point", "jitter", "count", "qq", "pointrange", "dotplot")
    }

    border_geoms <- c(border_polygons, border_points)
  }

  # Apply geom defaults to all geoms, with conditional linewidth
  geom_theme <- rlang::set_names(
    lapply(all_geoms, \(geom) {
      # Use border_linewidth for border geoms, otherwise use default linewidth
      lw <- if (geom %in% border_geoms) border_linewidth else linewidth

      ggplot2::element_geom(
        fill = fill,
        colour = fill,
        shape = shape,
        linewidth = lw,
        borderwidth = lw,
        size = size,
        stroke = stroke,
      )
    }),
    paste0("geom.", all_geoms)
  )

  #palette
  if (rlang::is_null(fill_palette)) {
    fill_palette_d <- scales::pal_hue()
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (length(fill_palette) == 1) {
    fill_palette_d <- unlist(fill_palette)
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (length(fill_palette) == 2) {
    fill_palette_d <- fill_palette[[1]]
    fill_palette_c <- fill_palette[[2]]
  }

  colour_palette_d <- fill_palette_d
  colour_palette_c <- fill_palette_c

  #set theme
  ggplot2::set_theme(
    base_theme +
      ggplot2::theme(
        palette.colour.discrete = colour_palette_d,
        palette.fill.discrete = fill_palette_d,
        palette.colour.continuous = colour_palette_c,
        palette.fill.continuous = fill_palette_c,
        palette.shape.discrete = shape_palette,
        palette.linetype.discrete = linetype_palette,
        !!!geom_theme
      )
  )

  invisible(NULL)
}

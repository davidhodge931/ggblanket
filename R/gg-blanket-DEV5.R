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
                       border = NULL,
                       highlight = \(x) scales::col_darker(x),
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
                       x_title = ggplot2::waiver(),
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
                       y_title = ggplot2::waiver(),
                       y_transform = NULL,
                       # fill scale arguments
                       fill_scale_type = NULL,
                       fill_scale_temporal = NULL,
                       fill_breaks = ggplot2::waiver(),
                       fill_breaks_n = 6,
                       fill_drop = FALSE,
                       fill_expand = NULL,
                       fill_guide = NULL,
                       fill_labels = NULL,
                       fill_limits = NULL,
                       fill_oob = scales::oob_keep,
                       fill_rescaler = scales::rescale,
                       fill_palette = NULL,
                       fill_title = ggplot2::waiver(),
                       fill_transform = "identity",
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
                       colour_title = ggplot2::waiver(),
                       colour_transform = NULL
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
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics)

  # Store flags for whether colour/fill were originally provided
  is_fill_mapped <- "fill" %in% names(separated$mapped)
  is_fill_fixed <- "fill" %in% names(separated$fixed)
  is_colour_mapped <- "colour" %in% names(separated$mapped)
  is_colour_fixed <- "colour" %in% names(separated$fixed)
  is_shape_mapped <- "shape" %in% names(separated$mapped)
  is_shape_fixed <- "shape" %in% names(separated$fixed)
  is_linewidth_mapped <- "linewidth" %in% names(separated$mapped)
  is_linewidth_fixed <- "linewidth" %in% names(separated$fixed)

  # Make colour mapped to fill, if fill is mapped and colour not specified
  if (is_fill_mapped & !is_colour_mapped & !is_colour_fixed) {
    separated$mapped$colour <- separated$mapped$fill
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

    if (border) computed_colour <- highlight(computed_colour)
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
  scale_info <- identify_scale(built)

  x_scale_type <- x_scale_type %||% scale_info$x$type
  y_scale_type <- y_scale_type %||% scale_info$y$type
  x_scale_temporal <- x_scale_temporal %||% scale_info$x$temporal
  y_scale_temporal <- y_scale_temporal %||% scale_info$y$temporal
  fill_scale_type <- fill_scale_type %||% scale_info$fill$type
  fill_scale_temporal <- fill_scale_temporal %||% scale_info$fill$temporal
  colour_scale_type <- colour_scale_type %||% scale_info$colour$type %||% scale_info$fill$type
  colour_scale_temporal <- colour_scale_temporal %||% scale_info$colour$temporal %||% scale_info$fill$temporal

  aspect <- aspect %||% get_aspect(built)
  coord <- coord %||% get_coord(stat_str, aspect)

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
        labels = x_labels %||% get_labels(stat_str, x_scale_temporal),
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
        labels = x_labels %||% get_labels(stat_str, x_scale_temporal),
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
        labels = y_labels %||% get_labels(stat_str, y_scale_temporal),
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
        labels = y_labels %||% get_labels(stat_str, y_scale_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        transform = y_transform %||% get_transform(y_scale_temporal)
      )
  }

  if (is_fill_mapped | is_colour_mapped) {
    if (rlang::is_null(fill_guide)) {
      if (fill_scale_type == "discrete") fill_guide <- ggplot2::guide_legend()
      if (fill_scale_type == "continuous") fill_guide <- ggplot2::guide_colourbar()
      if (fill_scale_type == "bins") fill_guide <- ggplot2::guide_bins()
    }
    if (rlang::is_null(fill_labels)) {
      if (colour_scale_type == "discrete") fill_labels <- ggplot2::waiver()
      if (colour_scale_type == "continuous") fill_labels <- scales::label_comma()
      if (colour_scale_type == "bins") fill_labels <- scales::label_comma()
    }

    # Add fill scale based on type
    if (!rlang::is_null(fill_scale_type)) {
      if (fill_scale_type == "discrete") {
        plot <- plot +
          ggplot2::scale_fill_discrete(
            palette = fill_palette %||% ggplot2::get_theme()$palette.fill.discrete,
            breaks = fill_breaks %||% ggplot2::waiver(),
            drop = fill_drop,
            guide = fill_guide,
            labels = fill_labels,
            limits = get_limits(fill_limits)
          )
      }
      else if (fill_scale_type == "continuous") {
        plot <- plot +
          ggplot2::scale_fill_continuous(
            palette = fill_palette %||% ggplot2::get_theme()$palette.fill.continuous,
            breaks = fill_breaks %||% ggplot2::waiver(),
            n.breaks = fill_breaks_n,
            guide = fill_guide,
            labels = fill_labels,
            limits = fill_limits,
            rescaler = fill_rescaler,
            transform = fill_transform %||% get_transform(fill_scale_temporal %||% NA_character_)
          )
      } else if (fill_scale_type == "binned") {
        plot <- plot +
          ggplot2::scale_fill_binned(
            palette = fill_palette %||% ggplot2::get_theme()$palette.fill.continuous,
            breaks = fill_breaks %||% ggplot2::waiver(),
            n.breaks = fill_breaks_n,
            guide = fill_guide,
            labels = fill_labels,
            limits = fill_limits,
            rescaler = fill_rescaler,
            transform = fill_transform %||% get_transform(fill_scale_temporal %||% NA_character_)
          )
      }
    }

    # Add colour scale based on type - with fill fallbacks
    if (!rlang::is_null(colour_scale_type)) {
      if (colour_scale_type == "discrete") {
        if (border) {
          colour_palette <- colour_palette %||% highlight(fill_palette) %||%
            highlight(ggplot2::get_theme()$palette.fill.discrete)
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
            guide = colour_guide  %||%  fill_guide,
            labels = colour_labels %||% fill_labels,
            limits = colour_limits %||% fill_limits
          )
      }
      else if (colour_scale_type == "continuous") {
        if (border) {
          colour_palette <- colour_palette %||% highlight(fill_palette) %||%
            highlight(ggplot2::get_theme()$palette.fill.continuous)
        }
        else {
          colour_palette <- colour_palette %||% fill_palette %||%
            ggplot2::get_theme()$palette.fill.continuous
        }

        plot <- plot +
          ggplot2::scale_colour_continuous(
            palette = colour_palette ,
            breaks = colour_breaks %||% fill_breaks,
            n.breaks = colour_breaks_n %||% fill_breaks_n,
            guide = colour_guide %||% if (border) ggplot2::guide_none() else fill_guide,
            labels = colour_labels %||% fill_labels,
            limits = colour_limits %||% fill_limits,
            rescaler = colour_rescaler %||% fill_rescaler,
            transform = colour_transform %||% fill_transform
          )
      } else if (colour_scale_type == "binned") {
        if (border) {
          colour_palette <- colour_palette %||% highlight(fill_palette) %||%
            highlight(ggplot2::get_theme()$palette.fill.continuous)
        }
        else {
          colour_palette <- colour_palette %||% fill_palette %||%
            ggplot2::get_theme()$palette.fill.continuous
        }

        plot <- plot +
          ggplot2::scale_colour_binned(
            palette = colour_palette ,
            breaks = colour_breaks %||% fill_breaks,
            n.breaks = colour_breaks_n %||% fill_breaks_n,
            guide = colour_guide %||% if (border) ggplot2::guide_none() else fill_guide,
            labels = colour_labels %||% fill_labels,
            limits = colour_limits %||% fill_limits,
            rescaler = colour_rescaler %||% fill_rescaler,
            transform = colour_transform %||% fill_transform
          )
      }
    }
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

  # Insert this code right after you add all the scales (after line ~545)
  # and before you add coord + theme

  # Extract scale information
  fill_scale <- NULL
  colour_scale <- NULL

  for (i in seq_along(plot$scales$scales)) {
    scale <- plot$scales$scales[[i]]
    if ("fill" %in% scale$aesthetics) {
      fill_scale <- scale
    }
    if ("colour" %in% scale$aesthetics) {
      colour_scale <- scale
    }
  }

  # Print comparison
  cat("\n=== FILL SCALE ===\n")
  if (!is.null(fill_scale)) {
    cat("Class:", class(fill_scale), "\n")
    cat("Aesthetics:", paste(fill_scale$aesthetics, collapse = ", "), "\n")
    cat("Palette function exists:", !is.null(fill_scale$palette), "\n")
    if (!is.null(fill_scale$palette)) {
      cat("Palette is function:", is.function(fill_scale$palette), "\n")
      # Try to see what the palette produces
      tryCatch({
        cat("Sample palette output:", paste(fill_scale$palette(3), collapse = ", "), "\n")
      }, error = function(e) {
        cat("Could not sample palette:", e$message, "\n")
      })
    }
    cat("Breaks:", paste(fill_scale$breaks, collapse = ", "), "\n")
    cat("Limits:", paste(fill_scale$limits, collapse = ", "), "\n")
    cat("Guide:", class(fill_scale$guide), "\n")
  } else {
    cat("No fill scale found\n")
  }

  cat("\n=== COLOUR SCALE ===\n")
  if (!is.null(colour_scale)) {
    cat("Class:", class(colour_scale), "\n")
    cat("Aesthetics:", paste(colour_scale$aesthetics, collapse = ", "), "\n")
    cat("Palette function exists:", !is.null(colour_scale$palette), "\n")
    if (!is.null(colour_scale$palette)) {
      cat("Palette is function:", is.function(colour_scale$palette), "\n")
      tryCatch({
        cat("Sample palette output:", paste(colour_scale$palette(3), collapse = ", "), "\n")
      }, error = function(e) {
        cat("Could not sample palette:", e$message, "\n")
      })
    }
    cat("Breaks:", paste(colour_scale$breaks, collapse = ", "), "\n")
    cat("Limits:", paste(colour_scale$limits, collapse = ", "), "\n")
    cat("Guide:", class(colour_scale$guide), "\n")
  } else {
    cat("No colour scale found\n")
  }

  cat("\n=== COMPARISON ===\n")
  cat("is_fill_mapped:", is_fill_mapped, "\n")
  cat("is_colour_mapped:", is_colour_mapped, "\n")
  cat("border:", border, "\n")
  cat("fill_palette provided:", !is.null(fill_palette), "\n")
  cat("colour_palette provided:", !is.null(colour_palette), "\n")

  # Also print the actual palette variables being used
  cat("\n=== PALETTE VARIABLES ===\n")
  cat("fill_palette:", if(is.function(fill_palette)) "function" else fill_palette, "\n")
  cat("colour_palette:", if(is.function(colour_palette)) "function" else colour_palette, "\n")

  plot
}

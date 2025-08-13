# Complete Color Scale Functions with Border Transformation Support

#' Process palette to vector for border transformation
#' @noRd
process_palette_to_vector <- function(palette, n = NULL, scale_type = "continuous") {
  if (is.function(palette)) {
    if (scale_type == "discrete" || scale_type == "ordinal") {
      # For discrete/ordinal, we need to know how many colors
      if (is.null(n)) {
        # Default to a reasonable number if not specified
        n <- 10
      }
      # Try to call the palette function
      tryCatch(
        palette(n),
        error = function(e) {
          # If it fails, try as continuous with sequence
          palette(seq(0, 1, length.out = n))
        }
      )
    } else {
      # For continuous/binned scales, generate a gradient
      # Get a representative set of colors
      tryCatch(
        {
          # Try calling with values from 0 to 1
          palette(seq(0, 1, length.out = 256))
        },
        error = function(e) {
          # If that fails, try calling with n
          palette(256)
        }
      )
    }
  } else if (is.character(palette) || is.numeric(palette)) {
    # Already a vector
    palette
  } else {
    # Unknown type, return as-is
    palette
  }
}

# Complete Color Scale Functions - Final Version

#' Reverse discrete palette for horizontal plots
#' @noRd
reverse_discrete_palette <- function(palette, n = NULL, aspect = "x") {
  # Only reverse if aspect is "y"
  if (aspect != "y") {
    return(palette)
  }

  if (is.function(palette)) {
    # Capture all attributes from the original function
    original_attrs <- attributes(palette)

    # For functions, return a wrapper that reverses the output
    wrapped_fn <- function(n) {
      colors <- palette(n)
      rev(colors)
    }

    # CRITICAL: Preserve ALL attributes from the original function
    # This includes class, type, nlevels, etc.
    attributes(wrapped_fn) <- original_attrs

    return(wrapped_fn)
  } else if (is.character(palette) || is.numeric(palette)) {
    # For vectors
    if (any(rlang::have_name(palette))) {
      # Named vector - reverse the whole thing
      rev(palette)
    } else {
      # Unnamed vector - subset first if n is provided, then reverse
      if (!is.null(n)) {
        palette <- palette[1:min(n, length(palette))]
      }
      rev(palette)
    }
  } else {
    # Unknown type, return as-is
    palette
  }
}

#' Check if colour and fill are mapped to same variable in plot
#' @noRd
check_same_colour_fill_mapping <- function(plot) {
  # Get the mapping from the plot object
  plot_mapping <- plot$mapping

  # Check if both colour and fill exist in the mapping
  has_colour <- !rlang::is_null(plot_mapping$colour)
  has_fill <- !rlang::is_null(plot_mapping$fill)

  if (has_colour && has_fill) {
    # Compare the expressions
    colour_expr <- rlang::quo_text(plot_mapping$colour)
    fill_expr <- rlang::quo_text(plot_mapping$fill)
    return(identical(colour_expr, fill_expr))
  }

  return(FALSE)
}

#' Add color scales wrapper
#' @noRd
add_col_scale <- function(
    plot,
    geom_name,
    stat_name = NULL,
    col_scale_class,
    aes_list,
    data,
    plot_data,
    plot_build,
    x_limits_to_breaks,
    is_border_geom,
    col_breaks,
    col_breaks_n,
    col_drop,
    col_limits_include,
    col_labels,
    col_legend_ncol,
    col_legend_nrow,
    col_legend_rev,
    col_rescale,
    col_scale_type,
    col_transform,
    colour_palette_d,
    colour_palette_c,
    colour_palette_o,
    colour_na,
    fill_palette_d,
    fill_palette_c,
    fill_palette_o,
    fill_na,
    # border_transform_colour = NULL,
    # border_transform_fill = NULL,
    aspect = "x"  # ADD THIS PARAMETER
) {
  # Get NA colors with defaults
  na_colour <- colour_na %||% "#CDC5BFFF"
  na_fill <- fill_na %||% "#CDC5BFFF"

  # Get transform and labels
  if (rlang::is_null(col_transform)) {
    # Special handling for color transforms
    if (!rlang::quo_is_null(aes_list$col) && col_scale_class == "continuous") {
      col_data <- rlang::eval_tidy(aes_list$col, data)
      if (inherits(col_data, "hms")) {
        col_transform <- "hms"
      } else {
        col_transform <- get_transform(NULL, col_scale_class)
      }
    } else {
      col_transform <- get_transform(NULL, col_scale_class)
    }
  }
  col_labels <- get_col_label(col_labels, col_scale_class, col_transform)

  # Apply scales based on type
  if (col_scale_class == "discrete") {
    plot <- add_col_scale_discrete(
      plot,
      aes_list,
      data,
      plot_data,
      colour_palette_d,
      fill_palette_d,
      na_colour,
      na_fill,
      col_breaks,
      col_labels,
      col_drop,
      col_legend_ncol,
      col_legend_nrow,
      col_legend_rev,
      x_limits_to_breaks,
      plot_build,
      stat_name = stat_name,
      # border_transform_colour = border_transform_colour,
      # border_transform_fill = border_transform_fill,
      is_border_geom = is_border_geom,
      aspect = aspect  # PASS ASPECT
    )
  } else if (col_scale_class %in% c("continuous", "date", "datetime", "time")) {
    plot <- add_col_scale_continuous(
      plot,
      colour_palette_c,
      fill_palette_c,
      na_colour,
      na_fill,
      is_border_geom,
      col_breaks,
      col_breaks_n,
      col_labels,
      col_legend_rev,
      col_rescale,
      col_scale_type,
      col_transform,
      aes_list,
      plot_build,
      # border_transform_colour = border_transform_colour,
      # border_transform_fill = border_transform_fill,
      aspect = aspect  # PASS ASPECT (though not used)
    )
  } else if (col_scale_class == "ordinal") {
    plot <- add_col_scale_ordinal(
      plot,
      aes_list,
      data,
      plot_data,
      colour_palette_o,
      fill_palette_o,
      na_colour,
      na_fill,
      col_breaks,
      col_labels,
      col_drop,
      col_legend_ncol,
      col_legend_nrow,
      col_legend_rev,
      plot_build,
      stat_name = stat_name,
      # border_transform_colour = border_transform_colour,
      # border_transform_fill = border_transform_fill,
      is_border_geom = is_border_geom,
      aspect = aspect  # PASS ASPECT (though not used)
    )
  }

  # Handle guides for other aesthetics - pass aes_list directly
  plot <- add_matching_aesthetic_guides(
    plot,
    plot_build,
    col_legend_rev,
    col_legend_ncol,
    col_legend_nrow,
    geom_name = geom_name,
    is_border_geom = is_border_geom,
    # border_transform_colour = border_transform_colour,
    # border_transform_fill = border_transform_fill,
    aes_list = aes_list,
    data = data
  )

  # Expand limits if necessary
  if (!rlang::is_null(col_limits_include)) {
    plot <- plot +
      ggplot2::expand_limits(
        colour = col_limits_include,
        fill = col_limits_include
      )
  }

  plot
}

#' Add discrete color scale
#' @noRd
add_col_scale_discrete <- function(
    plot,
    aes_list,
    data,
    plot_data,
    colour_palette,
    fill_palette,
    na_colour,
    na_fill,
    col_breaks,
    col_labels,
    col_drop,
    col_legend_ncol,
    col_legend_nrow,
    col_legend_rev,
    x_limits_to_breaks,
    plot_build,
    stat_name = NULL,
    # border_transform_colour = NULL,
    # border_transform_fill = NULL,
    is_border_geom = FALSE,
    aspect = "x"
) {
  # Determine legend reversal based on aspect if not explicitly set
  if (rlang::is_null(col_legend_rev)) {
    if (aspect == "y") {
      col_legend_rev <- TRUE
    } else {
      col_legend_rev <- FALSE
    }
  }

  # Calculate number of colors needed
  col_n <- get_col_n(aes_list, data, plot_data, stat_name)

  # Reverse palettes if aspect is "y"
  colour_palette <- reverse_discrete_palette(colour_palette, n = col_n, aspect = aspect)
  fill_palette <- reverse_discrete_palette(fill_palette, n = col_n, aspect = aspect)

  # Apply border transformations
  # if (is_border_geom && !is.null(border_transform_colour) && is.function(border_transform_colour)) {
  #   colour_palette <- border_transform_colour(colour_palette)
  # } else {
  #   colour_palette <- colour_palette
  # }

  # if (is_border_geom && !is.null(border_transform_fill) && is.function(border_transform_fill)) {
  #   fill_palette <- border_transform_fill(fill_palette)
  # } else {
  #   fill_palette <- fill_palette
  # }

  # Apply scales with the appropriate palette
  if (!rlang::is_null(colour_palette)) {
    plot <- plot +
      ggplot2::scale_colour_discrete(
        palette = colour_palette,
        breaks = col_breaks,
        labels = col_labels,
        na.value = na_colour,
        drop = col_drop
      )
  }

  if (!rlang::is_null(fill_palette)) {
    plot <- plot +
      ggplot2::scale_fill_discrete(
        palette = fill_palette,
        breaks = col_breaks,
        labels = col_labels,
        na.value = na_fill,
        drop = col_drop
      )
  }

  # Check if colour and fill map to the same variable using plot object
  same_mapping <- check_same_colour_fill_mapping(plot)

  if (same_mapping) {
    # Same variable mapped to both - typically show both for discrete
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
  } else {
    # Different variables or only one is mapped - show appropriate guides
    plot_mapping <- plot$mapping
    has_colour_mapping <- !rlang::is_null(plot_mapping$colour)
    has_fill_mapping <- !rlang::is_null(plot_mapping$fill)

    guide_list <- list()

    if (has_colour_mapping) {
      guide_list$colour <- ggplot2::guide_legend(
        reverse = col_legend_rev,
        ncol = col_legend_ncol,
        nrow = col_legend_nrow
      )
    }

    if (has_fill_mapping) {
      guide_list$fill <- ggplot2::guide_legend(
        reverse = col_legend_rev,
        ncol = col_legend_ncol,
        nrow = col_legend_nrow
      )
    }

    if (length(guide_list) > 0) {
      plot <- plot + rlang::exec(ggplot2::guides, !!!guide_list)
    }
  }

  plot
}

#' Add continuous color scale
#' @noRd
add_col_scale_continuous <- function(
    plot,
    colour_palette,
    fill_palette,
    na_colour,
    na_fill,
    is_border_geom,
    col_breaks,
    col_breaks_n,
    col_labels,
    col_legend_rev,
    col_rescale,
    col_scale_type,
    col_transform,
    aes_list,
    plot_build,
    # border_transform_colour = NULL,
    # border_transform_fill = NULL,
    aspect = "x"  # Add aspect parameter (but not used for continuous)
) {
  # For continuous scales, aspect doesn't affect legend reversal
  if (rlang::is_null(col_legend_rev)) {
    col_legend_rev <- FALSE
  }

  # Apply border transformations directly to palette functions if needed
  # if (is_border_geom && !is.null(border_transform_colour) && is.function(border_transform_colour)) {
  #   colour_palette <- border_transform_colour(colour_palette)
  # } else {
  #   colour_palette <- colour_palette
  # }

  # if (is_border_geom && !is.null(border_transform_fill) && is.function(border_transform_fill)) {
  #   fill_palette <- border_transform_fill(fill_palette)
  # } else {
  #   fill_palette <- fill_palette
  # }

  # Choose scale type
  if (col_scale_type == "binned") {
    # Use binned scales
    plot <- plot +
      ggplot2::scale_colour_binned(
        palette = colour_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = na_colour
      ) +
      ggplot2::scale_fill_binned(
        palette = fill_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = na_fill
      )

    # Check if colour and fill map to the same variable
    same_mapping <- check_same_colour_fill_mapping(plot)

    if (same_mapping) {
      # Same variable mapped to both - hide one guide
      if (is_border_geom) {
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
    } else {
      # Different variables or only one is mapped - show appropriate guides
      plot_mapping <- plot$mapping
      has_colour_mapping <- !rlang::is_null(plot_mapping$colour)
      has_fill_mapping <- !rlang::is_null(plot_mapping$fill)

      guide_list <- list()

      if (has_colour_mapping) {
        guide_list$colour <- ggplot2::guide_coloursteps(
          reverse = col_legend_rev,
          theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
        )
      }

      if (has_fill_mapping) {
        guide_list$fill <- ggplot2::guide_coloursteps(
          reverse = col_legend_rev,
          theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
        )
      }

      if (length(guide_list) > 0) {
        plot <- plot + rlang::exec(ggplot2::guides, !!!guide_list)
      }
    }
  } else {
    # Default to gradient/continuous scales
    plot <- plot +
      ggplot2::scale_colour_continuous(
        palette = colour_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = na_colour
      ) +
      ggplot2::scale_fill_continuous(
        palette = fill_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform,
        oob = scales::oob_keep,
        na.value = na_fill
      )

    # Check if colour and fill map to the same variable
    same_mapping <- check_same_colour_fill_mapping(plot)

    if (same_mapping) {
      # Same variable mapped to both - hide one guide
      if (is_border_geom) {
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
    } else {
      # Different variables or only one is mapped - show appropriate guides
      plot_mapping <- plot$mapping
      has_colour_mapping <- !rlang::is_null(plot_mapping$colour)
      has_fill_mapping <- !rlang::is_null(plot_mapping$fill)

      guide_list <- list()

      if (has_colour_mapping) {
        guide_list$colour <- ggplot2::guide_colourbar(reverse = col_legend_rev)
      }

      if (has_fill_mapping) {
        guide_list$fill <- ggplot2::guide_colourbar(reverse = col_legend_rev)
      }

      if (length(guide_list) > 0) {
        plot <- plot + rlang::exec(ggplot2::guides, !!!guide_list)
      }
    }
  }

  plot
}

#' Add ordinal color scale
#' @noRd
add_col_scale_ordinal <- function(
    plot,
    aes_list,
    data,
    plot_data,
    colour_palette,
    fill_palette,
    na_colour,
    na_fill,
    col_breaks,
    col_labels,
    col_drop,
    col_legend_ncol,
    col_legend_nrow,
    col_legend_rev,
    plot_build,
    stat_name = NULL,
    # border_transform_colour = NULL,
    # border_transform_fill = NULL,
    is_border_geom = FALSE,
    aspect = "x"  # Add aspect parameter (but not used for ordinal)
) {
  # Calculate number of colors needed
  col_n <- get_col_n(aes_list, data, plot_data, stat_name)

  # For ordinal scales, aspect doesn't affect legend reversal
  # Just handle the default and always invert
  if (rlang::is_null(col_legend_rev)) {
    col_legend_rev <- TRUE  # Default before inversion
  }

  # Always invert for ordinal scales
  # col_legend_rev <- !col_legend_rev

  # Apply border transformations directly to palette functions if needed
  # if (is_border_geom && !is.null(border_transform_colour) && is.function(border_transform_colour)) {
  #   colour_palette <- border_transform_colour(colour_palette)
  # } else {
  #   colour_palette <- colour_palette
  # }

  # if (is_border_geom && !is.null(border_transform_fill) && is.function(border_transform_fill)) {
  #   fill_palette <- border_transform_fill(fill_palette)
  # } else {
  #   fill_palette <- fill_palette
  # }

  # For ordinal scales, use discrete_scale directly to avoid the duplicate palette argument issue
  if (!rlang::is_null(colour_palette)) {
    if (is.function(colour_palette)) {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "colour",
          scale_name = "ordinal",
          palette = colour_palette,
          breaks = col_breaks,
          labels = col_labels,
          na.value = na_colour,
          drop = col_drop
        )
    } else {
      # If it's a vector, use scale_colour_manual
      plot <- plot +
        ggplot2::scale_colour_manual(
          values = colour_palette,
          breaks = col_breaks,
          labels = col_labels,
          na.value = na_colour,
          drop = col_drop
        )
    }
  }

  if (!rlang::is_null(fill_palette)) {
    if (is.function(fill_palette)) {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "fill",
          scale_name = "ordinal",
          palette = fill_palette,
          breaks = col_breaks,
          labels = col_labels,
          na.value = na_fill,
          drop = col_drop
        )
    } else {
      # If it's a vector, use scale_fill_manual
      plot <- plot +
        ggplot2::scale_fill_manual(
          values = fill_palette,
          breaks = col_breaks,
          labels = col_labels,
          na.value = na_fill,
          drop = col_drop
        )
    }
  }

  # Check if colour and fill map to the same variable using plot object
  same_mapping <- check_same_colour_fill_mapping(plot)

  if (same_mapping) {
    # Same variable mapped to both - show both guides for ordinal
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
  } else {
    # Different variables or only one is mapped - show appropriate guides
    plot_mapping <- plot$mapping
    has_colour_mapping <- !rlang::is_null(plot_mapping$colour)
    has_fill_mapping <- !rlang::is_null(plot_mapping$fill)

    guide_list <- list()

    if (has_colour_mapping) {
      guide_list$colour <- ggplot2::guide_legend(
        reverse = col_legend_rev,
        ncol = col_legend_ncol,
        nrow = col_legend_nrow
      )
    }

    if (has_fill_mapping) {
      guide_list$fill <- ggplot2::guide_legend(
        reverse = col_legend_rev,
        ncol = col_legend_ncol,
        nrow = col_legend_nrow
      )
    }

    if (length(guide_list) > 0) {
      plot <- plot + rlang::exec(ggplot2::guides, !!!guide_list)
    }
  }

  plot
}

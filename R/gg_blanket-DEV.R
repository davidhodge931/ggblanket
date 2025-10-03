#' A modern wrapper that creates a complete ggplot with a layer, accepting all
#' possible aesthetics as unquoted arguments. The `col` aesthetic is mapped to
#' both colour and fill.
#'
#' @param data A data frame.
#' @param ... Arguments passed to [ggplot2::layer(params = list(...))].
#' @param geom A geom as either character ("point") or object (ggplot2::GeomPoint).
#' @param stat A stat as either character ("identity") or object (ggplot2::StatIdentity).
#' @param position A position as either function (ggplot2::position_identity()) or object (ggplot2::PositionIdentity).
#' @param x Variable mapped to x, or a set value.
#' @param xmin Variable mapped to xmin, or a set value.
#' @param xmax Variable mapped to xmax, or a set value.
#' @param xend Variable mapped to xend, or a set value.
#' @param y Variable mapped to y, or a set value.
#' @param ymin Variable mapped to ymin, or a set value.
#' @param ymax Variable mapped to ymax, or a set value.
#' @param yend Variable mapped to yend, or a set value.
#' @param z Variable mapped to z, or a set value.
#' @param col Variable mapped to both colour and fill, or a set value.
#' @param alpha Variable mapped to alpha, or a set value.
#' @param shape Variable mapped to shape, or a set value.
#' @param linetype Variable mapped to linetype, or a set value.
#' @param linewidth Variable mapped to linewidth, or a set value.
#' @param size Variable mapped to size, or a set value.
#' @param stroke Variable mapped to stroke, or a set value.
#' @param label Variable mapped to label, or a set value.
#' @param weight Variable mapped to weight, or a set value.
#' @param group Variable mapped to group, or a set value.
#' @param width Variable mapped to width, or a set value.
#' @param height Variable mapped to height, or a set value.
#' @param slope Variable mapped to slope, or a set value.
#' @param intercept Variable mapped to intercept, or a set value.
#' @param xintercept Variable mapped to xintercept, or a set value.
#' @param yintercept Variable mapped to yintercept, or a set value.
#' @param sample Variable mapped to sample, or a set value.
#' @param angle Variable mapped to angle, or a set value.
#' @param radius Variable mapped to radius, or a set value.
#' @param mapping Additional aesthetic mapping within a [ggplot2::aes] call.
#'
#' @return A complete ggplot2 object
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   gg_blanket(
#'     geom = "point",
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
#' @export
gg_blanket <- function(data,
                       ...,
                       geom = "blank",
                       stat = "identity",
                       position = ggplot2::position_identity(),
                       coord = NULL,
                       aspect = NULL,
                       blend = NULL,
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
                       col = NULL,
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
                       x_labels = NULL,
                       x_limits = NULL,
                       x_oob = scales::oob_keep,
                       x_palette = seq_len,
                       x_position = "bottom",
                       x_sec_axis = ggplot2::waiver(),
                       x_transform = NULL,
                       # y scale arguments
                       y_scale_type = NULL,
                       y_scale_temporal = NULL,
                       y_breaks = NULL,
                       y_breaks_minor = ggplot2::waiver(),
                       y_breaks_n = 6,
                       y_drop = FALSE,
                       y_expand = NULL,
                       y_labels = NULL,
                       y_limits = NULL,
                       y_oob = scales::oob_keep,
                       y_palette = seq_len,
                       y_position = "left",
                       y_sec_axis = ggplot2::waiver(),
                       y_transform = NULL
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
    col = col,
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
  # aesthetics <- purrr::discard(aesthetics, rlang::quo_is_missing)
  aesthetics <- Filter(Negate(rlang::quo_is_missing), aesthetics)

  # Separate fixed values from aesthetic mappings
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics)

  # Handle col -> colour/fill mapping for mapped aesthetics
  final_aesthetics <- handle_col_aesthetic(separated$mapped)

  # Convert geom, stat, position to appropriate objects
  geom_obj <- resolve_geom(geom)
  stat_obj <- resolve_stat(stat)
  position_obj <- resolve_position(position)

  # Combine individual aesthetics with mapping argument
  final_mapping <- combine_aesthetics(final_aesthetics, mapping)

  # Capture additional parameters from ...
  additional_params <- rlang::list2(...)

  # Combine fixed aesthetic values with additional parameters
  all_params <- utils::modifyList(separated$fixed, additional_params)

  # Build initial plot to determine scale types
  if (is.null(blend)) {
    if (is_stat_sf(stat)) {
      plot <- ggplot2::ggplot(data, mapping = final_mapping) +
        ggplot2::layer_sf(
          geom = geom_obj,
          stat = stat_obj,
          position = position_obj,
          params = all_params
        )
    }
    else {
      plot <- ggplot2::ggplot(data, mapping = final_mapping) +
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
        plot <- ggplot2::ggplot(data, mapping = final_mapping) +
          ggplot2::layer_sf(
            geom = geom_obj,
            stat = stat_obj,
            position = position_obj,
            params = all_params
        ) |> ggblend::blend(blend = blend)
      }
      else {
        plot <- ggplot2::ggplot(data, mapping = final_mapping) +
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
        labels = x_labels %||% ggplot2::waiver(),
        limits = get_scale_limits(x_limits),
        continuous.limits = get_scale_limits_continuous(x_limits),
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
        expand = x_expand %||% get_scale_expand(scale_info$x$limits),
        labels = x_labels %||% get_scale_labels(stat, x_scale_temporal),
        limits = x_limits,
        oob = x_oob,
        position = x_position,
        sec.axis = x_sec_axis,
        transform = x_transform %||% get_scale_transform(x_scale_temporal)
      )
  } else if (x_scale_type == "binned") {
    plot <- plot +
      ggplot2::scale_x_binned(
        breaks = x_breaks %||% ggplot2::waiver(),
        n.breaks = x_breaks_n,
        expand = x_expand %||% get_scale_expand(scale_info$x$limits),
        labels = x_labels %||% get_scale_labels(stat, x_scale_temporal),
        limits = x_limits,
        oob = x_oob,
        position = x_position,
        transform = x_transform %||% get_scale_transform(x_scale_temporal)
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
        labels = y_labels %||% ggplot2::waiver(),
        limits = get_scale_limits(y_limits),
        continuous.limits = get_scale_limits_continuous(y_limits),
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
        expand = y_expand %||% get_scale_expand(scale_info$y$limits),
        labels = y_labels %||% get_scale_labels(stat, y_scale_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        sec.axis = y_sec_axis,
        transform = y_transform %||% get_scale_transform(y_scale_temporal)
      )
  } else if (y_scale_type == "binned") {
    plot <- plot +
      ggplot2::scale_y_binned(
        breaks = y_breaks %||% ggplot2::waiver(),
        n.breaks = y_breaks_n,
        expand = y_expand %||% get_scale_expand(scale_info$y$limits),
        labels = y_labels %||% get_scale_labels(stat, y_scale_temporal),
        limits = y_limits,
        oob = y_oob,
        position = y_position,
        transform = y_transform %||% get_scale_transform(y_scale_temporal)
      )
  }

  plot <- plot +
    coord +
    ggplot2::get_theme() +
    theme_to_aspect(aspect = aspect)

  plot
}



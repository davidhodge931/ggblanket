# gg_blanket_utils.R - Utility functions for gg_blanket

# Core utility functions ----

#' Extract ggproto name helper
#' @noRd
get_ggproto_name <- function(object, prefix) {
  if (ggplot2::is_ggproto(object)) {
    class(object)[1] |>
      stringr::str_remove(prefix) |>
      stringr::str_to_lower()
  } else if (is.character(object)) {
    object
  } else {
    NULL
  }
}

#' Extract transform name
#' @noRd
get_transform_name <- function(transform) {
  if (inherits(transform, "transform")) {
    transform$name |>
      stringr::str_remove("composition") |>
      stringr::str_remove_all("[()]") |>
      stringr::str_split(",") |>
      unlist()
  } else if (is.character(transform)) {
    transform
  } else {
    NULL
  }
}

#' Get params based on geom_name type
#' @noRd
get_geom_params <- function(geom_name, ...) {
  # Define geom_name-specific parameters
  geom_name_params <- list(
    boxplot = list(
      median_gp = list(
        linewidth = ggplot2::get_geom_defaults("line")$linewidth
      ),
      whisker_gp = list(
        linewidth = ggplot2::get_geom_defaults("line")$linewidth
      ),
      staple_gp = list(
        linewidth = ggplot2::get_geom_defaults("line")$linewidth
      ),
      box_gp = list(
        linewidth = ggplot2::get_geom_defaults("rect")$linewidth
      ),
      outlier_gp = list(stroke = 0)
    ),
    crossbar = list(
      middle_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth)
    ),
    violin = list(
      quantile_gp = list(linetype = 1)
    )
  )

  # Get specific params or empty list
  specific_params <- geom_name_params[[geom_name]] %||% list()

  # Combine with additional parameters
  rlang::list2(!!!specific_params, ...)
}

#' Add initial layer to plot
#' @noRd
add_initial_layer <- function(
    plot,
    geom,
    geom_name,
    stat,
    stat_name,
    position,
    params,
    show_legend,
    coord,
    blend
) {
  # Determine if using sf
  is_sf <- stringr::str_detect(stat_name, "sf")

  # Set default coord if not provided
  if (rlang::is_null(coord)) {
    coord <- if (is_sf) {
      ggplot2::coord_sf(clip = "off")
    } else {
      ggplot2::coord_cartesian(clip = "off")
    }
  }

  # Create layer function
  layer_fn <- if (is_sf) ggplot2::layer_sf else ggplot2::layer

  # Build layer
  layer_call <- layer_fn(
    geom = geom,
    stat = stat,
    position = position,
    params = params,
    show.legend = show_legend
  )

  # Add layer with optional blending
  if (!rlang::is_null(blend)) {
    plot + layer_call |> ggblend::blend(blend = blend) + coord
  } else {
    plot + layer_call + coord
  }
}

# Default value functions ----

#' Get transform based on scale class
#' @noRd
.get_transform <- function(transform = NULL, scale_subclass= NULL) {
  transform %||%
    switch(
      scale_subclass,
      time = "hms", # time scale class uses hms transform
      datetime = "time", # datetime scales use "time" transform
      date = "date",
      "identity" # default
    )
}

.get_aspect <- function(aspect = NULL, x_scale_subclass, y_scale_subclass, stat_name, aes_list, mapping = NULL) {
  aspect %||% {
    # Check if x is mapped (either in aes_list or mapping)
    has_x <- !rlang::quo_is_null(aes_list$x) || (!rlang::is_null(mapping) && "x" %in% names(mapping))

    # Check if y is mapped (either in aes_list or mapping)
    has_y <- !rlang::quo_is_null(aes_list$y) || (!rlang::is_null(mapping) && "y" %in% names(mapping))

    if (y_scale_subclass== "discrete" && x_scale_subclass!= "discrete") {
      "y"
    } else if (stat_name %in% c("bin", "density") && !has_x) {
      "y"
    } else if (stat_name %in% c("bin", "density") && has_x && !has_y) {
      "x"  # This is your second case - x mapped, y computed
    } else {
      "x"
    }
  }
}

# Input validation functions ----

#' Validate inputs are valid
#' @noRd
.validate_inputs <- function(
    mapping,
    aspect,
    x_limits_to_breaks,
    y_limits_to_breaks,
    x_transform,
    y_transform,
    stat_name
) {
  # Check mapping doesn't include facets
  if (!rlang::is_null(mapping)) {
    mapping_names <- names(unlist(mapping))
    if (any(mapping_names %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  if (!aspect %in% c("x", "y")) {
    rlang::abort("aspect can only be 'x' or 'y'")
  }

  both_symmetric <- x_limits_to_breaks && y_limits_to_breaks
  non_identity_x_transform <- !x_transform %in% c("identity", "reverse")
  non_identity_y_transform <- !y_transform %in% c("identity", "reverse")
  non_identity_stat_name <- !identical(stat_name, "identity")

  if (
    both_symmetric &&
    (non_identity_x_transform ||
     non_identity_y_transform ||
     non_identity_stat_name)
  ) {
    rlang::abort(c(
      "symmetric == 'both' not supported where",
      "a positional axis is transformed or the stat_name is not 'identity'"
    ))
  }
}

# Data processing functions ----

#' Process data for factors and reversing
#' @noRd
.process_data <- function(data, aes_list, aspect) {
  # Get non-NULL aesthetics
  active_aes <- list(
    aes_list$x,
    aes_list$xmin,
    aes_list$xmax,
    aes_list$xend,
    aes_list$y,
    aes_list$ymin,
    aes_list$ymax,
    aes_list$yend,
    aes_list$col,
    aes_list$facet,
    aes_list$facet2,
    aes_list$shape,
    aes_list$linetype
  ) |>
    purrr::discard(rlang::quo_is_null)

  data |>
    dplyr::ungroup() |>
    # Convert character/logical to factors
    dplyr::mutate(dplyr::across(
      c(!!!active_aes) &
        (tidyselect::where(is.character) |
           tidyselect::where(is.factor) |
           tidyselect::where(is.logical)),
      labelled::to_factor # or haven::as_factor, but does not seem to maintain order always
    )) |>
    # Reverse y factors for top-to-bottom reading
    dplyr::mutate(dplyr::across(
      c(!!aes_list$y, !!aes_list$ymin, !!aes_list$ymax, !!aes_list$yend) &
        tidyselect::where(is.factor),
      forcats::fct_rev
    )) |>
    # Handle col factor reversal for flipped plots
    .reverse_if_needed(aes_list, aspect)
}

#' Reverse factor if needed
#' @noRd
.reverse_if_needed <- function(data, aes_list, aspect) {
  if (aspect == "y") {
    # Reverse col if it's not the same as y
    if (!rlang::quo_is_null(aes_list$col)) {
      col_equals_y <- identical(
        rlang::eval_tidy(aes_list$y, data),
        rlang::eval_tidy(aes_list$col, data)
      )

      if (!col_equals_y) {
        data <- data |>
          dplyr::mutate(dplyr::across(
            !!aes_list$col & tidyselect::where(is.factor),
            forcats::fct_rev
          ))
      }
    }

    # Reverse shape if it's (A) not the same as y and (B) not the same as col
    if (!rlang::quo_is_null(aes_list$shape)) {
      shape_equals_y <- identical(
        rlang::eval_tidy(aes_list$y, data),
        rlang::eval_tidy(aes_list$shape, data)
      )

      if (!rlang::quo_is_null(aes_list$col)) {
        shape_equals_col <- identical(
          rlang::eval_tidy(aes_list$col, data),
          rlang::eval_tidy(aes_list$shape, data)
        )
      } else {
        shape_equals_col <- FALSE
      }

      if (!shape_equals_y && !shape_equals_col) {
        data <- data |>
          dplyr::mutate(dplyr::across(
            !!aes_list$shape & tidyselect::where(is.factor),
            forcats::fct_rev
          ))
      }
    }

    # Reverse linetype if it's (A) not the same as y and (B) not the same as col
    if (!rlang::quo_is_null(aes_list$linetype)) {
      linetype_equals_y <- identical(
        rlang::eval_tidy(aes_list$y, data),
        rlang::eval_tidy(aes_list$linetype, data)
      )

      if (!rlang::quo_is_null(aes_list$col)) {
        linetype_equals_col <- identical(
          rlang::eval_tidy(aes_list$col, data),
          rlang::eval_tidy(aes_list$linetype, data)
        )
      } else {
        linetype_equals_col <- FALSE
      }

      if (!linetype_equals_y && !linetype_equals_col) {
        data <- data |>
          dplyr::mutate(dplyr::across(
            !!aes_list$linetype & tidyselect::where(is.factor),
            forcats::fct_rev
          ))
      }
    }
  }

  data
}

# Facet functions ----

#' Get facet layout
#' @noRd
.get_facet_layout <- function(facet_layout, aes_list) {
  if (!rlang::is_null(facet_layout)) {
    return(facet_layout)
  }

  has_facet <- !rlang::quo_is_null(aes_list$facet)
  has_facet2 <- !rlang::quo_is_null(aes_list$facet2)

  dplyr::case_when(
    has_facet && !has_facet2 ~ "wrap",
    !has_facet && has_facet2 ~ "grid",
    has_facet && has_facet2 ~ "grid",
    TRUE ~ "null"
  )
}

#' Get facet axes default
#' @noRd
.get_facet_axes <- function(facet_axes, x_limits_to_breaks) {
  facet_axes %||% if (x_limits_to_breaks) "all_y" else "all_x"
}

#' Add facet layer to plot
#' @noRd
.add_facet_layer <- function(
    plot,
    aes_list,
    data,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels,
    y_scale_subclass
) {
  # Check if we need to reverse facet
  reverse_facet <- y_scale_subclass== "discrete" &&
    identical(
      rlang::eval_tidy(aes_list$y, data),
      rlang::eval_tidy(aes_list$facet, data)
    )

  # Choose appropriate faceting function
  facet_fn <- if (reverse_facet) {
    .add_facet_layer_rev
  } else {
    .add_facet_layer_std
  }

  facet_fn(
    plot,
    aes_list,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels
  )
}

#' Add facet layer with reversed facet
#' @noRd
.add_facet_layer_rev <- function(
    plot,
    aes_list,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels
) {
  # Build facet vars with reversal
  facet_vars <- .build_facet_vars(aes_list, reverse_facet = TRUE)

  .add_facet_by_layout(
    plot,
    facet_vars,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels
  )
}

#' Add facet layer normal (not reversed)
#' @noRd
.add_facet_layer_std <- function(
    plot,
    aes_list,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels
) {
  # Build facet vars without reversal
  facet_vars <- .build_facet_vars(aes_list, reverse_facet = FALSE)

  .add_facet_by_layout(
    plot,
    facet_vars,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels
  )
}

#' Build facet variables
#' @noRd
.build_facet_vars <- function(aes_list, reverse_facet = FALSE) {
  has_facet <- !rlang::quo_is_null(aes_list$facet)
  has_facet2 <- !rlang::quo_is_null(aes_list$facet2)

  list(
    facet = if (has_facet && reverse_facet) {
      ggplot2::vars(forcats::fct_rev(!!aes_list$facet))
    } else if (has_facet) {
      ggplot2::vars(!!aes_list$facet)
    } else {
      NULL
    },
    facet2 = if (has_facet2) {
      ggplot2::vars(!!aes_list$facet2)
    } else {
      NULL
    }
  )
}

#' Add facet by layout type
#' @noRd
.add_facet_by_layout <- function(
    plot,
    facet_vars,
    facet_layout,
    facet_scales,
    facet_space,
    facet_drop,
    facet_axes,
    facet_axis_labels,
    facet_nrow,
    facet_ncol,
    facet_labels
) {
  if (facet_layout == "wrap") {
    # Combine facet vars for wrap
    all_vars <- c(facet_vars$facet, facet_vars$facet2) |>
      purrr::compact()

    if (length(all_vars) > 0) {
      plot +
        ggplot2::facet_wrap(
          facets = all_vars,
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else {
      plot
    }
  } else if (facet_layout == "grid") {
    if (
      !rlang::is_null(facet_vars$facet) || !rlang::is_null(facet_vars$facet2)
    ) {
      plot +
        ggplot2::facet_grid(
          rows = facet_vars$facet2,
          cols = facet_vars$facet,
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else {
      plot
    }
  } else {
    plot
  }
}

#' Calculate number of colours needed
#' @noRd
.get_col_n <- function(aes_list, data, plot_data, stat_name = NULL) {
  # Special handling for contour_filled and density2d_filled
  if (
    !rlang::is_null(stat_name) &&
    stat_name %in% c("contour_filled", "density2d_filled")
  ) {
    # For contour plots, check if there's a 'level' column in plot_data
    if ("level" %in% names(plot_data)) {
      level_data <- plot_data$level
      if (is.factor(level_data)) {
        return(length(levels(level_data)))
      } else {
        return(length(unique(level_data[!is.na(level_data)])))
      }
    }
    # If no level column yet, fall through to standard colour counting
    # The actual levels will be determined by ggplot2's stat_name computation
  }

  # Get factor levels if col is a factor
  col_n_factor <- if (!rlang::quo_is_null(aes_list$col)) {
    col_data <- rlang::eval_tidy(aes_list$col, data)
    if (is.factor(col_data)) length(levels(col_data)) else NA
  } else {
    NA
  }

  # Count distinct colours in plot data - only if columns exist
  colour_n <- if ("colour" %in% names(plot_data)) {
    plot_data |>
      dplyr::select(tidyselect::any_of("colour")) |>
      dplyr::filter(.data$colour != "grey50") |>
      dplyr::n_distinct()
  } else {
    0
  }

  fill_n <- if ("fill" %in% names(plot_data)) {
    plot_data |>
      dplyr::select(tidyselect::any_of("fill")) |>
      dplyr::filter(.data$fill != "grey50") |>
      dplyr::n_distinct()
  } else {
    0
  }

  # Return maximum
  max(col_n_factor, colour_n, fill_n, na.rm = TRUE)
}

# Scale determination functions ----

#' Determine scale types from plot build
#' @noRd
.get_scale_subclass<- function(plot_build, aes_list, data) {
  # Extract scale names from plot
  plot_scales <- plot_build$plot$scales$scales |>
    purrr::map_chr(\(x) {
      call_name <- rlang::call_name(x[["call"]])
      if (rlang::is_null(call_name)) "continuous" else call_name
    })

  # Determine x scale class
  x_scale_subclass<- .get_x_scale_subclass(plot_scales)

  # Check actual data type for x if needed
  if (x_scale_subclass== "continuous" && !rlang::quo_is_null(aes_list$x)) {
    x_data <- rlang::eval_tidy(aes_list$x, data)
    if (inherits(x_data, "POSIXt") || inherits(x_data, "POSIXct")) {
      x_scale_subclass<- "datetime"
    } else if (inherits(x_data, "Date")) {
      x_scale_subclass<- "date"
    } else if (inherits(x_data, "hms")) {
      x_scale_subclass<- "hms"
    }
  }

  # Determine y scale class
  y_scale_subclass<- .get_y_scale_subclass(plot_scales)

  # Check actual data type for y if needed
  if (y_scale_subclass== "continuous" && !rlang::quo_is_null(aes_list$y)) {
    y_data <- rlang::eval_tidy(aes_list$y, data)
    if (inherits(y_data, "POSIXt") || inherits(y_data, "POSIXct")) {
      y_scale_subclass<- "datetime"
    } else if (inherits(y_data, "Date")) {
      y_scale_subclass<- "date"
    } else if (inherits(y_data, "hms")) {
      y_scale_subclass<- "hms"
    }
  }

  # Determine colour scale class
  col_scale_subclass<- .get_col_scale_subclass(plot_scales, aes_list$col, data)

  list(
    x_scale_subclass= x_scale_subclass,
    y_scale_subclass= y_scale_subclass,
    col_scale_subclass= col_scale_subclass
  )
}

#' Get x scale class
#' @noRd
.get_x_scale_subclass<- function(plot_scales) {
  # Find scale_x_* and extract the third word
  x_scale <- plot_scales[stringr::str_detect(plot_scales, "^scale_x_")]
  if (length(x_scale) > 0) {
    # Extract third word (after scale_x_)
    stringr::str_extract(x_scale[1], "(?<=scale_x_)\\w+")
  } else {
    "continuous" # default
  }
}

#' Get y scale class
#' @noRd
.get_y_scale_subclass<- function(plot_scales) {
  # Find scale_y_* and extract the third word
  y_scale <- plot_scales[stringr::str_detect(plot_scales, "^scale_y_")]
  if (length(y_scale) > 0) {
    # Extract third word (after scale_y_)
    stringr::str_extract(y_scale[1], "(?<=scale_y_)\\w+")
  } else {
    "continuous" # default
  }
}

#' Get colour scale class
#' @noRd
.get_col_scale_subclass<- function(plot_scales, col_quo, data) {
  # Find scale_colour_* or scale_fill_* and extract the third word
  col_scale <- plot_scales[stringr::str_detect(
    plot_scales,
    "^scale_(colour|fill)_"
  )]

  if (length(col_scale) > 0) {
    # Extract third word (after scale_colour_ or scale_fill_)
    scale_subclass<- stringr::str_extract(
      col_scale[1],
      "(?<=scale_colour_|scale_fill_)\\w+"
    )
  } else {
    scale_subclass<- "continuous" # default
  }

  # Special case for hms - colour scales remain continuous
  if (!rlang::quo_is_null(col_quo) && scale_subclass== "continuous") {
    col_data <- rlang::eval_tidy(col_quo, data)
    if (inherits(col_data, "hms")) {
      # Keep scale_subclassas "continuous" but we'll use hms transform
      scale_subclass<- "continuous"
    }
  }

  scale_subclass
}

# Helper functions for scales ----

#' Add continuous x scale
#' @noRd
.add_x_scale_continuous <- function(
    plot,
    stat_name,
    x_breaks,
    x_breaks_n,
    x_labels,
    x_expand,
    x_limits_include,
    x_position,
    x_sec_axis,
    x_limits_to_breaks,
    x_transform,
    plot_data
) {
  # Handle sf special case
  if (stringr::str_detect(stat_name, "sf")) {
    x_breaks <- x_breaks %||% ggplot2::waiver()
    x_labels <- x_labels %||% ggplot2::waiver()
  }

  if (x_limits_to_breaks) {
    data_x <- plot_data |>
      dplyr::select(tidyselect::matches(stringr::regex("^(?!xid|xbin)x.*"))) |>
      tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "x") |>
      dplyr::filter(!is.na(.data$x))

    plot +
      .scale_x_limits_to_breaks(
        data = data_x,
        x = NULL, # Not needed since data already has 'x' column
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
      .scale_x_limits_to_breaks(
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
.add_y_scale_continuous <- function(
    plot,
    stat_name,
    y_breaks,
    y_breaks_n,
    y_labels,
    y_expand,
    y_limits_include,
    y_position,
    y_sec_axis,
    y_limits_to_breaks,
    y_transform,
    plot_data
) {
  # Handle sf special case
  if (stringr::str_detect(stat_name, "sf")) {
    y_breaks <- y_breaks %||% ggplot2::waiver()
    y_labels <- y_labels %||% ggplot2::waiver()
  }

  if (y_limits_to_breaks) {
    data_y <- plot_data |>
      dplyr::select(tidyselect::matches(stringr::regex("^(?!yid|ybin)y.*"))) |>
      tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "y") |>
      dplyr::filter(!is.na(.data$y))

    plot +
      .scale_y_limits_to_breaks(
        data = data_y,
        y = NULL, # Not needed since data already has 'y' column
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
      .scale_y_limits_to_breaks(
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

#' Get colour labels
#' @noRd
.get_col_label <- function(col_labels, col_scale_subclass, col_transform) {
  if (!rlang::is_null(col_labels)) {
    return(col_labels)
  }

  if (col_scale_subclass%in% c("discrete", "ordinal")) {
    ggplot2::waiver()
  } else if (col_transform == "hms") {
    scales::label_time()
  } else if (col_transform %in% c("date", "datetime", "time")) {
    scales::label_date_short(leading = "")
  } else {
    scales::label_comma(drop0trailing = TRUE)
  }
}

#' Add matching aesthetic guides
#' @noRd
.add_matching_aesthetic_guides <- function(
    plot,
    plot_build,
    col_legend_rev,
    col_legend_ncol,
    col_legend_nrow,
    geom_name = NULL,
    is_border_geom = FALSE,
    colour_border_transform = NULL,
    fill_border_transform = NULL,
    aes_list = NULL,
    data = NULL
) {
  # Fixed grey colour for legend key overrides
  grey_col <- "#8991A1"

  # Check if col is mapped (either directly or through colour/fill)
  col_mapped <- !rlang::quo_is_null(aes_list$col) ||
    !rlang::quo_is_null(aes_list$colour) ||
    !rlang::quo_is_null(aes_list$fill)

  # Only proceed if col is mapped
  if (!col_mapped) {
    return(plot)
  }

  # Helper function to check if two aesthetics map to the same variable
  aes_are_same <- function(aes1_quo, aes2_quo, data) {
    if (rlang::quo_is_null(aes1_quo) || rlang::quo_is_null(aes2_quo)) {
      return(FALSE)
    }

    # Try to evaluate both aesthetics
    tryCatch(
      {
        data1 <- rlang::eval_tidy(aes1_quo, data)
        data2 <- rlang::eval_tidy(aes2_quo, data)
        identical(data1, data2)
      },
      error = function(e) {
        # If we can't evaluate, compare the expressions
        identical(rlang::quo_text(aes1_quo), rlang::quo_text(aes2_quo))
      }
    )
  }

  # Process each aesthetic
  aesthetics_to_check <- list(
    shape = aes_list$shape,
    linetype = aes_list$linetype,
    linewidth = aes_list$linewidth,
    size = aes_list$size,
    alpha = aes_list$alpha
  )

  for (aes_name in names(aesthetics_to_check)) {
    aes_quo <- aesthetics_to_check[[aes_name]]

    # Skip if this aesthetic is not mapped
    if (rlang::quo_is_null(aes_quo)) {
      next
    }

    # Check if this aesthetic maps to the same variable as col/colour/fill
    same_as_col <- aes_are_same(aes_quo, aes_list$col, data) ||
      aes_are_same(aes_quo, aes_list$colour, data) ||
      aes_are_same(aes_quo, aes_list$fill, data)

    if (!same_as_col) {
      # Apply grey styling
      override_aes <- switch(
        aes_name,
        "linetype" = list(colour = grey_col),
        "shape" = {
          if (
            geom_name %in%
            c("point", "jitter", "count", "qq", "pointrange") &&
            is_border_geom
          ) {
            list(
              colour = if (
                !rlang::is_null(colour_border_transform) &&
                is.function(colour_border_transform)
              ) {
                colour_border_transform(grey_col)
              } else {
                grey_col
              },
              fill = if (
                !rlang::is_null(fill_border_transform) &&
                is.function(fill_border_transform)
              ) {
                fill_border_transform(grey_col)
              } else {
                grey_col
              }
            )
          } else {
            list(colour = grey_col)
          }
        },
        "size" = ,
        "alpha" = {
          if (
            geom_name %in%
            c("point", "jitter", "count", "qq", "pointrange") &&
            is_border_geom
          ) {
            list(
              colour = if (
                !rlang::is_null(colour_border_transform) &&
                is.function(colour_border_transform)
              ) {
                colour_border_transform(grey_col)
              } else {
                grey_col
              },
              fill = if (
                !rlang::is_null(fill_border_transform) &&
                is.function(fill_border_transform)
              ) {
                fill_border_transform(grey_col)
              } else {
                grey_col
              }
            )
          } else {
            list(colour = grey_col, fill = grey_col)
          }
        },
        "linewidth" = {
          if (
            is_border_geom &&
            !rlang::is_null(colour_border_transform) &&
            is.function(colour_border_transform)
          ) {
            list(colour = colour_border_transform(grey_col), fill = grey_col)
          } else {
            list(colour = grey_col, fill = grey_col)
          }
        }
      )

      plot <- plot +
        ggplot2::guides(
          !!aes_name := ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            override.aes = override_aes
          )
        )
    }
  }

  plot
}

# Symmetric scale functions ----

#' Create symmetric x scale
#' @noRd
.scale_x_limits_to_breaks <- function(
    data = NULL,
    x = NULL,
    symmetric = TRUE,
    breaks = NULL,
    breaks_n = 6,
    expand = NULL,
    expand_limits = NULL,
    labels = NULL,
    position = "bottom",
    sec_axis = ggplot2::waiver(),
    transform = "identity"
) {
  # Get transform
  transform <- .get_transform(transform = transform)

  # Check if symmetric is supported for this transform
  if (symmetric) {
    if (
      any(stringr::str_detect(transform, "log-")) |
      any(transform %in% c("log", "log2", "log10"))
    ) {
      rlang::abort("ggblanket does not currently support symmetric log axes")
    }
  }

  if (symmetric) {
    # If x is provided as a symbol, use it; otherwise assume data has 'x' column
    if (!rlang::is_null(x)) {
      x <- rlang::enquo(x)
      vctr <- data |>
        dplyr::pull(!!x)
    } else {
      vctr <- data$x
    }

    if (!rlang::is_null(expand_limits)) {
      vctr <- c(vctr, expand_limits)
    }

    # Convert based on transform type
    if (any(transform == "hms")) {
      vctr <- hms::as_hms(vctr)
    } else if (any(transform %in% c("time", "datetime"))) {
      vctr <- lubridate::as_datetime(vctr)
    } else if (any(transform == "date")) {
      vctr <- lubridate::as_date(vctr)
    }

    range <- range(vctr, na.rm = TRUE)

    if (any(transform == "hms")) {
      range <- hms::as_hms(range)
    }

    # Generate breaks
    if (rlang::is_null(breaks)) {
      if (any(transform == "hms")) {
        breaks <- scales::breaks_timespan()(range)
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        breaks <- scales::breaks_pretty(n = breaks_n)(range)
      } else if (
        any(stringr::str_detect(transform, "log-")) |
        any(transform %in% c("log", "log2", "log10"))
      ) {
        breaks <- scales::breaks_log(n = breaks_n)(range)
      } else {
        breaks <- scales::breaks_pretty(n = breaks_n)(range)
      }
    } else if (is.function(breaks)) {
      breaks <- breaks(range)
    }

    # Set limits to range of breaks
    limits <- range(breaks)

    if (any(transform %in% "reverse")) {
      limits <- rev(limits)
    }

    # Zero expand for symmetric
    if (rlang::is_null(expand)) {
      expand <- ggplot2::expansion(mult = c(0, 0))
    }

    # Set labels
    if (rlang::is_null(labels)) {
      if (any(transform == "hms")) {
        labels <- scales::label_time()
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        labels <- scales::label_date_short(leading = "")
      } else {
        labels <- scales::label_comma(drop0trailing = TRUE)
      }
    }

    scale <- ggplot2::scale_x_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits,
      expand = expand,
      oob = scales::oob_keep,
      transform = transform,
      position = position,
      sec.axis = sec_axis
    )
  } else {
    # Non-symmetric scale
    if (rlang::is_null(breaks)) {
      if (any(transform == "hms")) {
        breaks <- scales::breaks_timespan()
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        breaks <- scales::breaks_pretty(n = breaks_n)
      } else if (
        any(stringr::str_detect(transform, "log-")) |
        any(transform %in% c("log", "log2", "log10"))
      ) {
        breaks <- scales::breaks_log(n = breaks_n)
      } else {
        breaks <- scales::breaks_pretty(n = breaks_n)
      }
    }

    if (rlang::is_null(expand)) {
      expand <- ggplot2::expansion(mult = c(0.05, 0.05))
    }

    if (rlang::is_null(labels)) {
      if (any(transform == "hms")) {
        labels <- scales::label_time()
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        labels <- scales::label_date_short(leading = "")
      } else {
        labels <- scales::label_comma(drop0trailing = TRUE)
      }
    }

    scale <- list(
      ggplot2::scale_x_continuous(
        breaks = breaks,
        labels = labels,
        expand = expand,
        oob = scales::oob_keep,
        transform = transform,
        position = position,
        sec.axis = sec_axis
      ),
      ggplot2::expand_limits(x = expand_limits)
    )
  }

  return(scale)
}

#' Create symmetric y scale
#' @noRd
.scale_y_limits_to_breaks <- function(
    data = NULL,
    y = NULL,
    symmetric = TRUE,
    breaks = NULL,
    breaks_n = 6,
    expand = NULL,
    expand_limits = NULL,
    labels = NULL,
    position = "left",
    sec_axis = ggplot2::waiver(),
    transform = "identity"
) {
  # Get transform
  transform <- .get_transform(transform = transform)

  # Check if symmetric is supported for this transform
  if (symmetric) {
    if (
      any(stringr::str_detect(transform, "log-")) |
      any(transform %in% c("log", "log2", "log10"))
    ) {
      rlang::abort("ggblanket does not currently support symmetric log axes")
    }
  }

  if (symmetric) {
    # If y is provided as a symbol, use it; otherwise assume data has 'y' column
    if (!rlang::is_null(y)) {
      y <- rlang::enquo(y)
      vctr <- data |>
        dplyr::pull(!!y)
    } else {
      vctr <- data$y
    }

    if (!rlang::is_null(expand_limits)) {
      vctr <- c(vctr, expand_limits)
    }

    # Convert based on transform type
    if (any(transform == "hms")) {
      vctr <- hms::as_hms(vctr)
    } else if (any(transform %in% c("time", "datetime"))) {
      vctr <- lubridate::as_datetime(vctr)
    } else if (any(transform == "date")) {
      vctr <- lubridate::as_date(vctr)
    }

    range <- range(vctr, na.rm = TRUE)

    if (any(transform == "hms")) {
      range <- hms::as_hms(range)
    }

    # Generate breaks
    if (rlang::is_null(breaks)) {
      if (any(transform == "hms")) {
        breaks <- scales::breaks_timespan()(range)
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        breaks <- scales::breaks_pretty(n = breaks_n)(range)
      } else if (
        any(stringr::str_detect(transform, "log-")) |
        any(transform %in% c("log", "log2", "log10"))
      ) {
        breaks <- scales::breaks_log(n = breaks_n)(range)
      } else {
        breaks <- scales::breaks_pretty(n = breaks_n)(range)
      }
    } else if (is.function(breaks)) {
      breaks <- breaks(range)
    }

    # Set limits to range of breaks
    limits <- range(breaks)

    if (any(transform %in% "reverse")) {
      limits <- rev(limits)
    }

    # Zero expand for symmetric
    if (rlang::is_null(expand)) {
      expand <- ggplot2::expansion(mult = c(0, 0))
    }

    # Set labels
    if (rlang::is_null(labels)) {
      if (any(transform == "hms")) {
        labels <- scales::label_time()
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        labels <- scales::label_date_short(leading = "")
      } else {
        labels <- scales::label_comma(drop0trailing = TRUE)
      }
    }

    scale <- ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits,
      expand = expand,
      oob = scales::oob_keep,
      transform = transform,
      position = position,
      sec.axis = sec_axis
    )
  } else {
    # Non-symmetric scale
    if (rlang::is_null(breaks)) {
      if (any(transform == "hms")) {
        breaks <- scales::breaks_timespan()
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        breaks <- scales::breaks_pretty(n = breaks_n)
      } else if (
        any(stringr::str_detect(transform, "log-")) |
        any(transform %in% c("log", "log2", "log10"))
      ) {
        breaks <- scales::breaks_log(n = breaks_n)
      } else {
        breaks <- scales::breaks_pretty(n = breaks_n)
      }
    }

    if (rlang::is_null(expand)) {
      expand <- ggplot2::expansion(mult = c(0.05, 0.05))
    }

    if (rlang::is_null(labels)) {
      if (any(transform == "hms")) {
        labels <- scales::label_time()
      } else if (any(transform %in% c("time", "datetime", "date"))) {
        labels <- scales::label_date_short(leading = "")
      } else {
        labels <- scales::label_comma(drop0trailing = TRUE)
      }
    }

    scale <- list(
      ggplot2::scale_y_continuous(
        breaks = breaks,
        labels = labels,
        expand = expand,
        oob = scales::oob_keep,
        transform = transform,
        position = position,
        sec.axis = sec_axis
      ),
      ggplot2::expand_limits(y = expand_limits)
    )
  }

  return(scale)
}

# Theme modification functions ----

#' Get transparency defaults
#' @noRd
.get_aspect_behaviour <- function(
    axis_line_aspect,
    axis_ticks_aspect,
    panel_grid_aspect
) {
  list(
    axis_line_aspect = axis_line_aspect %||%
      getOption("ggblanket.axis_line_aspect", "keep"),
    axis_ticks_aspect = axis_ticks_aspect %||%
      getOption("ggblanket.axis_ticks_aspect", "keep"),
    panel_grid_aspect = panel_grid_aspect %||%
      getOption("ggblanket.panel_grid_aspect", "keep")
  )
}

#' Add transparency based on aspect
#' @noRd
.add_aspect <- function(
    plot,
    aspect,
    axis_line_aspect,
    axis_ticks_aspect,
    panel_grid_aspect,
    x_scale_subclass,
    y_scale_subclass
) {
  theme_updates <- list()

  if (aspect == "x") {
    if (axis_line_aspect == "transparent") {
      theme_updates$axis.line.y.left <- element_line_transparent()
      theme_updates$axis.line.y.right <- element_line_transparent()
    }
    if (axis_ticks_aspect == "transparent") {
      theme_updates$axis.ticks.y.left <- element_line_transparent()
      theme_updates$axis.ticks.y.right <- element_line_transparent()

      theme_updates$axis.minor.ticks.y.left <- element_line_transparent()
      theme_updates$axis.minor.ticks.y.right <- element_line_transparent()
    }
    if (panel_grid_aspect == "transparent") {
      theme_updates$panel.grid.major.x <- element_line_transparent()
      theme_updates$panel.grid.minor.x <- element_line_transparent()
    }

    if (axis_line_aspect == "blank") {
      theme_updates$axis.line.y.left <- ggplot2::element_blank()
      theme_updates$axis.line.y.right <- ggplot2::element_blank()
    }
    if (axis_ticks_aspect == "blank") {
      theme_updates$axis.ticks.y.left <- ggplot2::element_blank()
      theme_updates$axis.ticks.y.right <- ggplot2::element_blank()
    }
    if (panel_grid_aspect == "blank") {
      theme_updates$panel.grid.major.x <- ggplot2::element_blank()
      theme_updates$panel.grid.minor.x <- ggplot2::element_blank()
    }

    if (x_scale_subclass== "discrete") {
      theme_updates$axis.ticks.x.bottom <- element_line_transparent()
      theme_updates$axis.ticks.x.top <- element_line_transparent()

      theme_updates$axis.ticks.x.bottom <- element_line_transparent()
      theme_updates$axis.minor.ticks.x.top <- element_line_transparent()
    }
  } else if (aspect == "y") {
    if (axis_line_aspect == "transparent") {
      theme_updates$axis.line.x.bottom <- element_line_transparent()
      theme_updates$axis.line.x.top <- element_line_transparent()
    }
    if (axis_ticks_aspect == "transparent") {
      theme_updates$axis.ticks.x.bottom <- element_line_transparent()
      theme_updates$axis.ticks.x.top <- element_line_transparent()

      theme_updates$axis.ticks.x.bottom <- element_line_transparent()
      theme_updates$axis.minor.ticks.x.top <- element_line_transparent()
    }
    if (panel_grid_aspect == "transparent") {
      theme_updates$panel.grid.major.y <- element_line_transparent()
      theme_updates$panel.grid.minor.y <- element_line_transparent()
    }

    if (axis_line_aspect == "blank") {
      theme_updates$axis.line.x.bottom <- ggplot2::element_blank()
      theme_updates$axis.line.x.top <- ggplot2::element_blank()
    }
    if (axis_ticks_aspect == "blank") {
      theme_updates$axis.ticks.x.bottom <- ggplot2::element_blank()
      theme_updates$axis.ticks.x.top <- ggplot2::element_blank()
    }
    if (panel_grid_aspect == "blank") {
      theme_updates$panel.grid.major.y <- ggplot2::element_blank()
      theme_updates$panel.grid.minor.y <- ggplot2::element_blank()
    }

    if (y_scale_subclass== "discrete") {
      theme_updates$axis.ticks.y.left <- element_line_transparent()
      theme_updates$axis.ticks.y.right <- element_line_transparent()

      theme_updates$axis.minor.ticks.y.left <- element_line_transparent()
      theme_updates$axis.minor.ticks.y.right <- element_line_transparent()
    }
  }

  if (length(theme_updates) > 0) {
    plot + rlang::exec(ggplot2::theme, !!!theme_updates)
  } else {
    plot
  }
}

#' Detect if a value is an aesthetic mapping or a fixed value
#' @noRd
.is_aes_map_or_set <- function(quo_input, arg_name = "col", data = NULL) {
  if (rlang::quo_is_null(quo_input)) {
    return(list(is_aesthetic = FALSE, value = NULL))
  }

  # Get the calling environment to evaluate variables
  env <- rlang::quo_get_env(quo_input)

  # Check if it's a symbol
  if (rlang::quo_is_symbol(quo_input)) {
    symbol_name <- rlang::as_name(rlang::quo_get_expr(quo_input))

    # First check if it's a column name in the data
    if (!rlang::is_null(data) && symbol_name %in% names(data)) {
      # It's a column name, treat as aesthetic
      return(list(is_aesthetic = TRUE, value = quo_input))
    }

    # Then check if it exists as a variable in the environment
    if (exists(symbol_name, envir = env, mode = "any")) {
      tryCatch(
        {
          eval_value <- rlang::eval_tidy(quo_input)

          # Check if the evaluated value is a valid fixed value
          if (arg_name %in% c("col", "colour", "fill")) {
            if (
              is.character(eval_value) &&
              length(eval_value) == 1 &&
              !inherits(eval_value, "AsIs")
            ) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            } else if (length(eval_value) == 1 && is.na(eval_value)) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            }
          } else if (arg_name == "shape") {
            if (
              (is.numeric(eval_value) || is.character(eval_value)) &&
              length(eval_value) == 1 &&
              !inherits(eval_value, "AsIs")
            ) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            } else if (length(eval_value) == 1 && is.na(eval_value)) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            }
          } else if (arg_name == "linetype") {
            if (
              (is.numeric(eval_value) || is.character(eval_value)) &&
              length(eval_value) == 1 &&
              !inherits(eval_value, "AsIs")
            ) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            } else if (length(eval_value) == 1 && is.na(eval_value)) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            }
          } else if (arg_name == "linewidth") {
            if (
              (is.numeric(eval_value) || is.character(eval_value)) &&
              length(eval_value) == 1 &&
              !inherits(eval_value, "AsIs")
            ) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            } else if (length(eval_value) == 1 && is.na(eval_value)) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            }
          } else if (arg_name == "size") {
            if (
              (is.numeric(eval_value) || is.character(eval_value)) &&
              length(eval_value) == 1 &&
              !inherits(eval_value, "AsIs")
            ) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            } else if (length(eval_value) == 1 && is.na(eval_value)) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            }
          } else if (arg_name == "alpha") {
            if (
              (is.numeric(eval_value) || is.character(eval_value)) &&
              length(eval_value) == 1 &&
              !inherits(eval_value, "AsIs")
            ) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            } else if (length(eval_value) == 1 && is.na(eval_value)) {
              return(list(is_aesthetic = FALSE, value = eval_value))
            }
          }
        },
        error = function(e) {
          # If evaluation fails, treat as aesthetic
        }
      )
    }

    # If we get here, treat as aesthetic (column name)
    return(list(is_aesthetic = TRUE, value = quo_input))
  }

  # Check if it's a call (expression)
  if (rlang::quo_is_call(quo_input)) {
    # Try to evaluate the call first
    tryCatch(
      {
        eval_value <- rlang::eval_tidy(quo_input)

        # Check based on argument type
        if (arg_name %in% c("col", "colour", "fill")) {
          if (
            (is.character(eval_value) && length(eval_value) == 1) ||
            (length(eval_value) == 1 && is.na(eval_value))
          ) {
            return(list(is_aesthetic = FALSE, value = eval_value))
          }
        } else if (arg_name == "shape") {
          if (
            ((is.numeric(eval_value) || is.character(eval_value)) &&
             length(eval_value) == 1) ||
            (length(eval_value) == 1 && is.na(eval_value))
          ) {
            return(list(is_aesthetic = FALSE, value = eval_value))
          }
        } else if (arg_name == "linetype") {
          if (
            ((is.numeric(eval_value) || is.character(eval_value)) &&
             length(eval_value) == 1) ||
            (length(eval_value) == 1 && is.na(eval_value))
          ) {
            return(list(is_aesthetic = FALSE, value = eval_value))
          }
        } else if (arg_name == "linewidth") {
          if (
            ((is.numeric(eval_value) || is.character(eval_value)) &&
             length(eval_value) == 1) ||
            (length(eval_value) == 1 && is.na(eval_value))
          ) {
            return(list(is_aesthetic = FALSE, value = eval_value))
          }
        } else if (arg_name == "size") {
          if (
            ((is.numeric(eval_value) || is.character(eval_value)) &&
             length(eval_value) == 1) ||
            (length(eval_value) == 1 && is.na(eval_value))
          ) {
            return(list(is_aesthetic = FALSE, value = eval_value))
          }
        } else if (arg_name == "alpha") {
          if (
            ((is.numeric(eval_value) || is.character(eval_value)) &&
             length(eval_value) == 1) ||
            (length(eval_value) == 1 && is.na(eval_value))
          ) {
            return(list(is_aesthetic = FALSE, value = eval_value))
          }
        }

        # If it doesn't meet fixed value criteria, treat as aesthetic
        return(list(is_aesthetic = TRUE, value = quo_input))
      },
      error = function(e) {
        # If evaluation fails, treat as aesthetic
        return(list(is_aesthetic = TRUE, value = quo_input))
      }
    )
  }

  # For other cases (like direct strings), try to evaluate
  tryCatch(
    {
      eval_value <- rlang::eval_tidy(quo_input)

      # Check based on argument type
      if (arg_name %in% c("col", "colour", "fill")) {
        if (
          (is.character(eval_value) && length(eval_value) == 1) ||
          (length(eval_value) == 1 && is.na(eval_value))
        ) {
          return(list(is_aesthetic = FALSE, value = eval_value))
        }
      } else if (arg_name == "shape") {
        if (
          ((is.numeric(eval_value) || is.character(eval_value)) &&
           length(eval_value) == 1) ||
          (length(eval_value) == 1 && is.na(eval_value))
        ) {
          return(list(is_aesthetic = FALSE, value = eval_value))
        }
      } else if (arg_name == "linetype") {
        if (
          ((is.numeric(eval_value) || is.character(eval_value)) &&
           length(eval_value) == 1) ||
          (length(eval_value) == 1 && is.na(eval_value))
        ) {
          return(list(is_aesthetic = FALSE, value = eval_value))
        }
      } else if (arg_name == "linewidth") {
        if (
          ((is.numeric(eval_value) || is.character(eval_value)) &&
           length(eval_value) == 1) ||
          (length(eval_value) == 1 && is.na(eval_value))
        ) {
          return(list(is_aesthetic = FALSE, value = eval_value))
        }
      } else if (arg_name == "size") {
        if (
          ((is.numeric(eval_value) || is.character(eval_value)) &&
           length(eval_value) == 1) ||
          (length(eval_value) == 1 && is.na(eval_value))
        ) {
          return(list(is_aesthetic = FALSE, value = eval_value))
        }
      } else if (arg_name == "alpha") {
        if (
          ((is.numeric(eval_value) || is.character(eval_value)) &&
           length(eval_value) == 1) ||
          (length(eval_value) == 1 && is.na(eval_value))
        ) {
          return(list(is_aesthetic = FALSE, value = eval_value))
        }
      }

      # Otherwise treat as aesthetic
      return(list(is_aesthetic = TRUE, value = quo_input))
    },
    error = function(e) {
      # If evaluation fails, treat as aesthetic
      return(list(is_aesthetic = TRUE, value = quo_input))
    }
  )
}

#' Check if an aesthetic is in mapping
#' @noRd
.is_in_mapping <- function(mapping, aesthetic) {
  if (rlang::is_null(mapping)) {
    return(FALSE)
  }
  aesthetic %in% names(mapping)
}

#' Determine if geom_name should be treated as having borders
#' @noRd
.is_border <- function(geom_name, theme_defaults) {
  # Define which geom_names are treated as border polygons
  border_polygons <- c(
    "area",
    "blank",
    "bar",
    "boxplot",
    "col",
    "crossbar",
    "density",
    "map",
    "polygon",
    "rect",
    "ribbon",
    "sf",
    "smooth",
    "tile",
    "violin",
    "raster",
    "contour_filled",
    "density2d_filled",
    "bin2d",
    "hex",

    #extensions
    "star"
  )

  # Define point geom_names that can be border based on shape
  border_points <- c("point", "jitter", "count", "qq", "pointrange")

  is_border_polygon <- geom_name %in% border_polygons

  is_border_point <- geom_name %in%
    border_points &&
    !rlang::is_null(theme_defaults$geom$pointshape) &&
    theme_defaults$geom$pointshape %in% 21:25

  is_border <- is_border_polygon || is_border_point

  return(is_border)
}

#' Create base ggplot from aesthetic list
#' @noRd
.initialise_ggplot_from_list <- function(
    data,
    aes_list,
    mapping = NULL
) {
  # Build base aesthetics from the provided aes_list
  base_aes <- ggplot2::aes()

  # Add each non-NULL aesthetic
  if (!rlang::quo_is_null(aes_list$x)) {
    base_aes <- utils::modifyList(base_aes, ggplot2::aes(x = !!aes_list$x))
  }

  if (!rlang::quo_is_null(aes_list$y)) {
    base_aes <- utils::modifyList(base_aes, ggplot2::aes(y = !!aes_list$y))
  }

  if (!rlang::quo_is_null(aes_list$xmin)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(xmin = !!aes_list$xmin)
    )
  }

  if (!rlang::quo_is_null(aes_list$xmax)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(xmax = !!aes_list$xmax)
    )
  }

  if (!rlang::quo_is_null(aes_list$xend)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(xend = !!aes_list$xend)
    )
  }

  if (!rlang::quo_is_null(aes_list$ymin)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(ymin = !!aes_list$ymin)
    )
  }

  if (!rlang::quo_is_null(aes_list$ymax)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(ymax = !!aes_list$ymax)
    )
  }

  if (!rlang::quo_is_null(aes_list$yend)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(yend = !!aes_list$yend)
    )
  }

  if (!rlang::quo_is_null(aes_list$z)) {
    base_aes <- utils::modifyList(base_aes, ggplot2::aes(z = !!aes_list$z))
  }

  # IMPORTANT: Add colour and fill from aes_list (which already has col inheritance)
  if (!rlang::quo_is_null(aes_list$colour)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(colour = !!aes_list$colour)
    )
  }

  if (!rlang::quo_is_null(aes_list$fill)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(fill = !!aes_list$fill)
    )
  }

  if (!rlang::quo_is_null(aes_list$group)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(group = !!aes_list$group)
    )
  }

  if (!rlang::quo_is_null(aes_list$subgroup)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(subgroup = !!aes_list$subgroup)
    )
  }

  if (!rlang::quo_is_null(aes_list$sample)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(sample = !!aes_list$sample)
    )
  }

  if (!rlang::quo_is_null(aes_list$label)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(label = !!aes_list$label)
    )
  }

  if (!rlang::quo_is_null(aes_list$text)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(text = !!aes_list$text)
    )
  }

  if (!rlang::quo_is_null(aes_list$shape)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(shape = !!aes_list$shape)
    )
  }

  if (!rlang::quo_is_null(aes_list$linetype)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(linetype = !!aes_list$linetype)
    )
  }

  if (!rlang::quo_is_null(aes_list$linewidth)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(linewidth = !!aes_list$linewidth)
    )
  }

  if (!rlang::quo_is_null(aes_list$size)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(size = !!aes_list$size)
    )
  }

  if (!rlang::quo_is_null(aes_list$alpha)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(alpha = !!aes_list$alpha)
    )
  }

  # Merge with additional mapping
  final_aes <- if (!rlang::is_null(mapping)) {
    utils::modifyList(base_aes, mapping)
  } else {
    base_aes
  }

  # Create plot
  data |>
    ggplot2::ggplot(mapping = final_aes)
}


#' Check if colour and fill are mapped to same variable in plot
#' @noRd
.check_same_colour_fill_mapping <- function(plot) {
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

#' Apply grey styling to legend key override guides
#' @noRd
.apply_secondary_grey_guides <- function(
    plot,
    aes_list,
    data,
    geom_name,
    is_border_geom,
    colour_border_transform,
    fill_border_transform,
    col_legend_ncol,
    col_legend_nrow,
    shape_legend_rev,
    linetype_legend_rev
) {
  grey_col <- "#8991A1"

  # Helper function to check if two aesthetics map to the same variable
  aes_are_same <- function(aes1_quo, aes2_quo, data) {
    if (rlang::quo_is_null(aes1_quo) || rlang::quo_is_null(aes2_quo)) {
      return(FALSE)
    }

    tryCatch(
      {
        data1 <- rlang::eval_tidy(aes1_quo, data)
        data2 <- rlang::eval_tidy(aes2_quo, data)
        identical(data1, data2)
      },
      error = function(e) {
        identical(rlang::quo_text(aes1_quo), rlang::quo_text(aes2_quo))
      }
    )
  }

  # Process shape
  if (!rlang::quo_is_null(aes_list$shape)) {
    same_as_col <- aes_are_same(aes_list$shape, aes_list$col, data) ||
      aes_are_same(aes_list$shape, aes_list$colour, data) ||
      aes_are_same(aes_list$shape, aes_list$fill, data)

    if (!same_as_col) {
      if (
        geom_name %in%
        c("point", "jitter", "count", "qq", "pointrange") &&
        is_border_geom
      ) {
        override_aes <- list(
          colour = if (
            !rlang::is_null(colour_border_transform) &&
            is.function(colour_border_transform)
          ) {
            colour_border_transform(grey_col)
          } else {
            grey_col
          },
          fill = if (
            !rlang::is_null(fill_border_transform) &&
            is.function(fill_border_transform)
          ) {
            fill_border_transform(grey_col)
          } else {
            grey_col
          }
        )
      } else {
        override_aes <- list(colour = grey_col)
      }

      plot <- plot +
        ggplot2::guides(
          shape = ggplot2::guide_legend(
            reverse = shape_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            override.aes = override_aes
          )
        )
    }
  }

  # Process linetype
  if (!rlang::quo_is_null(aes_list$linetype)) {
    same_as_col <- aes_are_same(aes_list$linetype, aes_list$col, data) ||
      aes_are_same(aes_list$linetype, aes_list$colour, data) ||
      aes_are_same(aes_list$linetype, aes_list$fill, data)

    if (!same_as_col) {
      plot <- plot +
        ggplot2::guides(
          linetype = ggplot2::guide_legend(
            reverse = linetype_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            override.aes = list(colour = grey_col)
          )
        )
    }
  }

  # Process other aesthetics (size, linewidth, alpha) similarly...

  plot
}

# Helper function to process titles using ggplot2's label system
.process_title <- function(
    title_param,
    aes_quo,
    mapping,
    aes_name,
    stat_name,
    data,
    fallback = "X",
    plot = NULL
) {
  if (
    rlang::is_null(title_param) ||
    (stringr::str_detect(stat_name, "sf") && aes_name %in% c("x", "y"))
  ) {
    return(NULL)
  } else if (!is.function(title_param)) {
    # User provided string/expression - use as-is
    return(title_param)
  } else {
    # title_param is a function - check for label, then apply function

    # First, try to get the original variable and check for labels
    quo <- if (!rlang::quo_is_null(aes_quo)) {
      aes_quo
    } else if (!rlang::is_null(mapping)) {
      # For col_title, check colour, fill, or col in mapping
      if (aes_name == "col" && "colour" %in% names(mapping)) {
        mapping$colour
      } else if (aes_name == "col" && "fill" %in% names(mapping)) {
        mapping$fill
      } else if (aes_name %in% names(mapping)) {
        mapping[[aes_name]]
      } else {
        rlang::quo(NULL)
      }
    } else {
      rlang::quo(NULL)
    }

    # Try to get label from original data column
    if (!rlang::quo_is_null(quo)) {
      tryCatch(
        {
          # Try to pull directly from data (works for simple variable references)
          data_col <- dplyr::pull(data, !!quo)
          # label_attr <- labelled::get_label_attribute(data_col)
          label_attr <- attr(data_col, "label")
          if (!rlang::is_null(label_attr) && !identical(label_attr, "")) {
            return(label_attr)
          }
          # No label - apply function to variable name
          var_name <- rlang::as_name(quo)
          return(title_param(var_name))
        },
        error = function(e) {
          # Fall through to ggplot2 approach for computed variables
        }
      )
    }

    # Fallback: Use ggplot2's computed title
    if (!rlang::is_null(plot)) {
      tryCatch(
        {
          plot_labs <- ggplot2::get_labs(plot)

          # For col_title, try both colour and fill labels from ggplot
          if (aes_name == "col") {
            ggplot_title <- plot_labs[["colour"]] %||% plot_labs[["fill"]]
          } else {
            ggplot_title <- plot_labs[[aes_name]]
          }

          if (!rlang::is_null(ggplot_title) && ggplot_title != "") {
            return(title_param(ggplot_title))
          }
        },
        error = function(e) {
          # Continue to final fallback
        }
      )
    }

    # Final fallback
    return(title_param(fallback))
  }
}

# Complete Colour Scale Functions with border Transformation Support

# Complete Colour Scale Functions - Final Version

#' Reverse discrete palette for horizontal plots
#' @noRd
.reverse_discrete_palette <- function(palette, n = NULL, aspect = "x") {
  # Only reverse if aspect is "y"
  if (aspect != "y") {
    return(palette)
  }

  if (is.function(palette)) {
    # Capture all attributes from the original function
    original_attrs <- attributes(palette)

    # For functions, return a wrapper that reverses the output
    wrapped_fn <- function(n) {
      colours <- palette(n)
      rev(colours)
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
.check_same_colour_fill_mapping <- function(plot) {
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

#' Add colour scales wrapper
#' @noRd
.add_col_scale <- function(
    plot,
    geom_name,
    stat_name = NULL,
    col_scale_subclass,
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
    col_rescaler,
    col_scale_class,
    col_transform,
    colour_palette_d,
    colour_palette_c,
    colour_palette_o,
    colour_na,
    fill_palette_d,
    fill_palette_c,
    fill_palette_o,
    fill_na,
    aspect = "x"
) {
  # Get NA colours with defaults
  na_colour <- colour_na %||% "#CDC5BFFF"
  na_fill <- fill_na %||% "#CDC5BFFF"

  # Get transform and labels
  if (rlang::is_null(col_transform)) {
    # Special handling for colour transforms
    if (!rlang::quo_is_null(aes_list$col) && col_scale_subclass== "continuous") {
      col_data <- rlang::eval_tidy(aes_list$col, data)
      if (inherits(col_data, "hms")) {
        col_transform <- "hms"
      } else {
        col_transform <- .get_transform(NULL, col_scale_subclass)
      }
    } else {
      col_transform <- .get_transform(NULL, col_scale_subclass)
    }
  }
  col_labels <- .get_col_label(col_labels, col_scale_subclass, col_transform)

  # Apply scales based on type
  if (col_scale_subclass== "discrete") {
    plot <- .add_col_scale_discrete(
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
      is_border_geom = is_border_geom,
      aspect = aspect  # PASS ASPECT
    )
  } else if (col_scale_subclass%in% c("continuous", "date", "datetime", "time")) {
    plot <- .add_col_scale_continuous(
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
      col_rescaler,
      col_scale_class,
      col_transform,
      aes_list,
      plot_build,
      aspect = aspect  # PASS ASPECT (though not used)
    )
  } else if (col_scale_subclass== "ordinal") {
    plot <- .add_col_scale_ordinal(
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
      is_border_geom = is_border_geom,
      aspect = aspect  # PASS ASPECT (though not used)
    )
  }

  # Handle guides for other aesthetics - pass aes_list directly
  plot <- .add_matching_aesthetic_guides(
    plot,
    plot_build,
    col_legend_rev,
    col_legend_ncol,
    col_legend_nrow,
    geom_name = geom_name,
    is_border_geom = is_border_geom,
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

#' Add discrete colour scale
#' @noRd
.add_col_scale_discrete <- function(
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

  # Calculate number of colours needed
  col_n <- .get_col_n(aes_list, data, plot_data, stat_name)

  # Reverse palettes if aspect is "y"
  colour_palette <- .reverse_discrete_palette(colour_palette, n = col_n, aspect = aspect)
  fill_palette <- .reverse_discrete_palette(fill_palette, n = col_n, aspect = aspect)

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
  same_mapping <- .check_same_colour_fill_mapping(plot)

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

#' Add continuous colour scale
#' @noRd
.add_col_scale_continuous <- function(
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
    col_rescaler,
    col_scale_class,
    col_transform,
    aes_list,
    plot_build,
    aspect = "x"  # Add aspect parameter (but not used for continuous)
) {
  # For continuous scales, aspect doesn't affect legend reversal
  if (rlang::is_null(col_legend_rev)) {
    col_legend_rev <- FALSE
  }

  # Choose scale type
  if (col_scale_class == "binned") {
    # Use binned scales
    plot <- plot +
      ggplot2::scale_colour_binned(
        palette = colour_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform, rescaler = col_rescaler,
        oob = scales::oob_keep,
        na.value = na_colour
      ) +
      ggplot2::scale_fill_binned(
        palette = fill_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform, rescaler = col_rescaler,
        oob = scales::oob_keep,
        na.value = na_fill
      )

    # Check if colour and fill map to the same variable
    same_mapping <- .check_same_colour_fill_mapping(plot)

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
        transform = col_transform, rescaler = col_rescaler,
        oob = scales::oob_keep,
        na.value = na_colour
      ) +
      ggplot2::scale_fill_continuous(
        palette = fill_palette,
        breaks = col_breaks,
        n.breaks = col_breaks_n,
        labels = col_labels,
        transform = col_transform, rescaler = col_rescaler,
        oob = scales::oob_keep,
        na.value = na_fill
      )

    # Check if colour and fill map to the same variable
    same_mapping <- .check_same_colour_fill_mapping(plot)

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

#' Add ordinal colour scale
#' @noRd
.add_col_scale_ordinal <- function(
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
    # colour_border_transform = NULL,
    # fill_border_transform = NULL,
    is_border_geom = FALSE,
    aspect = "x"  # Add aspect parameter (but not used for ordinal)
) {
  # Calculate number of colours needed
  col_n <- .get_col_n(aes_list, data, plot_data, stat_name)

  # For ordinal scales, aspect doesn't affect legend reversal
  # Just handle the default and always invert
  if (rlang::is_null(col_legend_rev)) {
    col_legend_rev <- TRUE  # Default before inversion
  }

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
  same_mapping <- .check_same_colour_fill_mapping(plot)

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

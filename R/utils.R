# gg_blanket_utils.R - Utility functions for gg_blanket

# Core utility functions ----

#' Extract ggproto name helper
#' @noRd
extract_ggproto_name <- function(object, prefix) {
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
extract_transform_name <- function(transform) {
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

#' # Plot creation functions ----
#'
#' #' Create base ggplot with aesthetic mappings
#' #'
#' #' @param data A data frame or tibble.
#' #' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' #' @param mapping Additional aesthetic mappings
#' #'
#' #' @noRd
#' initialise_ggplot <- function(
#'     data,
#'     x = NULL,
#'     xmin = NULL,
#'     xmax = NULL,
#'     xend = NULL,
#'     y = NULL,
#'     ymin = NULL,
#'     ymax = NULL,
#'     yend = NULL,
#'     z = NULL,
#'     col = NULL,
#'     shape = NULL, linetype = NULL, facet = NULL,
#'     facet2 = NULL,
#'     group = NULL,
#'     subgroup = NULL,
#'     sample = NULL,
#'     label = NULL,
#'     text = NULL,
#'     linewidth = NULL,
#'     size = NULL,
#'     # alpha = NULL,
#'     mapping = NULL
#' ) {
#'   # Quote all aesthetics
#'   aes_list <- list(
#'     x = rlang::enquo(x),
#'     y = rlang::enquo(y),
#'     col = rlang::enquo(col),
#'     xmin = rlang::enquo(xmin),
#'     xmax = rlang::enquo(xmax),
#'     xend = rlang::enquo(xend),
#'     ymin = rlang::enquo(ymin),
#'     ymax = rlang::enquo(ymax),
#'     yend = rlang::enquo(yend),
#'     z = rlang::enquo(z),
#'     group = rlang::enquo(group),
#'     subgroup = rlang::enquo(subgroup),
#'     sample = rlang::enquo(sample),
#'     label = rlang::enquo(label),
#'     text = rlang::enquo(text),
#'
#'     shape = rlang::enquo(shape),
#'     linetype = rlang::enquo(linetype),
#'     linewidth = rlang::enquo(linewidth),
#'     size = rlang::enquo(size)
#'   )
#'
#'   # Build base aesthetics
#'   base_aes <- ggplot2::aes(
#'     xmin = !!aes_list$xmin,
#'     xmax = !!aes_list$xmax,
#'     xend = !!aes_list$xend,
#'     ymin = !!aes_list$ymin,
#'     ymax = !!aes_list$ymax,
#'     yend = !!aes_list$yend,
#'     z = !!aes_list$z,
#'     group = !!aes_list$group,
#'     subgroup = !!aes_list$subgroup,
#'     sample = !!aes_list$sample,
#'     label = !!aes_list$label,
#'     text = !!aes_list$text,
#'
#'     shape = !!aes_list$shape,
#'     linetype = !!aes_list$linetype,
#'     linewidth = !!aes_list$linewidth,
#'     size = !!aes_list$size
#'   )
#'
#'   # Add x and y if provided
#'   if (!rlang::quo_is_null(aes_list$x)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(x = !!aes_list$x))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$y)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(y = !!aes_list$y))
#'   }
#'
#'   # Add col/fill if provided
#'   if (!rlang::quo_is_null(aes_list$col)) {
#'     base_aes <- utils::modifyList(
#'       base_aes,
#'       ggplot2::aes(col = !!aes_list$col, fill = !!aes_list$col)
#'     )
#'   }
#'
#'   # Merge with additional mapping
#'   final_aes <- if (!is.null(mapping)) {
#'     utils::modifyList(base_aes, mapping)
#'   } else {
#'     base_aes
#'   }
#'
#'   # Create plot
#'   data |>
#'     ggplot2::ggplot(mapping = final_aes)
#' }

#' Get params based on geom type
#' @noRd
get_geom_params <- function(geom, ...) {
  # Define geom-specific parameters
  geom_params <- list(
    boxplot = list(
      median_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth),
      whisker_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth),
      staple_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth),
      outlier_gp = list(stroke = 0)
    ),
    crossbar = list(
      middle_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth)
    ),
    smooth = list(
      alpha = NA
    )
  )

  # Get specific params or empty list
  specific_params <- geom_params[[geom]] %||% list()

  # Combine with additional parameters
  rlang::list2(!!!specific_params, ...)
}

#' Add initial layer to plot
#' @noRd
add_initial_layer <- function(
    plot, geom, stat, position, params,
    show_legend, coord, blend
) {
  # Determine if using sf
  is_sf <- stringr::str_detect(stat, "sf")

  # Set default coord if not provided
  if (is.null(coord)) {
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
  if (!is.null(blend)) {
    plot + layer_call |> ggblend::blend(blend = blend) + coord
  } else {
    plot + layer_call + coord
  }
}

# Default value functions ----

#' Get transform based on scale class
#' @noRd
get_transform <- function(transform = NULL, scale_class = NULL) {

  transform %||% switch(
    scale_class,
    time = "hms",       # time scale class uses hms transform
    datetime = "time",  # datetime scales use "time" transform
    date = "date",
    "identity"  # default
  )
}

#' Get titles case function
#' @noRd
get_titles_case <- function(titles_case = NULL) {
  titles_case %||% getOption("ggblanket.titles_case", \(x) x)
}

#' Get perspective based on scale classes
#' @noRd
get_perspective <- function(perspective = NULL, x_scale_class, y_scale_class) {
  perspective %||% if (y_scale_class == "discrete" && x_scale_class != "discrete") {
    "y"
  } else {
    "x"
  }
}

#' Determine if x should be symmetric
#' @noRd
is_x_symmetric <- function(
    x_symmetric = NULL,
    stat,
    facet_scales,
    x_scale_class,
    y_scale_class
) {
  if (!is.null(x_symmetric)) {
    return(x_symmetric)
  }

  # Conditions where x should NOT be symmetric
  !(stringr::str_detect(stat, "sf") ||
      facet_scales %in% c("free", "free_x") ||
      !(y_scale_class == "discrete" && x_scale_class != "discrete"))
}

#' Determine if y should be symmetric
#' @noRd
is_y_symmetric <- function(
    y_symmetric = NULL,
    stat,
    facet_scales,
    x_scale_class,
    y_scale_class
) {
  if (!is.null(y_symmetric)) {
    return(y_symmetric)
  }

  # Conditions where y should NOT be symmetric
  !(stringr::str_detect(stat, "sf") ||
      facet_scales %in% c("free", "free_y") ||
      (y_scale_class == "discrete" && x_scale_class != "discrete"))
}

# Input validation functions ----

#' Validate inputs are valid
#' @noRd
validate_inputs <- function(mapping, x_symmetric, y_symmetric,
                            x_transform, y_transform, stat) {
  # Check mapping doesn't include facets
  if (!is.null(mapping)) {
    mapping_names <- names(unlist(mapping))
    if (any(mapping_names %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  # Check symmetric constraints
  both_symmetric <- x_symmetric && y_symmetric
  has_transform <- !is.null(x_transform) || !is.null(y_transform)
  non_identity_stat <- !identical(stat, "identity")

  if (both_symmetric && (has_transform || non_identity_stat)) {
    rlang::abort(c(
      "Both x_symmetric and y_symmetric are not supported where",
      "a positional axis is transformed or the stat is not 'identity'"
    ))
  }
}

# Data processing functions ----

#' Process data for factors and reversing
#' @noRd
process_data <- function(data, aes_list, perspective) {
  # Get non-NULL aesthetics
  active_aes <- list(
    aes_list$x, aes_list$xmin, aes_list$xmax, aes_list$xend,
    aes_list$y, aes_list$ymin, aes_list$ymax, aes_list$yend,
    aes_list$col, aes_list$facet, aes_list$facet2,
    aes_list$shape, aes_list$linetype
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
      labelled::to_factor
    )) |>
    # Reverse y factors for top-to-bottom reading
    dplyr::mutate(dplyr::across(
      c(!!aes_list$y, !!aes_list$ymin, !!aes_list$ymax, !!aes_list$yend) &
        tidyselect::where(is.factor),
      forcats::fct_rev
    )) |>
    # Handle col factor reversal for flipped plots
    reverse_if_needed(aes_list, perspective)
}

#' Reverse factor if needed
#' @noRd
reverse_if_needed <- function(data, aes_list, perspective) {
  if (perspective == "y") {
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
      }
      else {
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
get_facet_layout <- function(facet_layout, aes_list) {
  if (!is.null(facet_layout)) {
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
get_facet_axes <- function(facet_axes, x_symmetric) {
  facet_axes %||% if (x_symmetric) "all_y" else "all_x"
}

#' Add facet layer to plot
#' @noRd
add_facet_layer <- function(
    plot, aes_list, data, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes, facet_axis_labels,
    facet_nrow, facet_ncol, facet_labels, y_scale_class
) {
  # Check if we need to reverse facet
  reverse_facet <- y_scale_class == "discrete" &&
    identical(
      rlang::eval_tidy(aes_list$y, data),
      rlang::eval_tidy(aes_list$facet, data)
    )

  # Choose appropriate faceting function
  facet_fn <- if (reverse_facet) {
    add_facet_layer_rev
  } else {
    add_facet_layer_std
  }

  facet_fn(
    plot, aes_list, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes, facet_axis_labels,
    facet_nrow, facet_ncol, facet_labels
  )
}

#' Add facet layer with reversed facet
#' @noRd
add_facet_layer_rev <- function(
    plot, aes_list, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes,
    facet_axis_labels, facet_nrow, facet_ncol,
    facet_labels
) {
  # Build facet vars with reversal
  facet_vars <- build_facet_vars(aes_list, reverse_facet = TRUE)

  add_facet_by_layout(
    plot, facet_vars, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes,
    facet_axis_labels, facet_nrow, facet_ncol,
    facet_labels
  )
}

#' Add facet layer normal (not reversed)
#' @noRd
add_facet_layer_std <- function(
    plot, aes_list, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes,
    facet_axis_labels, facet_nrow, facet_ncol,
    facet_labels
) {
  # Build facet vars without reversal
  facet_vars <- build_facet_vars(aes_list, reverse_facet = FALSE)

  add_facet_by_layout(
    plot, facet_vars, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes,
    facet_axis_labels, facet_nrow, facet_ncol,
    facet_labels
  )
}

#' Build facet variables
#' @noRd
build_facet_vars <- function(aes_list, reverse_facet = FALSE) {
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
add_facet_by_layout <- function(
    plot, facet_vars, facet_layout, facet_scales,
    facet_space, facet_drop, facet_axes,
    facet_axis_labels, facet_nrow, facet_ncol,
    facet_labels
) {
  if (facet_layout == "wrap") {
    # Combine facet vars for wrap
    all_vars <- c(facet_vars$facet, facet_vars$facet2) |>
      purrr::compact()

    if (length(all_vars) > 0) {
      plot + ggplot2::facet_wrap(
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
    if (!is.null(facet_vars$facet) || !is.null(facet_vars$facet2)) {
      plot + ggplot2::facet_grid(
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


# Colour calculation functions ----

#' Calculate number of colors needed
#' @noRd
get_col_n <- function(aes_list, data, plot_data) {
  # Get factor levels if col is a factor
  col_n_factor <- if (!rlang::quo_is_null(aes_list$col)) {
    col_data <- rlang::eval_tidy(aes_list$col, data)
    if (is.factor(col_data)) length(levels(col_data)) else NA
  } else {
    NA
  }

  # Count distinct colors in plot data
  colour_n <- plot_data |>
    dplyr::select(tidyselect::any_of("colour")) |>
    dplyr::filter(.data$colour != "grey50") |>
    dplyr::n_distinct()

  fill_n <- plot_data |>
    dplyr::select(tidyselect::any_of("fill")) |>
    dplyr::filter(.data$fill != "grey50") |>
    dplyr::n_distinct()

  # Return maximum
  max(col_n_factor, colour_n, fill_n, na.rm = TRUE)
}

#' Check if aesthetic matches colour
#' @noRd
is_aes_identical_to_col <- function(plot_build, aesthetic) {
  colour_label <- plot_build$plot$labels$colour
  fill_label <- plot_build$plot$labels$fill
  aes_label <- plot_build$plot$labels[[aesthetic]]

  # Check if labels match
  (!is.null(colour_label) && !is.null(aes_label) &&
      rlang::as_name(colour_label[1]) == rlang::as_name(aes_label[1])) ||
    (!is.null(fill_label) && !is.null(aes_label) &&
       rlang::as_name(fill_label[1]) == rlang::as_name(aes_label[1]))
}

# Scale determination functions ----

#' Determine scale types from plot build
#' @noRd
get_scale_class <- function(plot_build, aes_list, data) {
  # Extract scale names from plot
  plot_scales <- plot_build$plot$scales$scales |>
    purrr::map_chr(\(x) {
      call_name <- rlang::call_name(x[["call"]])
      if (is.null(call_name)) "continuous" else call_name
    })

  # Determine x scale class
  x_scale_class <- get_x_scale_class(plot_scales)

  # Check actual data type for x if needed
  if (x_scale_class == "continuous" && !rlang::quo_is_null(aes_list$x)) {
    x_data <- rlang::eval_tidy(aes_list$x, data)
    if (inherits(x_data, "POSIXt") || inherits(x_data, "POSIXct")) {
      x_scale_class <- "datetime"
    } else if (inherits(x_data, "Date")) {
      x_scale_class <- "date"
    } else if (inherits(x_data, "hms")) {
      x_scale_class <- "hms"
    }
  }

  # Determine y scale class
  y_scale_class <- get_y_scale_class(plot_scales)

  # Check actual data type for y if needed
  if (y_scale_class == "continuous" && !rlang::quo_is_null(aes_list$y)) {
    y_data <- rlang::eval_tidy(aes_list$y, data)
    if (inherits(y_data, "POSIXt") || inherits(y_data, "POSIXct")) {
      y_scale_class <- "datetime"
    } else if (inherits(y_data, "Date")) {
      y_scale_class <- "date"
    } else if (inherits(y_data, "hms")) {
      y_scale_class <- "hms"
    }
  }

  # Determine color scale class
  col_scale_class <- get_col_scale_class(plot_scales, aes_list$col, data)

  list(
    x_scale_class = x_scale_class,
    y_scale_class = y_scale_class,
    col_scale_class = col_scale_class
  )
}

#' Get x scale class
#' @noRd
get_x_scale_class <- function(plot_scales) {
  # Find scale_x_* and extract the third word
  x_scale <- plot_scales[stringr::str_detect(plot_scales, "^scale_x_")]
  if (length(x_scale) > 0) {
    # Extract third word (after scale_x_)
    stringr::str_extract(x_scale[1], "(?<=scale_x_)\\w+")
  } else {
    "continuous"  # default
  }
}

#' Get y scale class
#' @noRd
get_y_scale_class <- function(plot_scales) {
  # Find scale_y_* and extract the third word
  y_scale <- plot_scales[stringr::str_detect(plot_scales, "^scale_y_")]
  if (length(y_scale) > 0) {
    # Extract third word (after scale_y_)
    stringr::str_extract(y_scale[1], "(?<=scale_y_)\\w+")
  } else {
    "continuous"  # default
  }
}

#' Get color scale class
#' @noRd
get_col_scale_class <- function(plot_scales, col_quo, data) {
  # Find scale_colour_* or scale_fill_* and extract the third word
  col_scale <- plot_scales[stringr::str_detect(plot_scales, "^scale_(colour|fill)_")]

  if (length(col_scale) > 0) {
    # Extract third word (after scale_colour_ or scale_fill_)
    scale_class <- stringr::str_extract(col_scale[1], "(?<=scale_colour_|scale_fill_)\\w+")
  } else {
    scale_class <- "continuous"  # default
  }

  # Special case for hms - color scales remain continuous
  if (!rlang::quo_is_null(col_quo) && scale_class == "continuous") {
    col_data <- rlang::eval_tidy(col_quo, data)
    if (inherits(col_data, "hms")) {
      # Keep scale_class as "continuous" but we'll use hms transform
      scale_class <- "continuous"
    }
  }

  scale_class
}

# Helper functions for scales ----

#' Add continuous x scale
#' @noRd
add_x_scale_continuous <- function(
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
add_y_scale_continuous <- function(
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
add_col_scale <- function(
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
  palettes <- get_col_palette(
    col_scale_class, col_border, theme_palettes,
    colour_palette, fill_palette,
    colour_palette_na, fill_palette_na
  )

  # Get transform and labels
  if (is.null(col_transform)) {
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
      plot, aes_list, data, plot_data, palettes,
      col_breaks, col_labels, col_drop, col_legend_ncol,
      col_legend_nrow, col_legend_rev, x_symmetric
    )
  } else if (col_scale_class %in% c("continuous", "date", "datetime", "time")) {
    plot <- add_col_scale_continuous(
      plot, palettes, col_border, col_breaks, col_breaks_n,
      col_labels, col_legend_rev, col_rescale, col_scale_type,
      col_transform
    )
  } else if (col_scale_class == "ordinal") {
    plot <- add_col_scale_ordinal(
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
get_col_palette <- function(
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
get_col_label <- function(col_labels, col_scale_class, col_transform) {
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
add_col_scale_discrete <- function(
    plot, aes_list, data, plot_data, palettes,
    col_breaks, col_labels, col_drop, col_legend_ncol,
    col_legend_nrow, col_legend_rev, x_symmetric
) {
  # Calculate number of colors needed
  col_n <- get_col_n(aes_list, data, plot_data)

  # Process palettes
  colour_palette_processed <- process_discrete_palette(
    palettes$colour_palette, col_n
  )

  fill_palette_processed <- process_discrete_palette(
    palettes$fill_palette, col_n
  )

  # Handle x_symmetric reversal
  if (x_symmetric) {
    col_legend_rev <- !col_legend_rev
    if (!is.null(colour_palette_processed)) {
      colour_palette_processed <- rev(colour_palette_processed)
    }
    if (!is.null(fill_palette_processed)) {
      fill_palette_processed <- rev(fill_palette_processed)
    }
  }

  # Apply colour scale
  if (!is.null(colour_palette_processed)) {
    if (is.vector(colour_palette_processed)) {
      plot <- plot +
        ggplot2::scale_colour_manual(
          values = colour_palette_processed,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_colour,
          drop = col_drop
        )
    } else if (is.function(colour_palette_processed)) {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "colour",
          palette = colour_palette_processed,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_colour,
          drop = col_drop
        )
    }
  }

  # Apply fill scale
  if (!is.null(fill_palette_processed)) {
    if (is.vector(fill_palette_processed)) {
      plot <- plot +
        ggplot2::scale_fill_manual(
          values = fill_palette_processed,
          breaks = col_breaks,
          labels = col_labels,
          na.value = palettes$na_fill,
          drop = col_drop
        )
    } else if (is.function(fill_palette_processed)) {
      plot <- plot +
        ggplot2::discrete_scale(
          aesthetics = "fill",
          palette = fill_palette_processed,
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
add_col_scale_continuous <- function(
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
add_col_scale_ordinal <- function(
    plot, aes_list, data, plot_data, palettes,
    col_breaks, col_labels, col_drop, col_legend_ncol,
    col_legend_nrow, col_legend_rev
) {
  # Calculate number of colors needed
  col_n <- get_col_n(aes_list, data, plot_data)

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
      if (is_aes_identical_to_col(plot_build, aes)) {
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



# Symmetric scale functions ----

#' Create symmetric x scale
#' @noRd
scale_x_symmetric <- function(
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
  transform <- get_transform(transform = transform)

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
    if (!is.null(x)) {
      x <- rlang::enquo(x)
      vctr <- data |>
        dplyr::pull(!!x)
    } else {
      vctr <- data$x
    }

    if (!is.null(expand_limits)) {
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
    if (is.null(breaks)) {
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
    if (is.null(expand)) {
      expand <- ggplot2::expansion(mult = c(0, 0))
    }

    # Set labels
    if (is.null(labels)) {
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
    if (is.null(breaks)) {
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

    if (is.null(expand)) {
      expand <- ggplot2::expansion(mult = c(0.05, 0.05))
    }

    if (is.null(labels)) {
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
scale_y_symmetric <- function(
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
  transform <- get_transform(transform = transform)

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
    if (!is.null(y)) {
      y <- rlang::enquo(y)
      vctr <- data |>
        dplyr::pull(!!y)
    } else {
      vctr <- data$y
    }

    if (!is.null(expand_limits)) {
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
    if (is.null(breaks)) {
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
    if (is.null(expand)) {
      expand <- ggplot2::expansion(mult = c(0, 0))
    }

    # Set labels
    if (is.null(labels)) {
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
    if (is.null(breaks)) {
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

    if (is.null(expand)) {
      expand <- ggplot2::expansion(mult = c(0.05, 0.05))
    }

    if (is.null(labels)) {
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

# Title extraction functions ----

#' Extract title with fallback
#' @noRd
get_title <- function(data, aes_quo, build_title, titles_case, default = NULL) {
  # Try to get label attribute from data
  if (!rlang::quo_is_null(aes_quo)) {
    data_col <- dplyr::pull(data, !!aes_quo)
    label_attr <- attr(data_col, "label")
    if (!is.null(label_attr)) {
      return(label_attr)
    }
  }

  # Try to get from build title
  if (!is.null(build_title)) {
    return(purrr::map_chr(rlang::as_name(build_title[1]), titles_case))
  }

  # Return default
  default
}

#' Extract titles for other aesthetics
#' @noRd
get_titles2 <- function(plot_build, col_title, titles_case) {
  aesthetics <- c("alpha", "shape", "size", "linewidth", "linetype", "pattern")

  aesthetics |>
    purrr::set_names() |>
    purrr::map(\(aes) {
      label <- plot_build$plot$labels[[aes]]
      if (is.null(label)) {
        return(NULL)
      }

      if (is_aes_identical_to_col(plot_build, aes)) {
        col_title
      } else {
        purrr::map_chr(rlang::as_name(label[1]), titles_case)
      }
    }) |>
    purrr::set_names(paste0, "_title")
}

# Theme modification functions ----

#' Get transparency defaults
#' @noRd
get_perspective_behaviour <- function(
    axis_line_transparent,
    axis_ticks_transparent,
    panel_grid_transparent
) {
  list(
    axis_line_transparent = axis_line_transparent %||%
      getOption("ggblanket.axis_line_transparent", TRUE),
    axis_ticks_transparent = axis_ticks_transparent %||%
      getOption("ggblanket.axis_ticks_transparent", TRUE),
    panel_grid_transparent = panel_grid_transparent %||%
      getOption("ggblanket.panel_grid_transparent", TRUE)
  )
}

#' Add transparency based on perspective
#' @noRd
add_perspective <- function(
    plot, perspective, axis_line_transparent,
    axis_ticks_transparent, panel_grid_transparent,
    x_scale_class, y_scale_class
) {
  theme_updates <- list()

  if (perspective == "x") {
    if (axis_line_transparent) {
      theme_updates$axis.line.y <- ggplot2::element_line(colour = "transparent")
    }
    if (axis_ticks_transparent) {
      theme_updates$axis.ticks.y <- ggplot2::element_line(colour = "transparent")
    }
    if (panel_grid_transparent) {
      theme_updates$panel.grid.major.x <- ggplot2::element_line(colour = "transparent")
      theme_updates$panel.grid.minor.x <- ggplot2::element_line(colour = "transparent")
    }
    if (x_scale_class == "discrete") {
      theme_updates$axis.ticks.x <- ggplot2::element_line(colour = "transparent")
    }
  } else if (perspective == "y") {
    if (axis_line_transparent) {
      theme_updates$axis.line.x <- ggplot2::element_line(colour = "transparent")
    }
    if (axis_ticks_transparent) {
      theme_updates$axis.ticks.x <- ggplot2::element_line(colour = "transparent")
    }
    if (panel_grid_transparent) {
      theme_updates$panel.grid.major.y <- ggplot2::element_line(colour = "transparent")
      theme_updates$panel.grid.minor.y <- ggplot2::element_line(colour = "transparent")
    }
    if (y_scale_class == "discrete") {
      theme_updates$axis.ticks.y <- ggplot2::element_line(colour = "transparent")
    }
  }

  if (length(theme_updates) > 0) {
    plot + do.call(ggplot2::theme, theme_updates)
  } else {
    plot
  }
}


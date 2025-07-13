#' Extract geom, stat, and position names
#' @noRd
get_geom_stat_position_names <- function(geom = NULL, stat = NULL, position = NULL) {
  # Extract geom name
  if (ggplot2::is_ggproto(geom)) {
    geom_name <- stringr::str_to_lower(stringr::str_remove(
      class(geom)[1],
      "Geom"
    ))
  } else if (is.character(geom)) {
    geom_name <- geom
  }

  # Extract stat name
  if (ggplot2::is_ggproto(stat)) {
    stat_name <- stringr::str_to_lower(stringr::str_remove(
      class(stat)[1],
      "Stat"
    ))
  } else if (is.character(stat)) {
    stat_name <- stat
  }

  # Extract position name
  if (ggplot2::is_ggproto(position)) {
    position_name <- stringr::str_to_lower(stringr::str_remove(
      class(position)[1],
      "Position"
    ))
  } else if (is.character(position)) {
    position_name <- position
  }

  list(
    geom_name = geom_name,
    stat_name = stat_name,
    position_name = position_name
  )
}

#' Get the base of the plot
#'
#' @param data A data frame or tibble.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' @param mapping Additional aesthetic mappings
#'
#' @noRd
create_ggplot <- function(
    data,
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
    sample = NULL,
    label = NULL,
    text = NULL,
    mapping = NULL
) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  xmin <- rlang::enquo(xmin)
  xmax <- rlang::enquo(xmax)
  xend <- rlang::enquo(xend)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  yend <- rlang::enquo(yend)
  z <- rlang::enquo(z)
  group <- rlang::enquo(group)
  subgroup <- rlang::enquo(subgroup)
  sample <- rlang::enquo(sample)
  label <- rlang::enquo(label)
  text <- rlang::enquo(text)

  # Build base aesthetics
  base_aes <- ggplot2::aes(
    # x = !!x,
    # y = !!y,
    xmin = !!xmin,
    xmax = !!xmax,
    xend = !!xend,
    ymin = !!ymin,
    ymax = !!ymax,
    yend = !!yend,
    z = !!z,
    group = !!group,
    subgroup = !!subgroup,
    sample = !!sample,
    label = !!label,
    text = !!text
  )

  if (!rlang::quo_is_null(x)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(x = !!x)
    )
  }

  if (!rlang::quo_is_null(y)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(y = !!y)
    )
  }


  # Add col/fill if provided
  if (!rlang::quo_is_null(col)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(col = !!col, fill = !!col)
    )
  }

  # Merge with additional mapping if provided
  if (!is.null(mapping)) {
    final_aes <- utils::modifyList(base_aes, mapping)
  } else {
    final_aes <- base_aes
  }

  # Create plot with merged aesthetics
  plot <- data |> ggplot2::ggplot(mapping = final_aes)

  return(plot)
}

#' Get params based on geom type
#' @noRd
get_geom_params <- function(geom_name, ...) {
  if (geom_name == "boxplot") {
    rlang::list2(
      median_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth), #take from polygon
      whisker_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth), #take from polygon
      staple_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth), #take from polygon
      outlier_gp = list(stroke = 0),
      ...
    )
  } else if (geom_name == "crossbar") {
    rlang::list2(
      middle_gp = list(linewidth = ggplot2::get_geom_defaults("line")$linewidth), #take from polygon
      ...
    )
  } else if (geom_name == "smooth") {
    rlang::list2(
      alpha = NA,
      ...
    )
  }
  else {
    rlang::list2(...)
  }
}

#' Add initial layer to plot
#' @noRd
add_initial_layer <- function(plot, geom, stat, position, params,
                              show_legend, coord, blend, stat_name) {

  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(coord)) {
      coord <- ggplot2::coord_sf(clip = "off")
    }

    if (rlang::is_null(blend)) {
      plot +
        ggplot2::layer_sf(
          geom = geom,
          stat = stat,
          position = position,
          params = params,
          show.legend = show_legend,
        ) +
        coord
    } else {
      plot +
        ggplot2::layer_sf(
          geom = geom,
          stat = stat,
          position = position,
          params = params,
          show.legend = show_legend,
        ) |>
        ggblend::blend(blend = blend) +
        coord
    }
  } else {
    if (rlang::is_null(coord)) {
      coord <- ggplot2::coord_cartesian(clip = "off")
    }

    if (rlang::is_null(blend)) {
      plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = params,
          show.legend = show_legend,
        ) +
        coord
    } else {
      plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = params,
          show.legend = show_legend,
        ) |>
        ggblend::blend(blend = blend) +
        coord
    }
  }
}

#' Determine scale types from plot build
#' @noRd
get_scale_class <- function(plot_build, aes_list, data) {
  plot_scales <- purrr::map_chr(plot_build$plot$scales$scales, function(x) {
    ifelse(
      rlang::is_null(rlang::call_name(x[["call"]])),
      NA,
      rlang::call_name(x[["call"]])
    )
  })

  # X scale type
  if (any(plot_scales %in% "scale_x_discrete")) {
    x_scale_class <- "discrete"
  } else if (any(plot_scales %in% "scale_x_date")) {
    x_scale_class <- "date"
  } else if (any(plot_scales %in% "scale_x_datetime")) {
    x_scale_class <- "datetime"
  } else if (any(plot_scales %in% "scale_x_time")) {
    x_scale_class <- "time"
  } else if (any(plot_scales %in% "scale_x_continuous")) {
    x_scale_class <- "numeric"
  } else {
    x_scale_class <- "numeric"
  }

  # Y scale type
  if (any(plot_scales %in% "scale_y_discrete")) {
    y_scale_class <- "discrete"
  } else if (any(plot_scales %in% "scale_y_date")) {
    y_scale_class <- "date"
  } else if (any(plot_scales %in% "scale_y_datetime")) {
    y_scale_class <- "datetime"
  } else if (any(plot_scales %in% "scale_y_time")) {
    y_scale_class <- "time"
  } else if (any(plot_scales %in% "scale_y_continuous")) {
    y_scale_class <- "numeric"
  } else {
    y_scale_class <- "numeric"
  }

  # Color scale class
  if (any(plot_scales %in% c("scale_colour_discrete", "scale_fill_discrete"))) {
    col_scale_class <- "discrete"
  } else if (any(plot_scales %in% c("scale_colour_ordinal", "scale_fill_ordinal"))) {
    col_scale_class <- "ordinal"
  } else if (any(plot_scales %in% c("scale_colour_date", "scale_fill_date"))) {
    col_scale_class <- "date"
  } else if (any(plot_scales %in% c("scale_colour_datetime", "scale_fill_datetime"))) {
    col_scale_class <- "datetime"
  } else if (any(plot_scales %in% c("scale_colour_time", "scale_fill_time"))) {
    col_scale_class <- "time"
  } else if (any(plot_scales %in% c("scale_colour_continuous", "scale_fill_continuous"))) {
    col_scale_class <- "numeric"
  } else {
    col_scale_class <- "numeric"
  }

  # Special case for hms
  if (!rlang::quo_is_null(aes_list$col)) {
    if (inherits(rlang::eval_tidy(aes_list$col, data), what = c("hms"))) {
      col_scale_class <- "time"
    }
  }

  list(
    x_scale_class = x_scale_class,
    y_scale_class = y_scale_class,
    col_scale_class = col_scale_class
  )
}

#' Get default transform based on scale type
#' @noRd
get_transform_default <- function(scale_class) {
  if (scale_class == "time") {
    scales::transform_hms()
  } else if (scale_class == "datetime") {
    scales::transform_time()
  } else if (scale_class == "date") {
    scales::transform_date()
  } else {
    scales::transform_identity()
  }
}

#' Get defaults for various parameters
#' @noRd
get_other_defaults <- function(x_transform, y_transform, x_scale_class, y_scale_class,
                               facet_scales, x_symmetric, y_symmetric,
                               stat_name, perspective, titles_case) {
  # Get transforms
  if (rlang::is_null(x_transform)) {
    x_transform_null <- TRUE
    x_transform <- get_transform_default(x_scale_class)
  } else {
    x_transform_null <- FALSE
  }

  if (rlang::is_null(y_transform)) {
    y_transform_null <- TRUE
    y_transform <- get_transform_default(y_scale_class)
  } else {
    y_transform_null <- FALSE
  }

  # Make drop appropriate to facet scales
  x_drop <- ifelse(facet_scales %in% c("free_x", "free"), TRUE, FALSE)
  y_drop <- ifelse(facet_scales %in% c("free_y", "free"), TRUE, FALSE)

  # Determine *_symmetric
  if (rlang::is_null(x_symmetric)) {
    if (stringr::str_detect(stat_name, "sf")) {
      x_symmetric <- FALSE
    } else if (facet_scales %in% c("free", "free_x")) {
      x_symmetric <- FALSE
    } else if (y_scale_class == "discrete" & x_scale_class != "discrete") {
      x_symmetric <- TRUE
    } else {
      x_symmetric <- FALSE
    }
  }

  if (rlang::is_null(y_symmetric)) {
    if (stringr::str_detect(stat_name, "sf")) {
      y_symmetric <- FALSE
    } else if (facet_scales %in% c("free", "free_y")) {
      y_symmetric <- FALSE
    } else if (y_scale_class == "discrete" & x_scale_class != "discrete") {
      y_symmetric <- FALSE
    } else {
      y_symmetric <- TRUE
    }
  }

  # Determine perspective
  if (rlang::is_null(perspective)) {
    if (rlang::is_null(perspective)) {
      if (y_scale_class == "discrete" & x_scale_class != "discrete") {
        perspective <- "y"
      } else {
        perspective <- "x"
      }
    }
  }

  # Get titles_case
  if (rlang::is_null(titles_case)) {
    titles_case <- getOption("ggblanket.titles_case")
    if (rlang::is_null(titles_case)) titles_case <- \(x) x
  }

  list(
    x_transform = x_transform,
    y_transform = y_transform,
    x_transform_null = x_transform_null,
    y_transform_null = y_transform_null,
    x_drop = x_drop,
    y_drop = y_drop,
    theme = theme,
    x_symmetric = x_symmetric,
    y_symmetric = y_symmetric,
    perspective = perspective,
    titles_case = titles_case
  )
}

#' Check inputs are valid
#' @noRd
check_inputs <- function(mapping, x_symmetric, y_symmetric,
                         x_transform_null, y_transform_null, stat) {
  if (!rlang::is_null(mapping)) {
    if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  if (x_symmetric & y_symmetric &
      !(x_transform_null & y_transform_null & identical(stat, "identity"))) {
    rlang::abort(
      "Both x_symmetric and y_symmetric are not supported
       where a positional axis is transformed or the stat is not 'identity'"
    )
  }
}

#' Process data for factors and reversing
#' @noRd
process_data <- function(data, aes_list, x_symmetric) {
  # Get list of aesthetics that are not NULL
  aes_to_process <- list(
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
    aes_list$facet2
  )
  # Remove NULL values
  aes_to_process <- aes_to_process[!sapply(aes_to_process, rlang::quo_is_null)]
  # Ungroup and convert to factors
  data <- data |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(
      c(!!!aes_to_process) &
        (tidyselect::where(is.character) |
           tidyselect::where(is.factor) |
           tidyselect::where(is.logical)),
      function(x) labelled::to_factor(x)
    )) |>
    # Reverse y*, so that reads top low-levels to bottom high-levels
    dplyr::mutate(dplyr::across(
      c(!!aes_list$y, !!aes_list$ymin, !!aes_list$ymax, !!aes_list$yend) &
        tidyselect::where(is.factor),
      function(x) forcats::fct_rev(x)
    ))
  # If flipped, order col correctly (only if col is not NULL)
  if (!rlang::quo_is_null(aes_list$col) && x_symmetric) {
    if (!identical(rlang::eval_tidy(aes_list$y, data),
                   rlang::eval_tidy(aes_list$col, data))) {
      data <- data |>
        dplyr::mutate(dplyr::across(
          !!aes_list$col & tidyselect::where(is.factor),
          function(x) forcats::fct_rev(x)
        ))
    }
  }
  data
}
#' Get facet layout
#' @noRd
get_facet_layout <- function(facet_layout, aes_list) {
  if (rlang::is_null(facet_layout)) {
    if (!rlang::quo_is_null(aes_list$facet) & rlang::quo_is_null(aes_list$facet2)) {
      facet_layout <- "wrap"
    } else if (rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      facet_layout <- "grid"
    } else if (!rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      facet_layout <- "grid"
    } else {
      facet_layout <- "null"
    }
  }
  facet_layout
}

#' Get facet axes default
#' @noRd
get_facet_axes <- function(facet_axes, x_symmetric) {
  if (rlang::is_null(facet_axes)) {
    if (x_symmetric) {
      facet_axes <- "all_y"
    } else {
      facet_axes <- "all_x"
    }
  }
  facet_axes
}

#' Add facet layer to plot
#' @noRd
add_facet_layer <- function(plot, aes_list, data, facet_layout, facet_scales,
                            facet_space, facet_drop, facet_axes, facet_axis_labels,
                            facet_nrow, facet_ncol, facet_labels, y_scale_class) {

  # Check if we need to reverse facet
  reverse_facet <- y_scale_class == "discrete" &
    identical(rlang::eval_tidy(aes_list$y, data),
              rlang::eval_tidy(aes_list$facet, data))

  if (reverse_facet) {
    add_facet_layer_rev(plot, aes_list, facet_layout, facet_scales,
                        facet_space, facet_drop, facet_axes, facet_axis_labels,
                        facet_nrow, facet_ncol, facet_labels)
  } else {
    add_facet_layer_std(plot, aes_list, facet_layout, facet_scales,
                        facet_space, facet_drop, facet_axes, facet_axis_labels,
                        facet_nrow, facet_ncol, facet_labels)
  }
}

#' Add facet layer with reversed facet
#' @noRd
add_facet_layer_rev <- function(plot, aes_list, facet_layout, facet_scales,
                                facet_space, facet_drop, facet_axes,
                                facet_axis_labels, facet_nrow, facet_ncol,
                                facet_labels) {
  if (facet_layout == "wrap") {
    if (!rlang::quo_is_null(aes_list$facet) & rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(forcats::fct_rev(!!aes_list$facet)),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!aes_list$facet2),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (!rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!aes_list$facet, !!aes_list$facet2),
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
    if (!rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!aes_list$facet2),
          cols = ggplot2::vars(forcats::fct_rev(!!aes_list$facet)),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (!rlang::quo_is_null(aes_list$facet) & rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_grid(
          cols = ggplot2::vars(forcats::fct_rev(!!aes_list$facet)),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!aes_list$facet2),
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

#' Add facet layer normal (not reversed)
#' @noRd
add_facet_layer_std <- function(plot, aes_list, facet_layout, facet_scales,
                                facet_space, facet_drop, facet_axes,
                                facet_axis_labels, facet_nrow, facet_ncol,
                                facet_labels) {
  if (facet_layout == "wrap") {
    if (!rlang::quo_is_null(aes_list$facet) & rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!aes_list$facet),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!aes_list$facet2),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (!rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!aes_list$facet, !!aes_list$facet2),
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
    if (!rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!aes_list$facet2),
          cols = ggplot2::vars(!!aes_list$facet),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (!rlang::quo_is_null(aes_list$facet) & rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!aes_list$facet),
          scales = facet_scales,
          space = facet_space,
          drop = facet_drop,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    } else if (rlang::quo_is_null(aes_list$facet) & !rlang::quo_is_null(aes_list$facet2)) {
      plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!aes_list$facet2),
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

#' Calculate number of colors needed
#' @noRd
calculate_colour_n <- function(aes_list, data, plot_data) {
  col_n_factor <- NA

  if (!rlang::quo_is_null(aes_list$col)) {
    if (inherits(rlang::eval_tidy(aes_list$col, data), what = c("factor"))) {
      col_n_factor <- length(levels(rlang::eval_tidy(aes_list$col, data)))
    }
  }

  colour_distinct <- plot_data |>
    dplyr::select(tidyselect::any_of("colour")) |>
    dplyr::distinct()

  if (ncol(colour_distinct) > 0) {
    colour_n <- colour_distinct |>
      dplyr::filter(.data$colour != "grey50") |>
      dplyr::count() |>
      dplyr::pull()
  } else {
    colour_n <- 1
  }

  fill_distinct <- plot_data |>
    dplyr::select(tidyselect::any_of("fill")) |>
    dplyr::distinct()

  if (ncol(fill_distinct) > 0) {
    fill_n <- fill_distinct |>
      dplyr::filter(.data$fill != "grey50") |>
      dplyr::count() |>
      dplyr::pull()
  } else {
    fill_n <- 1
  }

  max(col_n_factor, colour_n, fill_n, na.rm = TRUE)
}

#' Get transform name
#' @noRd
get_transform_name <- function(transform) {
  if (is.character(transform)) {
    transform
  } else if (inherits(transform, what = "transform")) {
    transform$name |>
      stringr::str_remove("composition") |>
      stringr::str_remove("\\(") |>
      stringr::str_remove("\\)") |>
      stringr::str_split(",") |>
      unlist()
  } else {
    "identity"
  }
}

#' Check if aesthetic matches colour
#' @noRd
check_aesthetic_matches_colour <- function(plot_build, aesthetic) {
  if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
    if (rlang::as_name(plot_build$plot$labels$colour[1]) ==
        rlang::as_name(plot_build$plot$labels[[aesthetic]][1])) {
      return(TRUE)
    }
  }

  if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
    if (rlang::as_name(plot_build$plot$labels$fill[1]) ==
        rlang::as_name(plot_build$plot$labels[[aesthetic]][1])) {
      return(TRUE)
    }
  }

  FALSE
}

#' Extract title with fallback
#' @noRd
get_title <- function(data, aes_quo, build_title, titles_case, default = NULL) {
  if (!rlang::quo_is_null(aes_quo)) {
    #get any label attribute from data
    if (!rlang::is_null(attr(dplyr::pull(data, !!aes_quo), "label"))) {
      title <- attr(dplyr::pull(data, !!aes_quo), "label")
    }
    #then get it from the plot_data and apply titles_case function
    else if (!rlang::is_null(build_title)) {
      title <- purrr::map_chr(rlang::as_name(build_title[1]), titles_case)
    }
    #otherwise
    else {
      title <- default
    }
  }
  #if NULL
  else if (!rlang::is_null(build_title)) {
    title <- purrr::map_chr(rlang::as_name(build_title[1]), titles_case)
  } else {
    title <- default
  }
  return(title)
}

#' Extract titles for other aesthetics
#' @noRd
get_titles2 <- function(plot_build, col_title, titles_case) {
  titles <- list()

  # Helper to get label for an aesthetic
  get_aes_title <- function(aes) {
    if (!rlang::is_null(plot_build$plot$labels[[aes]])) {
      if (check_aesthetic_matches_colour(plot_build, aes)) {
        col_title
      } else {
        purrr::map_chr(
          rlang::as_name(plot_build$plot$labels[[aes]][1]),
          titles_case
        )
      }
    } else {
      NULL
    }
  }

  titles$alpha_title <- get_aes_title("alpha")
  titles$shape_title <- get_aes_title("shape")
  titles$size_title <- get_aes_title("size")
  titles$linewidth_title <- get_aes_title("linewidth")
  titles$linetype_title <- get_aes_title("linetype")
  titles$pattern_title <- get_aes_title("pattern")

  titles
}

#' Get transparency defaults
#' @noRd
get_perspective_behaviour <- function(axis_line_transparent, axis_ticks_transparent,
                                      panel_grid_transparent) {
  if (rlang::is_null(axis_line_transparent)) {
    axis_line_transparent <- getOption("ggblanket.axis_line_transparent")
    if (rlang::is_null(axis_line_transparent)) axis_line_transparent <- TRUE
  }

  if (rlang::is_null(axis_ticks_transparent)) {
    axis_ticks_transparent <- getOption("ggblanket.axis_ticks_transparent")
    if (rlang::is_null(axis_ticks_transparent)) axis_ticks_transparent <- TRUE
  }

  if (rlang::is_null(panel_grid_transparent)) {
    panel_grid_transparent <- getOption("ggblanket.panel_grid_transparent")
    if (rlang::is_null(panel_grid_transparent)) panel_grid_transparent <- TRUE
  }

  list(
    axis_line_transparent = axis_line_transparent,
    axis_ticks_transparent = axis_ticks_transparent,
    panel_grid_transparent = panel_grid_transparent
  )
}

#' Add transparency
#' @noRd
add_perspective <- function(plot, perspective, axis_line_transparent,
                            axis_ticks_transparent, panel_grid_transparent,
                            x_scale_class, y_scale_class) {
  if (perspective == "x") {
    if (axis_line_transparent) {
      plot <- plot +
        ggplot2::theme(
          axis.line.y = ggplot2::element_line(colour = "transparent")
        )
    }
    if (axis_ticks_transparent) {
      plot <- plot +
        ggplot2::theme(
          axis.ticks.y = ggplot2::element_line(colour = "transparent")
        )
    }
    if (panel_grid_transparent) {
      plot <- plot +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
          panel.grid.minor.x = ggplot2::element_line(colour = "transparent")
        )
    }

    if (x_scale_class == "discrete") {
      plot <- plot +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_line(colour = "transparent")
        )
    }
  }
  else if (perspective == "y") {
    if (axis_line_transparent) {
      plot <- plot +
        ggplot2::theme(
          axis.line.x = ggplot2::element_line(colour = "transparent")
        )
    }
    if (axis_ticks_transparent) {
      plot <- plot +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_line(colour = "transparent")
        )
    }
    if (panel_grid_transparent) {
      plot <- plot +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
          panel.grid.minor.y = ggplot2::element_line(colour = "transparent")
        )
    }

    if (y_scale_class == "discrete") {
      plot <- plot +
        ggplot2::theme(
          axis.ticks.y = ggplot2::element_line(colour = "transparent")
        )
    }
  }

  plot
}

#' Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()`
#' with [geom_blank()][ggplot2::geom_blank()] defaults for the geom, stat and position.
#'
#' This function underlies all other `gg_*` functions.
#'
#' @param data A data frame or tibble.
#' @param ... Other arguments passed to within a `params` list in [ggplot2::layer()].
#' @param geom A geometric object to display the data. A snakecase character string of a ggproto Geom subclass object minus the Geom prefix (e.g. `"point"`).
#' @param stat A statistical transformation to use on the data. A snakecase character string of a ggproto Stat subclass object minus the Stat prefix (e.g. `"identity"`).
#' @param position A position adjustment. A snakecase character string of a ggproto Position subclass object minus the Position prefix (e.g. `"identity"`), or a `position_*()` function that outputs a ggproto Position subclass object (e.g. `ggplot2::position_identity()`).
#' @param coord A coordinate system. A `coord_*()` function that outputs a constructed ggproto Coord subclass object (e.g. [ggplot2::coord_cartesian()]).
#' @param blend The blending mode per [ggblend::blend()] (e.g. "multiply").
#' @param perspective The perspective of plot, which affects the theme components that are removed. Either `"x"` or `"y"`.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether to remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether to remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether to remove the relevant panel grid per the `perspective` of the plot.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,shape,linetype,facet,facet2,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' @param mapping A set of additional aesthetic mappings in [ggplot2::aes()] defaults. Intended primarily for non-supported aesthetics (e.g. `shape`, `linetype`, `linewidth`, or `size`), but can also be used for delayed evaluation etc.
#' @param x_breaks,y_breaks,col_breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param x_breaks_n,y_breaks_n,col_breaks_n A number of desired breaks.
#' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param x_limits_include,y_limits_include,col_limits_include For a continuous variable, any values that the limits should encompass (e.g. `0`). For a discrete scale, manipulate the data instead with `forcats::fct_expand`.
#' @param x_title,y_title,col_title Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)` for no title.
#' @param x_labels,y_labels,col_labels,facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels. (Note this must be named for `facet_labels`).
#' @param x_position,y_position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).If using `y_position = "top"` with a `*_*` theme, add `caption = ""` or `caption = "\n"`.
#' @param x_sec_axis,y_sec_axis A secondary axis with [ggplot2::dup_axis()] or  [ggplot2::sec_axis()] defaults.
#' @param x_symmetric,y_symmetric `TRUE` or `FALSE` of whether a symmetric scale.
#' @param x_transform,y_transform,col_transform For a continuous scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param border `TRUE` or `FALSE` of whether the geom is to be treated as if it has borders.
#' @param col_drop,facet_drop For a discrete variable, FALSE or TRUE of whether to drop unused levels.
#' @param col_legend_ncol,col_legend_nrow The number of columns and rows in a legend guide.
#' @param col_legend_rev `TRUE` or `FALSE` of whether to reverse the elements of a legend guide. Defaults to `FALSE`.
#' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' @param col_scale_type Either "gradient" or "steps". Defaults to "gradient".
#' @param colour_palette,fill_palette A character vector of hex codes (or names) or a `scales::pal_*()` function.
#' @param colour_palette_na,fill_palette_na A hex code (or name) for the `NA` values.
#' @param shape_palette A numeric vector of shape codes or a `scales::pal_*()` function. If NULL, uses the value from `getOption("ggblanket.shape_palette")`.
#' @param linetype_palette A character vector of linetype names or a `scales::pal_*()` function. If NULL, uses the value from `getOption("ggblanket.linetype_palette")`.
#' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`. Sometimes `+ *_*()` may be needed.
#' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' @param facet_ncol,facet_nrow The number of columns and rows of facet panels. Only applies to a facet layout of `"wrap"`.
#' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param facet_space When the facet scales are _not_ `"fixed"`, whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param caption Caption title string.
#' @param titles_case A function to format the title of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#'
#' @return A ggplot object.
#' @export
gg_blanket <- function(
    data = NULL,
    ...,
    geom = "blank",
    stat = "identity",
    position = "identity",
    coord = NULL,
    blend = NULL,
    perspective = NULL,
    axis_line_transparent = NULL,
    axis_ticks_transparent = NULL,
    panel_grid_transparent = NULL,
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
    colour = NULL,
    fill = NULL,
    alpha = NULL,
    shape = NULL,
    linetype = NULL,
    linewidth = NULL,
    size = NULL, stroke = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    subgroup = NULL,
    label = NULL,
    text = NULL,
    sample = NULL,
    mapping = NULL,
    border = NULL, border_colour = NULL,
    border_fill = NULL,
    border_linewidth = NULL,
    x_breaks = NULL,
    x_breaks_n = NULL,
    x_expand = NULL,
    x_limits_include = NULL,
    x_title = NULL,
    x_labels = NULL,
    x_position = "bottom",
    x_sec_axis = ggplot2::waiver(),
    x_symmetric = NULL,
    x_transform = NULL,
    y_breaks = NULL,
    y_breaks_n = NULL,
    y_expand = NULL,
    y_limits_include = NULL,
    y_title = NULL,
    y_labels = NULL,
    y_position = "left",
    y_sec_axis = ggplot2::waiver(),
    y_symmetric = NULL,
    y_transform = NULL,
    col_breaks = ggplot2::waiver(),
    col_breaks_n = NULL,
    col_drop = FALSE,
    col_limits_include = NULL,
    col_title = NULL,
    col_labels = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_palette = NULL, col_palette_na = NULL,
    col_legend_rev = FALSE,
    col_rescale = scales::rescale(),
    col_scale_type = "gradient",
    col_transform = NULL,
    colour_palette = NULL,
    colour_palette_na = NULL,
    fill_palette = NULL,
    fill_palette_na = NULL,
    shape_palette = NULL,
    linetype_palette = NULL,
    facet_axes = NULL,
    facet_axis_labels = "margins",
    facet_drop = FALSE,
    facet_labels = NULL,
    facet_layout = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    titles_case = NULL
) {

  # Step 1: Handle NULL data
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }

  # Step 2: Extract geom, stat & transform strings
  geom <- get_ggproto_name(geom, "Geom")
  stat <- get_ggproto_name(stat, "Stat")
  if (!rlang::is_null(x_transform)) x_transform <- get_transform_name(x_transform)
  if (!rlang::is_null(y_transform)) y_transform <- get_transform_name(y_transform)
  if (!rlang::is_null(col_transform)) col_transform <- get_transform_name(col_transform)

  # Step 2.5: Handle after_stat aesthetics for certain stats
  if (stat %in% c("bin2d", "binhex")) {
    default_aes <- ggplot2::aes(colour = ggplot2::after_stat(.data$count),
                                fill = ggplot2::after_stat(.data$count))
    if (is.null(mapping)) {
      mapping <- default_aes
    } else {
      has_colour <- "colour" %in% names(mapping)
      has_fill <- "fill" %in% names(mapping)

      if (!has_colour && !has_fill) {
        mapping <- utils::modifyList(mapping, default_aes)
      } else if (has_colour && !has_fill) {
        mapping$fill <- mapping$colour
      } else if (!has_colour && has_fill) {
        mapping$colour <- mapping$fill
      }
    }
  }

  if (stat %in% c("contour_filled", "density2d_filled")) {
    default_aes <- ggplot2::aes(colour = ggplot2::after_stat(.data$level),
                                fill = ggplot2::after_stat(.data$level))
    if (is.null(mapping)) {
      mapping <- default_aes
    } else {
      has_colour <- "colour" %in% names(mapping)
      has_fill <- "fill" %in% names(mapping)

      if (!has_colour && !has_fill) {
        mapping <- utils::modifyList(mapping, default_aes)
      } else if (has_colour && !has_fill) {
        mapping$fill <- mapping$colour
      } else if (!has_colour && has_fill) {
        mapping$colour <- mapping$fill
      }
    }
  }

  # Step 3: Detect aesthetic vs fixed
  col_map_or_set <- is_aes_map_or_set(rlang::enquo(col), "col", data)
  colour_map_or_set <- is_aes_map_or_set(rlang::enquo(colour), "colour", data)
  fill_map_or_set <- is_aes_map_or_set(rlang::enquo(fill), "fill", data)
  shape_map_or_set <- is_aes_map_or_set(rlang::enquo(shape), "shape", data)
  linetype_map_or_set <- is_aes_map_or_set(rlang::enquo(linetype), "linetype", data)
  linewidth_map_or_set <- is_aes_map_or_set(rlang::enquo(linewidth), "linewidth", data)
  size_map_or_set <- is_aes_map_or_set(rlang::enquo(size), "size", data)
  alpha_map_or_set <- is_aes_map_or_set(rlang::enquo(alpha), "alpha", data)

  # Step 4: Get theme defaults
  theme_defaults <- ggplot2::get_theme()

  # Step 4.5: Check if colour or fill are in mapping (needed for border detection and fixed params)
  colour_in_mapping <- is_in_mapping(mapping, "colour")
  fill_in_mapping <- is_in_mapping(mapping, "fill")

  # Step 5: Determine if this is a border geom
  if (is.null(border)) {
    # First get the default border value
    border <- is_border(geom, theme_defaults)

    # Then check if user explicitly set colour or fill to NA
    user_set_colour_na <- !colour_map_or_set$is_aesthetic &&
      !is.null(colour_map_or_set$value) &&
      length(colour_map_or_set$value) == 1 &&
      is.na(colour_map_or_set$value)

    user_set_fill_na <- !fill_map_or_set$is_aesthetic &&
      !is.null(fill_map_or_set$value) &&
      length(fill_map_or_set$value) == 1 &&
      is.na(fill_map_or_set$value)

    # Check if col or colour/fill is mapped as aesthetic
    has_col_aesthetic <- col_map_or_set$is_aesthetic
    has_colour_aesthetic <- colour_map_or_set$is_aesthetic || has_col_aesthetic
    has_fill_aesthetic <- fill_map_or_set$is_aesthetic || has_col_aesthetic

    # If user maps col/colour and sets fill=NA, or maps col/fill and sets colour=NA,
    # then border should be FALSE
    if ((has_colour_aesthetic && user_set_fill_na) ||
        (has_fill_aesthetic && user_set_colour_na)) {
      border <- FALSE
    }
  }

  # Step 6: Get border adjustment functions
  if (rlang::is_null(border_colour)) border_colour <- getOption("ggblanket.border_colour")
  if (rlang::is_null(border_fill)) border_fill <- getOption("ggblanket.border_fill")
  if (rlang::is_null(border_linewidth)) border_linewidth <- getOption("ggblanket.border_linewidth")

  # Step 7: Get palettes from theme and options
  if (rlang::is_null(col_palette)) {
    col_palette_discrete <- theme_defaults$palette.colour.discrete
    col_palette_continuous <- theme_defaults$palette.colour.continuous
    col_palette_ordinal <- getOption("ggblanket.col_palette_ordinal")
  } else {
    col_palette_discrete <- col_palette
    col_palette_continuous <- col_palette
    col_palette_ordinal <- col_palette
  }

  # Get NA color - use col_palette_na if provided, otherwise global option
  if (rlang::is_null(col_palette_na)) {
    col_palette_na <- getOption("ggblanket.col_palette_na", "grey50")
  }

  # Apply border adjustments to palettes and NA colors
  if (border) {
    if (!is.null(border_colour)) {
      # If user provided colour_palette, use it; otherwise inherit from col_palette with border adjustment
      if (!is.null(colour_palette)) {
        colour_palette_discrete <- colour_palette
        colour_palette_continuous <- colour_palette
        colour_palette_ordinal <- colour_palette
      } else {
        # Apply border_colour transformation based on palette type
        if (is.function(col_palette_discrete)) {
          colour_palette_discrete <- border_colour(col_palette_discrete)
        } else if (is.character(col_palette_discrete)) {
          # For vector palettes, apply border_colour to each color
          colour_palette_discrete <- purrr::map_chr(col_palette_discrete, border_colour)
        } else {
          colour_palette_discrete <- col_palette_discrete
        }

        if (is.function(col_palette_continuous)) {
          colour_palette_continuous <- border_colour(col_palette_continuous)
        } else if (is.character(col_palette_continuous)) {
          # For vector palettes, apply border_colour to each color
          colour_palette_continuous <- purrr::map_chr(col_palette_continuous, border_colour)
        } else {
          colour_palette_continuous <- col_palette_continuous
        }

        if (is.function(col_palette_ordinal)) {
          colour_palette_ordinal <- border_colour(col_palette_ordinal)
        } else if (is.character(col_palette_ordinal)) {
          # For vector palettes, apply border_colour to each color
          colour_palette_ordinal <- purrr::map_chr(col_palette_ordinal, border_colour)
        } else {
          colour_palette_ordinal <- col_palette_ordinal
        }
      }

      # For NA color: priority is colour_palette_na > col_palette_na (transformed)
      if (is.null(colour_palette_na)) {
        colour_palette_na <- border_colour(col_palette_na)
      }
    } else {
      # No border_colour adjustment
      colour_palette_discrete <- colour_palette %||% col_palette_discrete
      colour_palette_continuous <- colour_palette %||% col_palette_continuous
      colour_palette_ordinal <- colour_palette %||% col_palette_ordinal
      if (is.null(colour_palette_na)) {
        colour_palette_na <- col_palette_na
      }
    }

    if (!is.null(border_fill)) {
      # If user provided fill_palette, use it; otherwise inherit from col_palette with border adjustment
      if (!is.null(fill_palette)) {
        fill_palette_discrete <- fill_palette
        fill_palette_continuous <- fill_palette
        fill_palette_ordinal <- fill_palette
      } else {
        # Apply border_fill transformation based on palette type
        if (is.function(col_palette_discrete)) {
          fill_palette_discrete <- border_fill(col_palette_discrete)
        } else if (is.character(col_palette_discrete)) {
          # For vector palettes, apply border_fill to each color
          fill_palette_discrete <- purrr::map_chr(col_palette_discrete, border_fill)
        } else {
          fill_palette_discrete <- col_palette_discrete
        }

        if (is.function(col_palette_continuous)) {
          fill_palette_continuous <- border_fill(col_palette_continuous)
        } else if (is.character(col_palette_continuous)) {
          # For vector palettes, apply border_fill to each color
          fill_palette_continuous <- purrr::map_chr(col_palette_continuous, border_fill)
        } else {
          fill_palette_continuous <- col_palette_continuous
        }

        if (is.function(col_palette_ordinal)) {
          fill_palette_ordinal <- border_fill(col_palette_ordinal)
        } else if (is.character(col_palette_ordinal)) {
          # For vector palettes, apply border_fill to each color
          fill_palette_ordinal <- purrr::map_chr(col_palette_ordinal, border_fill)
        } else {
          fill_palette_ordinal <- col_palette_ordinal
        }
      }

      # For NA color: priority is fill_palette_na > col_palette_na (transformed)
      if (is.null(fill_palette_na)) {
        fill_palette_na <- border_fill(col_palette_na)
      }
    } else {
      # No border_fill adjustment
      fill_palette_discrete <- fill_palette %||% col_palette_discrete
      fill_palette_continuous <- fill_palette %||% col_palette_continuous
      fill_palette_ordinal <- fill_palette %||% col_palette_ordinal
      if (is.null(fill_palette_na)) {
        fill_palette_na <- col_palette_na
      }
    }
  } else {
    # Not a border geom - respect user-provided palettes
    colour_palette_discrete <- colour_palette %||% col_palette_discrete
    colour_palette_continuous <- colour_palette %||% col_palette_continuous
    colour_palette_ordinal <- colour_palette %||% col_palette_ordinal
    if (is.null(colour_palette_na)) {
      colour_palette_na <- col_palette_na
    }

    fill_palette_discrete <- fill_palette %||% col_palette_discrete
    fill_palette_continuous <- fill_palette %||% col_palette_continuous
    fill_palette_ordinal <- fill_palette %||% col_palette_ordinal
    if (is.null(fill_palette_na)) {
      fill_palette_na <- col_palette_na
    }
  }

  # Get shape and linetype palettes from options
  if (rlang::is_null(shape_palette)) {
    shape_palette <- getOption("ggblanket.shape_palette_discrete")
  }
  if (rlang::is_null(linetype_palette)) {
    linetype_palette <- getOption("ggblanket.linetype_palette_discrete")
  }

  # Step 9: Initialize fixed_params list
  fixed_params <- list()

  # Handle colour fixed value (priority: colour > col)
  # IMPORTANT: Only set as fixed if it's not being used as an aesthetic
  # AND not in mapping AND not explicitly set to NA
  if (!colour_map_or_set$is_aesthetic && !colour_in_mapping) {
    # colour is not mapped as aesthetic either directly or via col inheritance or mapping
    if (!is.null(colour_map_or_set$value)) {
      if (!is.na(colour_map_or_set$value)) {
        # Explicit colour value provided (and it's not NA)
        fixed_params$colour <- colour_map_or_set$value
      }
      # If it IS NA, we don't add it to fixed_params
    } else if (!col_map_or_set$is_aesthetic && !is.null(col_map_or_set$value)) {
      if (!is.na(col_map_or_set$value)) {
        # No colour value, but col is set as fixed value (and it's not NA)
        if (border && !is.null(border_colour)) {
          fixed_params$colour <- border_colour(col_map_or_set$value)
        } else {
          fixed_params$colour <- col_map_or_set$value
        }
      }
    } else {
      # Use theme default when neither colour nor col are set
      default_col <- theme_defaults$geom$colour %||% "#8991A1FF"
      if (border && !is.null(border_colour)) {
        fixed_params$colour <- border_colour(default_col)
      } else {
        fixed_params$colour <- default_col
      }
    }
  }

  # Handle fill fixed value (priority: fill > col)
  # IMPORTANT: Only set as fixed if it's not being used as an aesthetic
  # AND not in mapping AND not explicitly set to NA
  if (!fill_map_or_set$is_aesthetic && !fill_in_mapping) {
    # fill is not mapped as aesthetic either directly or via col inheritance or mapping
    if (!is.null(fill_map_or_set$value)) {
      if (!is.na(fill_map_or_set$value)) {
        # Explicit fill value provided (and it's not NA)
        fixed_params$fill <- fill_map_or_set$value
      }
      # If it IS NA, we don't add it to fixed_params
    } else if (!col_map_or_set$is_aesthetic && !is.null(col_map_or_set$value)) {
      if (!is.na(col_map_or_set$value)) {
        # No fill value, but col is set as fixed value (and it's not NA)
        if (border && !is.null(border_fill)) {
          fixed_params$fill <- border_fill(col_map_or_set$value)
        } else {
          fixed_params$fill <- col_map_or_set$value
        }
      }
    } else {
      # Use theme default when neither fill nor col are set
      default_fill <- theme_defaults$geom$fill %||% "#8991A1FF"
      if (border && !is.null(border_fill)) {
        fixed_params$fill <- border_fill(default_fill)
      } else {
        fixed_params$fill <- default_fill
      }
    }
  }

  # Fix 2: When col is mapped as an aesthetic, remove default colour/fill from fixed_params
  # UNLESS the user explicitly provided them as fixed values (and they're not NA) OR they're in mapping
  if (col_map_or_set$is_aesthetic) {
    # For colour: only remove if it wasn't explicitly set by the user (or was set to NA) and not in mapping
    if (!colour_map_or_set$is_aesthetic &&
        (is.null(colour_map_or_set$value) || is.na(colour_map_or_set$value)) &&
        !colour_in_mapping) {
      # colour was not provided by user (or was NA), so remove the default
      fixed_params$colour <- NULL
    }
    # For fill: only remove if it wasn't explicitly set by the user (or was set to NA) and not in mapping
    if (!fill_map_or_set$is_aesthetic &&
        (is.null(fill_map_or_set$value) || is.na(fill_map_or_set$value)) &&
        !fill_in_mapping) {
      # fill was not provided by user (or was NA), so remove the default
      fixed_params$fill <- NULL
    }
  }

  # Handle other fixed values with theme defaults
  if (!shape_map_or_set$is_aesthetic && !is.null(shape_map_or_set$value)) {
    fixed_params$shape <- shape_map_or_set$value
  } else if (!shape_map_or_set$is_aesthetic) {
    fixed_params$shape <- theme_defaults$geom$pointshape %||% 19
  }

  if (!linetype_map_or_set$is_aesthetic && !is.null(linetype_map_or_set$value)) {
    fixed_params$linetype <- linetype_map_or_set$value
  } else if (!linetype_map_or_set$is_aesthetic) {
    default_linetype <- theme_defaults$geom$linetype %||% 1
    if (border) {
      fixed_params$linetype <- theme_defaults$geom$linetype %||% default_linetype
    } else {
      fixed_params$linetype <- default_linetype
    }
  }

  if (!linewidth_map_or_set$is_aesthetic && !is.null(linewidth_map_or_set$value)) {
    fixed_params$linewidth <- linewidth_map_or_set$value
  } else if (!linewidth_map_or_set$is_aesthetic) {
    default_linewidth <- theme_defaults$geom$linewidth %||% 0.5
    if (border && !is.null(border_linewidth)) {
      fixed_params$linewidth <- border_linewidth(default_linewidth)
    } else if (border) {
      fixed_params$linewidth <- theme_defaults$geom$linewidth %||% default_linewidth
    } else {
      fixed_params$linewidth <- default_linewidth
    }
  }

  if (!size_map_or_set$is_aesthetic && !is.null(size_map_or_set$value)) {
    fixed_params$size <- size_map_or_set$value
  } else if (!size_map_or_set$is_aesthetic) {
    fixed_params$size <- theme_defaults$geom$size %||% 1.5
  }

  if (!alpha_map_or_set$is_aesthetic && !is.null(alpha_map_or_set$value)) {
    fixed_params$alpha <- alpha_map_or_set$value
  }

  # Handle stroke fixed value
  if (!is.null(stroke)) {
    fixed_params$stroke <- stroke
  } else if (geom %in% c("point", "jitter", "count", "qq", "pointrange")) {
    # Only set default stroke for point geoms that support it
    # Get stroke from global option, fallback to 0.5
    fixed_params$stroke <- getOption("ggblanket.stroke", 0.5)
  }

  # Sophisticated hacky fix for certain stats
  # Should not apply if the user supplies colour = "black", fill = "grey" etc
  # OR if colour/fill are in mapping
  if (stat %in% c("bin2d", "binhex", "contour_filled", "density2d_filled")) {
    if (!colour_in_mapping && is.null(colour_map_or_set$value)) {
      fixed_params$colour <- NULL
    }
    if (!fill_in_mapping && is.null(fill_map_or_set$value)) {
      fixed_params$fill <- NULL
    }
  }

  # Build aesthetic list (only aesthetics, not fixed values)
  aes_list <- list(
    x = rlang::enquo(x),
    y = rlang::enquo(y),
    xmin = rlang::enquo(xmin),
    xmax = rlang::enquo(xmax),
    xend = rlang::enquo(xend),
    ymin = rlang::enquo(ymin),
    ymax = rlang::enquo(ymax),
    yend = rlang::enquo(yend),
    z = rlang::enquo(z),
    facet = rlang::enquo(facet),
    facet2 = rlang::enquo(facet2),
    group = rlang::enquo(group),
    subgroup = rlang::enquo(subgroup),
    label = rlang::enquo(label),
    text = rlang::enquo(text),
    sample = rlang::enquo(sample),
    mapping = mapping
  )

  # Keep track of col, colour, and fill aesthetics separately
  aes_list$col <- if (col_map_or_set$is_aesthetic) rlang::enquo(col) else rlang::quo(NULL)

  # For colour: use colour if it's aesthetic, else use col if col is aesthetic, else NULL
  aes_list$colour <- if (colour_map_or_set$is_aesthetic) {
    rlang::enquo(colour)
  } else if (col_map_or_set$is_aesthetic) {
    rlang::enquo(col)  # Inherit from col
  } else {
    rlang::quo(NULL)
  }

  # For fill: use fill if it's aesthetic, else use col if col is aesthetic, else NULL
  aes_list$fill <- if (fill_map_or_set$is_aesthetic) {
    rlang::enquo(fill)
  } else if (col_map_or_set$is_aesthetic) {
    rlang::enquo(col)  # Inherit from col
  } else {
    rlang::quo(NULL)
  }

  # Other aesthetics
  aes_list$shape <- if (shape_map_or_set$is_aesthetic) rlang::enquo(shape) else rlang::quo(NULL)
  aes_list$linetype <- if (linetype_map_or_set$is_aesthetic) rlang::enquo(linetype) else rlang::quo(NULL)
  aes_list$linewidth <- if (linewidth_map_or_set$is_aesthetic) rlang::enquo(linewidth) else rlang::quo(NULL)
  aes_list$size <- if (size_map_or_set$is_aesthetic) rlang::enquo(size) else rlang::quo(NULL)
  aes_list$alpha <- if (alpha_map_or_set$is_aesthetic) rlang::enquo(alpha) else rlang::quo(NULL)

  # Step 10: Create initial plot to determine scale types
  plot <- initialise_ggplot_from_list(
    data = data,
    aes_list = aes_list,
    mapping = mapping
  )

  show_legend <- !(geom %in% c("blank", "abline"))
  params <- get_geom_params(geom, ...)

  # Merge in the fixed params
  params <- utils::modifyList(params, fixed_params)

  # Add initial layer
  plot <- add_initial_layer(plot, geom, stat, position, params,
                            show_legend, coord, blend)

  # Get plot build
  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]
    })
  })

  # Determine scale types
  scale_class <- get_scale_class(plot_build, aes_list, data)

  x_scale_class <- scale_class$x_scale_class
  y_scale_class <- scale_class$y_scale_class
  col_scale_class <- scale_class$col_scale_class

  # Check which aesthetics are mapped
  shape_mapped <- !rlang::quo_is_null(aes_list$shape)
  linetype_mapped <- !rlang::quo_is_null(aes_list$linetype)
  linewidth_mapped <- !rlang::quo_is_null(aes_list$linewidth)
  size_mapped <- !rlang::quo_is_null(aes_list$size)
  alpha_mapped <- !rlang::quo_is_null(aes_list$alpha)

  # Step 11: Get defaults
  x_drop <- facet_scales %in% c("free_x", "free")
  y_drop <- facet_scales %in% c("free_y", "free")

  x_transform <- get_transform(x_transform, scale_class = x_scale_class)
  y_transform <- get_transform(y_transform, scale_class = y_scale_class)

  x_symmetric <- is_x_symmetric(x_symmetric,
                                stat = stat,
                                facet_scales = facet_scales,
                                x_scale_class = x_scale_class,
                                y_scale_class = y_scale_class)

  y_symmetric <- is_y_symmetric(y_symmetric,
                                stat = stat,
                                facet_scales = facet_scales,
                                x_scale_class = x_scale_class,
                                y_scale_class = y_scale_class)

  titles_case <- get_titles_case(titles_case = titles_case)

  perspective <- get_perspective(perspective = perspective,
                                 x_scale_class = x_scale_class,
                                 y_scale_class = y_scale_class)

  # Step 12: Check inputs
  validate_inputs(mapping, x_symmetric, y_symmetric,
                  x_transform, y_transform, stat)

  # Step 13: Process the data
  data <- process_data(data, aes_list, perspective)

  # Step 14: Rebuild base plot with processed data
  plot <- initialise_ggplot_from_list(
    data = data,
    aes_list = aes_list,
    mapping = mapping
  )

  # Step 15: Add geom layer
  plot <- add_initial_layer(plot, geom, stat, position, params,
                            show_legend, coord, blend)

  if (!is.null(x_limits_include)) {
    plot <- plot + ggplot2::expand_limits(x = x_limits_include)
  }

  if (!is.null(y_limits_include)) {
    plot <- plot + ggplot2::expand_limits(y = y_limits_include)
  }

  # Step 16: Add facet layer
  facet_layout <- get_facet_layout(facet_layout, aes_list)
  facet_axes <- get_facet_axes(facet_axes, x_symmetric)

  plot <- add_facet_layer(plot, aes_list, data, facet_layout, facet_scales,
                          facet_space, facet_drop, facet_axes, facet_axis_labels,
                          facet_nrow, facet_ncol, facet_labels, y_scale_class)

  # Step 17: Get plot build again
  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]

      facet_nrows <- length(unique(plot_build$layout$layout$ROW))
      facet_ncols <- length(unique(plot_build$layout$layout$COL))
    })
  })

  # Step 18: Make colour scale
  if (!is.na(col_scale_class)) {
    plot <- add_col_scale(
      plot = plot,
      geom = geom,
      stat = stat,  # Add stat parameter here
      col_scale_class = col_scale_class,
      aes_list = aes_list,
      data = data,
      plot_data = plot_data,
      plot_build = plot_build,
      x_symmetric = x_symmetric,
      border = border,
      col_breaks = col_breaks,
      col_breaks_n = col_breaks_n,
      col_drop = col_drop,
      col_limits_include = col_limits_include,
      col_labels = col_labels,
      col_legend_ncol = col_legend_ncol,
      col_legend_nrow = col_legend_nrow,
      col_legend_rev = col_legend_rev,
      col_rescale = col_rescale,
      col_scale_type = col_scale_type,
      col_transform = col_transform,
      colour_palette_discrete = colour_palette_discrete,
      colour_palette_continuous = colour_palette_continuous,
      colour_palette_ordinal = colour_palette_ordinal,
      colour_palette_na = colour_palette_na,
      fill_palette_discrete = fill_palette_discrete,
      fill_palette_continuous = fill_palette_continuous,
      fill_palette_ordinal = fill_palette_ordinal,
      fill_palette_na = fill_palette_na
    )
  }

  # Step 19: Add shape scale
  if (shape_mapped && !is.null(shape_palette)) {
    # Calculate number of shapes needed
    shape_n <- if (!rlang::quo_is_null(aes_list$shape)) {
      shape_data <- rlang::eval_tidy(aes_list$shape, data)
      if (is.factor(shape_data)) {
        length(levels(shape_data))
      } else {
        length(unique(shape_data[!is.na(shape_data)]))
      }
    } else {
      NA
    }

    # Get correct number of shape values
    if (is.function(shape_palette)) {
      shape_values <- if (!is.na(shape_n)) shape_palette(shape_n) else shape_palette
    } else {
      shape_values <- if (!is.na(shape_n) && !any(rlang::have_name(shape_palette))) {
        shape_palette[1:shape_n]
      } else {
        shape_palette
      }
    }

    # Reverse the shape values and legend if required
    if (perspective == "y") {
      shape_legend_rev <- TRUE
      shape_values <- rev(shape_values)
    }
    else {
      shape_legend_rev <- FALSE
    }

    plot <- plot +
      ggplot2::scale_shape_manual(values = shape_values) +
      ggplot2::guides(shape = ggplot2::guide_legend(reverse = shape_legend_rev))
  }

  # Step 20: Add linetype scale
  if (linetype_mapped && !is.null(linetype_palette)) {
    # Calculate number of linetypes needed
    linetype_n <- if (!rlang::quo_is_null(aes_list$linetype)) {
      linetype_data <- rlang::eval_tidy(aes_list$linetype, data)
      if (is.factor(linetype_data)) {
        length(levels(linetype_data))
      } else {
        length(unique(linetype_data[!is.na(linetype_data)]))
      }
    } else {
      NA
    }

    # Get correct number of linetype values
    if (is.function(linetype_palette)) {
      linetype_values <- if (!is.na(linetype_n)) linetype_palette(linetype_n) else linetype_palette
    } else {
      linetype_values <- if (!is.na(linetype_n) && !any(rlang::have_name(linetype_palette))) {
        linetype_palette[1:linetype_n]
      } else {
        linetype_palette
      }
    }

    # Reverse the linetype values and legend if required
    if (perspective == "y") {
      linetype_legend_rev <- TRUE
      linetype_values <- rev(linetype_values)
    }
    else {
      linetype_legend_rev <- FALSE
    }

    plot <- plot +
      ggplot2::scale_linetype_manual(values = linetype_values) +
      ggplot2::guides(linetype = ggplot2::guide_legend(reverse = linetype_legend_rev))
  }

  # Step 21: Add positional scales
  # Make x scale
  if (x_scale_class == "discrete") {
    if (is.null(x_expand)) x_expand <- ggplot2::waiver()
    if (is.null(x_labels)) x_labels <- ggplot2::waiver()
    if (is.null(x_breaks)) x_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_x_discrete(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        drop = x_drop,
        position = x_position,
        sec.axis = x_sec_axis
      )
  } else {
    plot <- plot |>
      add_x_scale_continuous(
        stat = stat,
        x_breaks = x_breaks,
        x_breaks_n = x_breaks_n %||% if (facet_ncols == 1) 6 else 4,
        x_labels = x_labels,
        x_expand = x_expand,
        x_limits_include = x_limits_include,
        x_position = x_position,
        x_sec_axis = x_sec_axis,
        x_symmetric = x_symmetric,
        x_transform = x_transform,
        plot_data = plot_data
      )
  }

  # Make y scale
  if (y_scale_class == "discrete") {
    if (is.null(y_expand)) y_expand <- ggplot2::waiver()
    if (is.null(y_labels)) y_labels <- ggplot2::waiver()
    if (is.null(y_breaks)) y_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_y_discrete(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels,
        drop = y_drop,
        position = y_position,
        sec.axis = y_sec_axis
      )
  } else {
    plot <- plot |>
      add_y_scale_continuous(
        stat = stat,
        y_breaks = y_breaks,
        y_breaks_n = y_breaks_n %||% if (facet_nrows == 1) 6 else 4,
        y_labels = y_labels,
        y_expand = y_expand,
        y_limits_include = y_limits_include,
        y_position = y_position,
        y_sec_axis = y_sec_axis,
        y_symmetric = y_symmetric,
        y_transform = y_transform,
        plot_data = plot_data
      )
  }

  # Step 22: Get titles
  all_titles <- get_plot_titles(
    data = data,
    aes_list = aes_list,
    plot_build = plot_build,
    titles_case = titles_case,
    stat = stat,
    x_title = x_title,
    y_title = y_title,
    col_title = col_title
  )

  # Apply labels
  plot <- plot +
    ggplot2::labs(
      x = all_titles$x,
      y = all_titles$y,
      colour = all_titles$colour,
      fill = all_titles$fill,
      alpha = all_titles$alpha_title,
      shape = all_titles$shape_title,
      size = all_titles$size_title,
      linewidth = all_titles$linewidth_title,
      linetype = all_titles$linetype_title,
      pattern = all_titles$pattern_title,
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # Step 23: Apply theme transparency
  transparency <- get_perspective_behaviour(axis_line_transparent,
                                            axis_ticks_transparent,
                                            panel_grid_transparent)

  plot <- add_perspective(
    plot, perspective,
    transparency$axis_line_transparent,
    transparency$axis_ticks_transparent,
    transparency$panel_grid_transparent,
    x_scale_class, y_scale_class
  )

  return(plot)
}

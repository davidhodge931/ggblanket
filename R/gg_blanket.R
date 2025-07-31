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
#' @param aspect The aspect of plot, which affects the theme components that are removed. Either `"x"` or `"y"`.
#' @param aspect_axis_line `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis line for an `"x"` `aspect`, and vice versa.
#' @param aspect_axis_ticks `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis ticks for an `"x"` `aspect`, and vice versa.
#' @param aspect_panel_grid `"transparent"`, `"blank"` or `"keep"` of how to treat the x panel grid for an `"x"` `aspect`, and vice versa.
#' @param blend The blending mode per [ggblend::blend()] (e.g. "multiply").
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,colour,fill,shape,linetype,alpha,linewidth,size,facet,facet2,group,subgroup,label,text,sample A mapped (unquoted) aesthetic variable. Or a set aesthetic value.
#' @param mapping A set of additional aesthetic mappings in [ggplot2::aes()] for advanced edge-case situations (e.g.delayed evaluation etc).
#' @param bordered `TRUE` or `FALSE` of whether the `bordered_colour` and `bordered_fill` should be applied.
#' @param bordered_colour A function with input of `col` or `col_palette`. Defaults to screen/multiply based on theme.
#' @param bordered_fill A function with input of `col` or `col_palette`. Defaults to NULL.
#' @param x_breaks,y_breaks,col_breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param x_breaks_n,y_breaks_n,col_breaks_n A number of desired breaks.
#' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param x_labels,y_labels,col_labels,facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels. (Note this must be named for `facet_labels`).
#' @param x_limits_include,y_limits_include,col_limits_include For a continuous variable, any values that the limits should encompass (e.g. `0`). For a discrete scale, manipulate the data instead with `forcats::fct_expand`.
#' @param x_limits_to_breaks,y_limits_to_breaks `TRUE` or `FALSE` of whether the limits are to equal the range of the `*_breaks` (and `*_expand = c(0, 0)`). Note can only both be `TRUE` if `stat`, `x_transform` and `y_transform` equals `"identity"` (or `"reverse"`).
#' @param x_position,y_position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).If using `y_position = "top"` with a `*_*` theme, add `caption = ""` or `caption = "\n"`.
#' @param x_sec_axis,y_sec_axis A secondary axis with [ggplot2::dup_axis()] or  [ggplot2::sec_axis()] defaults.
#' @param x_title,y_title,col_title Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)` for no title.
#' @param x_transform,y_transform,col_transform For a continuous scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param col_drop,facet_drop For a discrete variable, FALSE or TRUE of whether to drop unused levels.
#' @param col_legend_ncol,col_legend_nrow The number of columns and rows in a legend guide.
#' @param col_legend_rev `TRUE` or `FALSE` of whether to reverse the elements of a legend guide. Defaults to `FALSE`.
#' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' @param col_palette,colour_palette,fill_palette colour_palette,fill_palette A character vector of hex codes (or names) or a `scales::pal_*()` function.
#' @param col_na,colour_na,fill_na A hex code (or name) for the `NA` value.
#' @param col_scale_type Either `"gradient"` or `"steps"`. Defaults to `"gradient"`.
#' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`. Sometimes `+ *_*()` may be needed.
#' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' @param facet_ncol,facet_nrow The number of columns and rows of facet panels. Only applies to a facet layout of `"wrap"`.
#' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param facet_space Whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param shape_palette A numeric vector of shape codes or a `scales::pal_*()` function. If NULL, uses the value from `getOption("ggblanket.shape_palette")`.
#' @param linetype_palette A character vector of linetype names or a `scales::pal_*()` function. If NULL, uses the value from `getOption("ggblanket.linetype_palette")`.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param caption Caption title string.
#' @param titles_case A function to format the title of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' set_blanket()
#'
#' palmerpenguins::penguins |>
#'   gg_blanket(
#'     geom = "violin",
#'     stat = "ydensity",
#'     position = "dodge",
#'     x = flipper_length_mm,
#'     y = species,
#'     col = sex,
#'   )
#'
gg_blanket <- function(
    data = NULL,
    ...,
    geom = "blank",
    stat = "identity",
    position = "identity",
    coord = NULL,
    aspect = NULL,
    aspect_axis_line = NULL,
    aspect_axis_ticks = NULL,
    aspect_panel_grid = NULL,

    blend = NULL,
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
    size = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    subgroup = NULL,
    label = NULL,
    text = NULL,
    sample = NULL,
    mapping = NULL,
    bordered = NULL,
    bordered_colour = NULL,
    bordered_fill = NULL,

    x_breaks = NULL,
    x_breaks_n = NULL,
    x_expand = NULL,
    x_limits_include = NULL,
    x_title = NULL,
    x_labels = NULL,
    x_position = "bottom",
    x_sec_axis = ggplot2::waiver(),
    x_limits_to_breaks = NULL, x_transform = NULL,
    y_breaks = NULL,
    y_breaks_n = NULL,
    y_expand = NULL,
    y_limits_include = NULL,
    y_title = NULL,
    y_labels = NULL,
    y_position = "left",
    y_sec_axis = ggplot2::waiver(),
    y_limits_to_breaks = NULL, y_transform = NULL,
    col_breaks = ggplot2::waiver(),
    col_breaks_n = NULL,
    col_drop = FALSE,
    col_limits_include = NULL,
    col_title = NULL,
    col_labels = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_palette = NULL,
    col_na = NULL,
    col_legend_rev = FALSE,
    col_rescale = scales::rescale(),
    col_scale_type = "gradient",
    col_transform = NULL,
    colour_palette = NULL,
    colour_na = NULL,
    fill_palette = NULL,
    fill_na = NULL,
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
  if (!rlang::is_null(x_transform)) {
    x_transform <- get_transform_name(x_transform)
  }
  if (!rlang::is_null(y_transform)) {
    y_transform <- get_transform_name(y_transform)
  }
  if (!rlang::is_null(col_transform)) {
    col_transform <- get_transform_name(col_transform)
  }

  # Step 3: Detect aesthetic vs fixed
  col_map_or_set <- is_aes_map_or_set(rlang::enquo(col), "col", data)
  colour_map_or_set <- is_aes_map_or_set(rlang::enquo(colour), "colour", data)
  fill_map_or_set <- is_aes_map_or_set(rlang::enquo(fill), "fill", data)
  shape_map_or_set <- is_aes_map_or_set(rlang::enquo(shape), "shape", data)
  linetype_map_or_set <- is_aes_map_or_set(
    rlang::enquo(linetype),
    "linetype",
    data
  )
  linewidth_map_or_set <- is_aes_map_or_set(
    rlang::enquo(linewidth),
    "linewidth",
    data
  )
  size_map_or_set <- is_aes_map_or_set(rlang::enquo(size), "size", data)
  alpha_map_or_set <- is_aes_map_or_set(rlang::enquo(alpha), "alpha", data)

  # Step 2.5: Handle after_stat aesthetics for certain stats
  if (stat %in% c("bin2d", "binhex")) {
    # Check if user explicitly set colour as fixed
    user_set_colour_fixed <- !colour_map_or_set$is_aesthetic &&
      !is.null(colour_map_or_set$value)

    if (user_set_colour_fixed) {
      # User set colour, only apply after_stat to fill
      default_aes <- ggplot2::aes(
        fill = ggplot2::after_stat(.data$count)
      )
    } else {
      # Apply after_stat to both as before
      default_aes <- ggplot2::aes(
        colour = ggplot2::after_stat(.data$count),
        fill = ggplot2::after_stat(.data$count)
      )
    }

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
    # Check if user explicitly set colour as fixed
    user_set_colour_fixed <- !colour_map_or_set$is_aesthetic &&
      !is.null(colour_map_or_set$value)

    if (user_set_colour_fixed) {
      # User set colour, only apply after_stat to fill
      default_aes <- ggplot2::aes(
        fill = ggplot2::after_stat(.data$level)
      )
    } else {
      # Apply after_stat to both as before
      default_aes <- ggplot2::aes(
        colour = ggplot2::after_stat(.data$level),
        fill = ggplot2::after_stat(.data$level)
      )
    }

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

  # Step 4: Get theme defaults
  theme_defaults <- ggplot2::get_theme()

  # Step 4.5: Check if colour or fill are in mapping (needed for border detection and fixed params)
  colour_in_mapping <- is_in_mapping(mapping, "colour")
  fill_in_mapping <- is_in_mapping(mapping, "fill")

  # Step 5: Determine if this is a border geom
  if (rlang::is_null(bordered)) {
    # Auto-detect if it's a border geom type
    is_bordered_geom <- is_bordered(geom, theme_defaults)
  } else {
    # User explicitly set bordered = TRUE or FALSE
    is_bordered_geom <- bordered
  }

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
  # then treat as non-border behavior
  if (
    (has_colour_aesthetic && user_set_fill_na) ||
    (has_fill_aesthetic && user_set_colour_na)
  ) {
    is_bordered_geom <- FALSE
  }

  # Step 6: Get border adjustment functions/values
  if (rlang::is_null(bordered_colour)) {
    bordered_colour <- if (is_bordered_geom) {
      getOption("ggblanket.bordered_colour")
    } else {
      NULL
    }
  } else if (is.na(bordered_colour)) {
    # User explicitly disabled border colour adjustment
    bordered_colour <- NULL
  } else {
    # User provided a function or value
    bordered_colour <- bordered_colour
  }

  if (rlang::is_null(bordered_fill)) {
    bordered_fill <- if (is_bordered_geom) {
      getOption("ggblanket.bordered_fill")
    } else {
      NULL
    }
  } else if (is.na(bordered_fill)) {
    # User explicitly disabled border fill adjustment
    bordered_fill <- NULL
  } else {
    # User provided a function or value
    bordered_fill <- bordered_fill
  }
  ##############
  # Step 7: Get palettes from theme and options - FIXED VERSION
  if (rlang::is_null(col_palette)) {
    col_palette_d <- theme_defaults$palette.colour.discrete
    col_palette_c <- theme_defaults$palette.colour.continuous
    col_palette_o <- getOption("ggblanket.col_palette_o")

    # Add fallback defaults if theme palettes are NULL
    if (is.null(col_palette_d)) {
      col_palette_d <- scales::hue_pal()  # or your preferred default
    }
    if (is.null(col_palette_c)) {
      col_palette_c <- scales::viridis_pal()  # or your preferred default
    }
    if (is.null(col_palette_o)) {
      col_palette_o <- scales::viridis_pal()  # or your preferred default
    }
  } else {
    col_palette_d <- col_palette
    col_palette_c <- col_palette
    col_palette_o <- col_palette
  }

  # Get NA color - use col_na if provided, otherwise global option
  if (rlang::is_null(col_na)) {
    col_na <- getOption("ggblanket.col_na", "grey50")
  }

  # Apply border adjustments to palettes and NA colors - FIXED VERSION
  if (is_bordered_geom) {
    if (!is.null(bordered_colour)) {
      # ... existing border logic but add NULL checks
      if (!is.null(colour_palette)) {
        colour_palette_d <- colour_palette
        colour_palette_c <- colour_palette
        colour_palette_o <- colour_palette
      } else {
        # Apply bordered_colour transformation with NULL safety
        if (is.function(col_palette_d) && is.function(bordered_colour)) {
          colour_palette_d <- bordered_colour(col_palette_d)
        } else if (is.character(col_palette_d) && is.function(bordered_colour) && length(col_palette_d) > 0) {
          colour_palette_d <- purrr::map_chr(col_palette_d, bordered_colour)
        } else {
          colour_palette_d <- col_palette_d %||% scales::hue_pal()  # Add fallback
        }

        if (is.function(col_palette_c) && is.function(bordered_colour)) {
          colour_palette_c <- bordered_colour(col_palette_c)
        } else if (is.character(col_palette_c) && is.function(bordered_colour) && length(col_palette_c) > 0) {
          colour_palette_c <- purrr::map_chr(col_palette_c, bordered_colour)
        } else {
          colour_palette_c <- col_palette_c %||% scales::viridis_pal()  # Add fallback
        }

        if (is.function(col_palette_o) && is.function(bordered_colour)) {
          colour_palette_o <- bordered_colour(col_palette_o)
        } else if (is.character(col_palette_o) && is.function(bordered_colour) && length(col_palette_o) > 0) {
          colour_palette_o <- purrr::map_chr(col_palette_o, bordered_colour)
        } else {
          colour_palette_o <- col_palette_o %||% scales::viridis_pal()  # Add fallback
        }
      }

      # For NA color: priority is colour_na > col_na (transformed)
      if (is.null(colour_na) && is.function(bordered_colour)) {
        colour_na <- bordered_colour(col_na)
      }
    } else {
      # No bordered_colour adjustment - add NULL safety
      colour_palette_d <- colour_palette %||% col_palette_d %||% scales::hue_pal()
      colour_palette_c <- colour_palette %||% col_palette_c %||% scales::viridis_pal()
      colour_palette_o <- colour_palette %||% col_palette_o %||% scales::viridis_pal()
      if (is.null(colour_na)) {
        colour_na <- col_na
      }
    }

    # Similar fixes for fill palettes...
    if (!is.null(bordered_fill)) {
      # Add similar NULL safety checks for fill palettes
      if (!is.null(fill_palette)) {
        fill_palette_d <- fill_palette
        fill_palette_c <- fill_palette
        fill_palette_o <- fill_palette
      } else {
        # Apply bordered_fill transformation with NULL safety
        if (is.function(col_palette_d) && is.function(bordered_fill)) {
          fill_palette_d <- bordered_fill(col_palette_d)
        } else if (is.character(col_palette_d) && is.function(bordered_fill) && length(col_palette_d) > 0) {
          fill_palette_d <- purrr::map_chr(col_palette_d, bordered_fill)
        } else {
          fill_palette_d <- col_palette_d %||% scales::hue_pal()
        }

        if (is.function(col_palette_c) && is.function(bordered_fill)) {
          fill_palette_c <- bordered_fill(col_palette_c)
        } else if (is.character(col_palette_c) && is.function(bordered_fill) && length(col_palette_c) > 0) {
          fill_palette_c <- purrr::map_chr(col_palette_c, bordered_fill)
        } else {
          fill_palette_c <- col_palette_c %||% scales::viridis_pal()
        }

        if (is.function(col_palette_o) && is.function(bordered_fill)) {
          fill_palette_o <- bordered_fill(col_palette_o)
        } else if (is.character(col_palette_o) && is.function(bordered_fill) && length(col_palette_o) > 0) {
          fill_palette_o <- purrr::map_chr(col_palette_o, bordered_fill)
        } else {
          fill_palette_o <- col_palette_o %||% scales::viridis_pal()
        }
      }

      # For NA color: priority is fill_na > col_na (transformed)
      if (is.null(fill_na) && is.function(bordered_fill)) {
        fill_na <- bordered_fill(col_na)
      }
    } else {
      # No bordered_fill adjustment - add NULL safety
      fill_palette_d <- fill_palette %||% col_palette_d %||% scales::hue_pal()
      fill_palette_c <- fill_palette %||% col_palette_c %||% scales::viridis_pal()
      fill_palette_o <- fill_palette %||% col_palette_o %||% scales::viridis_pal()
      if (is.null(fill_na)) {
        fill_na <- col_na
      }
    }
  } else {
    # Not a border geom - respect user-provided palettes with NULL safety
    colour_palette_d <- colour_palette %||% col_palette_d %||% scales::hue_pal()
    colour_palette_c <- colour_palette %||% col_palette_c %||% scales::viridis_pal()
    colour_palette_o <- colour_palette %||% col_palette_o %||% scales::viridis_pal()
    if (is.null(colour_na)) {
      colour_na <- col_na
    }

    fill_palette_d <- fill_palette %||% col_palette_d %||% scales::hue_pal()
    fill_palette_c <- fill_palette %||% col_palette_c %||% scales::viridis_pal()
    fill_palette_o <- fill_palette %||% col_palette_o %||% scales::viridis_pal()
    if (is.null(fill_na)) {
      fill_na <- col_na
    }
  }

  # Get shape and linetype palettes from options
  if (rlang::is_null(shape_palette)) {
    shape_palette <- getOption("ggblanket.shape_palette_d")
  }
  if (rlang::is_null(linetype_palette)) {
    linetype_palette <- getOption("ggblanket.linetype_palette_d")
  }

  # Step 9: Initialize fixed_params list
  fixed_params <- list()

  # Handle colour fixed value (priority: colour > col)
  # IMPORTANT: Only set as fixed if it's not being used as an aesthetic
  # AND not in mapping
  if (!colour_map_or_set$is_aesthetic && !colour_in_mapping) {
    # colour is not mapped as aesthetic either directly or via col inheritance or mapping
    if (!is.null(colour_map_or_set$value)) {
      # Explicit colour value provided - include even if NA!
      fixed_params$colour <- colour_map_or_set$value
    } else if (!col_map_or_set$is_aesthetic && !is.null(col_map_or_set$value)) {
      if (!is.na(col_map_or_set$value)) {
        # No colour value, but col is set as fixed value (and it's not NA)
        if (
          is_bordered_geom &&
          !is.null(bordered_colour) &&
          is.function(bordered_colour)
        ) {
          fixed_params$colour <- bordered_colour(col_map_or_set$value)
        } else {
          fixed_params$colour <- col_map_or_set$value
        }
      }
      # If col IS NA, don't inherit it to colour
    } else {
      if (geom == "sf") {
        default_col <- ggplot2::get_geom_defaults("bar")$colour %||% "#8991A1FF"
      }
      else {
        default_col <- ggplot2::get_geom_defaults(geom)$colour %||% "#8991A1FF"
      }

      if (
        is_bordered_geom &&
        !is.null(bordered_colour) &&
        is.function(bordered_colour)
      ) {
        fixed_params$colour <- bordered_colour(default_col)
      } else {
        fixed_params$colour <- default_col
      }
    }
  }

  # Handle fill fixed value (priority: fill > col)
  # IMPORTANT: Only set as fixed if it's not being used as an aesthetic
  # AND not in mapping
  if (!fill_map_or_set$is_aesthetic && !fill_in_mapping) {
    # fill is not mapped as aesthetic either directly or via col inheritance or mapping
    if (!is.null(fill_map_or_set$value)) {
      # Explicit fill value provided - include even if NA!
      fixed_params$fill <- fill_map_or_set$value
    } else if (!col_map_or_set$is_aesthetic && !is.null(col_map_or_set$value)) {
      if (!is.na(col_map_or_set$value)) {
        # No fill value, but col is set as fixed value (and it's not NA)
        if (
          is_bordered_geom &&
          !is.null(bordered_fill) &&
          is.function(bordered_fill)
        ) {
          fixed_params$fill <- bordered_fill(col_map_or_set$value)
        } else {
          fixed_params$fill <- col_map_or_set$value
        }
      }
      # If col IS NA, don't inherit it to fill
    } else {
      if (geom == "sf") {
        default_fill <- ggplot2::get_geom_defaults("bar")$fill %||% "#8991A1FF"
      }
      else {
        default_fill <- ggplot2::get_geom_defaults(geom)$fill %||% "#8991A1FF"
      }

      if (
        is_bordered_geom &&
        !is.null(bordered_fill) &&
        is.function(bordered_fill)
      ) {
        fixed_params$fill <- bordered_fill(default_fill)
      } else {
        fixed_params$fill <- default_fill
      }
    }
  }

  # Fix 2: When col is mapped as an aesthetic, remove default colour/fill from fixed_params
  # UNLESS the user explicitly provided them as fixed values OR they're in mapping
  if (col_map_or_set$is_aesthetic) {
    # For colour: only remove if it wasn't explicitly set by the user and not in mapping
    if (
      !colour_map_or_set$is_aesthetic &&
      is.null(colour_map_or_set$value) &&
      !colour_in_mapping
    ) {
      # colour was not provided by user, so remove the default
      fixed_params$colour <- NULL
    }
    # For fill: only remove if it wasn't explicitly set by the user and not in mapping
    if (
      !fill_map_or_set$is_aesthetic &&
      is.null(fill_map_or_set$value) &&
      !fill_in_mapping
    ) {
      # fill was not provided by user, so remove the default
      fixed_params$fill <- NULL
    }
  }

  # Handle other fixed values with theme defaults
  if (!shape_map_or_set$is_aesthetic && !is.null(shape_map_or_set$value)) {
    fixed_params$shape <- shape_map_or_set$value
  } else if (!shape_map_or_set$is_aesthetic) {
    if (geom == "sf") {
      fixed_params$shape <- 21
    }
    else if (!shape_map_or_set$is_aesthetic) {
      fixed_params$shape <- ggplot2::get_geom_defaults(geom)$shape %||% 21
    }
  }

  if (
    !linetype_map_or_set$is_aesthetic && !is.null(linetype_map_or_set$value)
  ) {
    fixed_params$linetype <- linetype_map_or_set$value
  } else if (!linetype_map_or_set$is_aesthetic) {
    fixed_params$linetype <- ggplot2::get_geom_defaults(geom)$linetype %||% 1
  }

  if (
    !linewidth_map_or_set$is_aesthetic && !is.null(linewidth_map_or_set$value)
  ) {
    fixed_params$linewidth <- linewidth_map_or_set$value
  } else if (!linewidth_map_or_set$is_aesthetic) {
    if (geom == "smooth") {
      fixed_params$linewidth <- ggplot2::get_geom_defaults("line")$linewidth
    } else if (geom == "tile") {
      fixed_params$linewidth <- ggplot2::get_geom_defaults("bar")$linewidth
    } else if (geom == "sf") {
      fixed_params$linewidth <- ggplot2::get_geom_defaults("bar")$linewidth
    } else {
      fixed_params$linewidth <- ggplot2::get_geom_defaults(geom)$linewidth
    }
  }

  if (!size_map_or_set$is_aesthetic && !is.null(size_map_or_set$value)) {
    fixed_params$size <- size_map_or_set$value
  } else if (!size_map_or_set$is_aesthetic) {
    fixed_params$size <- theme_defaults$geom$size %||% 1.5
  }

  if (!alpha_map_or_set$is_aesthetic && !is.null(alpha_map_or_set$value)) {
    fixed_params$alpha <- alpha_map_or_set$value
  } else if (!alpha_map_or_set$is_aesthetic) {
    if (geom == "smooth") fixed_params$alpha <- NA
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
  aes_list$col <- if (col_map_or_set$is_aesthetic) {
    rlang::enquo(col)
  } else {
    rlang::quo(NULL)
  }

  # For colour: use colour if it's aesthetic, else use col if col is aesthetic AND colour wasn't explicitly set to NA
  aes_list$colour <- if (colour_map_or_set$is_aesthetic) {
    rlang::enquo(colour)
  } else if (
    col_map_or_set$is_aesthetic &&
    (is.null(colour_map_or_set$value) || !is.na(colour_map_or_set$value))
  ) {
    rlang::enquo(col) # Inherit from col only if colour wasn't explicitly NA
  } else {
    rlang::quo(NULL)
  }

  # For fill: use fill if it's aesthetic, else use col if col is aesthetic AND fill wasn't explicitly set to NA
  aes_list$fill <- if (fill_map_or_set$is_aesthetic) {
    rlang::enquo(fill)
  } else if (
    col_map_or_set$is_aesthetic &&
    (is.null(fill_map_or_set$value) || !is.na(fill_map_or_set$value))
  ) {
    rlang::enquo(col) # Inherit from col only if fill wasn't explicitly NA
  } else {
    rlang::quo(NULL)
  }

  # Other aesthetics
  aes_list$shape <- if (shape_map_or_set$is_aesthetic) {
    rlang::enquo(shape)
  } else {
    rlang::quo(NULL)
  }
  aes_list$linetype <- if (linetype_map_or_set$is_aesthetic) {
    rlang::enquo(linetype)
  } else {
    rlang::quo(NULL)
  }
  aes_list$linewidth <- if (linewidth_map_or_set$is_aesthetic) {
    rlang::enquo(linewidth)
  } else {
    rlang::quo(NULL)
  }
  aes_list$size <- if (size_map_or_set$is_aesthetic) {
    rlang::enquo(size)
  } else {
    rlang::quo(NULL)
  }
  aes_list$alpha <- if (alpha_map_or_set$is_aesthetic) {
    rlang::enquo(alpha)
  } else {
    rlang::quo(NULL)
  }

  # Step 9.5: Auto-set fill=NA when only colour is mapped (and vice versa)
  # This prevents unwanted fill colors when user only wants outlines
  if (colour_map_or_set$is_aesthetic && !fill_map_or_set$is_aesthetic &&
      !col_map_or_set$is_aesthetic && is.null(fill_map_or_set$value)) {
    # User mapped colour but not fill or col, and didn't set fill
    # Default to NA fill to show only outlines
    fixed_params$fill <- NA
  }

  if (fill_map_or_set$is_aesthetic && !colour_map_or_set$is_aesthetic &&
      !col_map_or_set$is_aesthetic && is.null(colour_map_or_set$value)) {
    # User mapped fill but not colour or col, and didn't set colour
    # Default to NA colour to show only fills
    fixed_params$colour <- NA
  }

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
  plot <- add_initial_layer(
    plot,
    geom,
    stat,
    position,
    params,
    show_legend,
    coord,
    blend
  )

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

  aspect <- get_aspect(
    aspect = aspect,
    x_scale_class = x_scale_class,
    y_scale_class = y_scale_class
  )

  if (rlang::is_null(x_limits_to_breaks)) {
    if (geom == "sf") x_limits_to_breaks <- FALSE
    else if (aspect == "x") {
      x_limits_to_breaks <- FALSE
    }
    else if (aspect == "y") {
      if (!facet_scales %in% c("free_x", "free")) x_limits_to_breaks <- TRUE
      else x_limits_to_breaks <- FALSE
    }
  }

  if (rlang::is_null(y_limits_to_breaks)) {
    if (geom == "sf") y_limits_to_breaks <- FALSE
    else if (aspect == "x") {
      if (!facet_scales %in% c("free_y", "free")) y_limits_to_breaks <- TRUE
      else y_limits_to_breaks <- FALSE
    }
    else if (aspect == "y") {
      y_limits_to_breaks <- FALSE
    }
  }

  titles_case <- get_titles_case(titles_case = titles_case)

  # Step 12: Check inputs
  validate_inputs(
    mapping,
    aspect,
    x_limits_to_breaks,
    y_limits_to_breaks,
    x_transform,
    y_transform,
    stat
  )

  # Step 13: Process the data
  data <- process_data(data, aes_list, aspect)

  # Step 14: Rebuild base plot with processed data
  plot <- initialise_ggplot_from_list(
    data = data,
    aes_list = aes_list,
    mapping = mapping
  )

  # Step 15: Add geom layer
  plot <- add_initial_layer(
    plot,
    geom,
    stat,
    position,
    params,
    show_legend,
    coord,
    blend
  )

  if (!is.null(x_limits_include)) {
    plot <- plot + ggplot2::expand_limits(x = x_limits_include)
  }

  if (!is.null(y_limits_include)) {
    plot <- plot + ggplot2::expand_limits(y = y_limits_include)
  }

  # Step 16: Add facet layer
  facet_layout <- get_facet_layout(facet_layout, aes_list)
  facet_axes <- get_facet_axes(facet_axes, x_limits_to_breaks)

  plot <- add_facet_layer(
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
    y_scale_class
  )

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
      stat = stat,
      col_scale_class = col_scale_class,
      aes_list = aes_list,
      data = data,
      plot_data = plot_data,
      plot_build = plot_build,
      x_limits_to_breaks = x_limits_to_breaks,
      is_bordered_geom = is_bordered_geom,
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
      colour_palette_d = colour_palette_d,
      colour_palette_c = colour_palette_c,
      colour_palette_o = colour_palette_o,
      colour_na = colour_na,
      fill_palette_d = fill_palette_d,
      fill_palette_c = fill_palette_c,
      fill_palette_o = fill_palette_o,
      fill_na = fill_na,
      bordered_colour = bordered_colour,
      bordered_fill = bordered_fill
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
      shape_values <- if (!is.na(shape_n)) {
        shape_palette(shape_n)
      } else {
        shape_palette
      }
    } else {
      shape_values <- if (
        !is.na(shape_n) && !any(rlang::have_name(shape_palette))
      ) {
        shape_palette[1:shape_n]
      } else {
        shape_palette
      }
    }

    # Reverse the shape values and legend if required
    if (aspect == "y") {
      shape_legend_rev <- TRUE
      shape_values <- rev(shape_values)
    } else {
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
      linetype_values <- if (!is.na(linetype_n)) {
        linetype_palette(linetype_n)
      } else {
        linetype_palette
      }
    } else {
      linetype_values <- if (
        !is.na(linetype_n) && !any(rlang::have_name(linetype_palette))
      ) {
        linetype_palette[1:linetype_n]
      } else {
        linetype_palette
      }
    }

    # Reverse the linetype values and legend if required
    if (aspect == "y") {
      linetype_legend_rev <- TRUE
      linetype_values <- rev(linetype_values)
    } else {
      linetype_legend_rev <- FALSE
    }

    plot <- plot +
      ggplot2::scale_linetype_manual(values = linetype_values) +
      ggplot2::guides(
        linetype = ggplot2::guide_legend(reverse = linetype_legend_rev)
      )
  }

  # Step 20.5: Apply grey styling to secondary aesthetic guides
  # This must come AFTER all scales are added
  if (!is.na(col_scale_class)) {
    # Check if col is mapped
    col_mapped <- !rlang::quo_is_null(aes_list$col) ||
      !rlang::quo_is_null(aes_list$colour) ||
      !rlang::quo_is_null(aes_list$fill)

    if (col_mapped) {
      plot <- apply_secondary_grey_guides(
        plot = plot,
        aes_list = aes_list,
        data = data,
        geom = geom,
        is_bordered_geom = is_bordered_geom,
        bordered_colour = bordered_colour,
        bordered_fill = bordered_fill,
        col_legend_ncol = col_legend_ncol,
        col_legend_nrow = col_legend_nrow,
        shape_legend_rev = if (aspect == "y") TRUE else FALSE,
        linetype_legend_rev = if (aspect == "y") TRUE else FALSE
      )
    }
  }

  # Step 21: Add positional scales
  # Make x scale
  if (x_scale_class == "discrete") {
    if (is.null(x_expand)) {
      x_expand <- ggplot2::waiver()
    }
    if (is.null(x_labels)) {
      x_labels <- ggplot2::waiver()
    }
    if (is.null(x_breaks)) {
      x_breaks <- ggplot2::waiver()
    }

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
        x_transform = x_transform, x_limits_to_breaks = x_limits_to_breaks,
        plot_data = plot_data
      )
  }

  # Make y scale
  if (y_scale_class == "discrete") {
    if (is.null(y_expand)) {
      y_expand <- ggplot2::waiver()
    }
    if (is.null(y_labels)) {
      y_labels <- ggplot2::waiver()
    }
    if (is.null(y_breaks)) {
      y_breaks <- ggplot2::waiver()
    }

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
        y_transform = y_transform, y_limits_to_breaks = y_limits_to_breaks,
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
  transparency <- get_aspect_behaviour(
    aspect_axis_line,
    aspect_axis_ticks,
    aspect_panel_grid
  )

  plot <- add_aspect(
    plot,
    aspect,
    transparency$aspect_axis_line,
    transparency$aspect_axis_ticks,
    transparency$aspect_panel_grid,
    x_scale_class,
    y_scale_class
  )

  return(plot)
}

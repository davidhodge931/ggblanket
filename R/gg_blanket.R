#' Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()` with [geom_blank()][ggplot2::geom_blank()] defaults. This function underlies all other `gg_*` functions. It contains a `geom` argument for maximum flexibility.
#'
#' @param data A data frame or tibble.
#' @param ... Other arguments passed to within a `params` list in `layer()`.
#' @param geom A geometric object to display the data. A snakecase character string of a ggproto Geom subclass object minus the Geom prefix (e.g. `"point"`).
#' @param stat A statistical transformation to use on the data. A snakecase character string of a ggproto Stat subclass object minus the Stat prefix (e.g. `"identity"`).
#' @param position A position adjustment. A snakecase character string of a ggproto Position subclass object minus the Position prefix (e.g. `"identity"`), or a `position_*()` function that outputs a ggproto Position subclass object (e.g. `ggplot2::position_identity()`).
#' @param coord A coordinate system. A `coord_*()` function that outputs a constructed ggproto Coord subclass object (e.g. [ggplot2::coord_cartesian()]).
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#' @param mode_orientation The orientation of plot, which affects the theme components that are removed from the mode. Either "x" or "y".
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,facet,facet2,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' @param mapping A set of additional aesthetic mappings in [ggplot2::aes()]. Intended primarily for non-supported aesthetics (e.g. `shape`, `linetype`, `linewidth`, or `size`), but can also be used for delayed evaluation etc.
#' @param x_breaks,y_breaks,col_breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param x_breaks_n,y_breaks_n,col_breaks_n A number of desired breaks for when `*_breaks = NULL`.
#' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param x_expand_limits,y_expand_limits,col_expand_limits For a continuous variable, any values that the limits should encompass (e.g. `0`). For a discrete scale, manipulate the data instead with `forcats::fct_expand`.
#' @param x_label,y_label,col_label Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)` for no title.
#' @param x_labels,y_labels,col_labels,facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels. (Note this must be named for `facet_labels`).
#' @param x_position,y_position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).If using `y_position = "top"` with a `*_mode_*` theme, add `caption = ""` or `caption = "\n"`.
#' @param x_sec_axis,y_sec_axis A secondary axis with [ggplot2::dup_axis()] or  [ggplot2::sec_axis()].
#' @param x_symmetric,y_symmetric `TRUE` or `FALSE` of whether a symmetric scale.
#' @param x_transform,y_transform,col_transform For a continuous scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param col_drop,facet_drop For a discrete variable, FALSE or TRUE of whether to drop unused levels.
#' @param col_legend_ncol,col_legend_nrow The number of columns and rows in a legend guide.
#' @param col_legend_rev `TRUE` or `FALSE` of whether to reverse the elements of a legend guide. Defaults to `FALSE`.
#' @param col_palette A character vector of hex codes (or names) or a `scales::pal_*()` function.
#' @param col_palette_na A hex code (or name) for the colour of `NA` values.
#' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' @param col_steps For a continuous variable, `TRUE` or `FALSE` of whether to colour in steps. Defaults to `FALSE`.
#' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`. Sometimes `+ *_mode_*()` may be needed.
#' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' @param facet_ncol,facet_nrow The number of columns and rows of facet panels. Only applies to a facet layout of `"wrap"`.
#' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param facet_space When the facet layout is `"grid"` and facet scales are not `"fixed"`, whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param caption Caption title string.
#' @param label_to_case A function to format the default `x_label`, `y_label` and `col_label` of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins %>%
#'   gg_blanket(
#'     geom = "violin",
#'     stat = "ydensity",
#'     position = "dodge",
#'     x = species,
#'     y = body_mass_g,
#'     col = sex,
#'   )
#'
gg_blanket <- function(data = NULL,
                       ...,
                       geom = "blank",
                       stat = "identity",
                       position = "identity",
                       coord = NULL,
                       mode = NULL, mode_orientation = NULL,
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
                       label = NULL,
                       text = NULL,
                       sample = NULL,
                       mapping = NULL,
                       x_breaks = NULL, x_breaks_n = NULL,
                       x_expand = NULL,
                       x_expand_limits = NULL,
                       x_label = NULL, x_labels = NULL,


                       x_position = "bottom",

                       x_sec_axis = ggplot2::waiver(), x_symmetric = NULL, x_transform = NULL,
                       y_breaks = NULL, y_breaks_n = NULL,
                       y_expand = NULL,
                       y_expand_limits = NULL,
                       y_label = NULL, y_labels = NULL,
                        y_position = "left",
                       y_sec_axis = ggplot2::waiver(),
                       y_symmetric = NULL,

                       y_transform = NULL,
                       col_breaks = NULL, col_breaks_n = 5,
                       col_drop = FALSE,
                       col_expand_limits = NULL,
                       col_label = NULL, col_labels = NULL,
                       col_legend_ncol = NULL,
                       col_legend_nrow = NULL,
                       col_legend_rev = FALSE,


                       col_palette = NULL,
                       col_palette_na = NULL,
                       col_rescale = scales::rescale(),
                       col_steps = FALSE,

                       col_transform = NULL,
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
                       label_to_case = snakecase::to_sentence_case) {

  ##############################################################################
  #quote
  ##############################################################################

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
  label <- rlang::enquo(label)
  text <- rlang::enquo(text)
  sample <- rlang::enquo(sample)

  ##############################################################################
  #make gg_function work
  ##############################################################################

  if (rlang::is_null(data)) data <- data.frame(x = NA)

  ##############################################################################
  #get geom, stat & position strings
  ##############################################################################

  if (ggplot2::is.ggproto(geom)) {
    geom_name <- stringr::str_to_lower(stringr::str_remove(class(geom)[1], "Geom"))
  }
  else if (is.character(geom)) geom_name <- geom

  if (ggplot2::is.ggproto(stat)) {
    stat_name <- stringr::str_to_lower(stringr::str_remove(class(stat)[1], "Stat"))
  }
  else if (is.character(stat)) stat_name <- stat

  if (ggplot2::is.ggproto(position)) {
    position_name <- stringr::str_to_lower(stringr::str_remove(class(position)[1], "Position"))
  }
  else if (is.character(position)) position_name <- position

  ##############################################################################
  # determine scale type
  ##############################################################################

  plot <- get_base(
    data = data,
    x = !!x,
    y = !!y,
    col = !!col,
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
    text = !!text,
  )

  if (geom_name == "blank") show_legend <- FALSE
  else show_legend <- TRUE

  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")

    plot <- plot +
      ggplot2::layer_sf(
        geom = geom,
        stat = stat,
        position = position,
        mapping = ggplot2::aes(!!!mapping),
        params = rlang::list2(...),
        show.legend = show_legend,
      ) +
      coord
  }
  else {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_cartesian(clip = "off")

    plot <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        mapping = ggplot2::aes(!!!mapping),
        params = rlang::list2(...),
        show.legend = show_legend,
      ) +
      coord
  }

  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]
    })
  })

  plot_scales <- purrr::map_chr(plot_build$plot$scales$scales, function(x) {
    ifelse(rlang::is_null(rlang::call_name(x[["call"]])), NA,
           rlang::call_name(x[["call"]]))
  })

  if (any(plot_scales %in% "scale_x_discrete")) x_scale_type <- "discrete"
  else if (any(plot_scales %in% "scale_x_date")) x_scale_type <- "date"
  else if (any(plot_scales %in% "scale_x_datetime")) x_scale_type <- "datetime"
  else if (any(plot_scales %in% "scale_x_time")) x_scale_type <- "time"
  else if (any(plot_scales %in% "scale_x_continuous")) x_scale_type <- "numeric"
  else x_scale_type <- "numeric"

  if (any(plot_scales %in% "scale_y_discrete")) y_scale_type <- "discrete"
  else if (any(plot_scales %in% "scale_y_date")) y_scale_type <- "date"
  else if (any(plot_scales %in% "scale_y_datetime")) y_scale_type <- "datetime"
  else if (any(plot_scales %in% "scale_y_time")) y_scale_type <- "time"
  else if (any(plot_scales %in% "scale_y_continuous")) y_scale_type <- "numeric"
  else y_scale_type <- "numeric"

  if (any(plot_scales %in% c("scale_colour_discrete", "scale_fill_discrete"))) col_scale_type <- "discrete"
  else if (any(plot_scales %in% c("scale_colour_ordinal", "scale_fill_ordinal"))) col_scale_type <- "ordinal"
  else if (any(plot_scales %in% c("scale_colour_date", "scale_fill_date"))) col_scale_type <- "date"
  else if (any(plot_scales %in% c("scale_colour_datetime", "scale_fill_datetime"))) col_scale_type <- "datetime"
  else if (any(plot_scales %in% c("scale_colour_time", "scale_fill_time"))) col_scale_type <- "time"
  else if (any(plot_scales %in% c("scale_colour_continuous", "scale_fill_continuous"))) col_scale_type <- "numeric"
  else col_scale_type <- "numeric"

  if (!rlang::quo_is_null(col)) {
    if (inherits(rlang::eval_tidy(col, data), what = c("hms"))) {
      col_scale_type <- "time"
    }
  }

  ##############################################################################
  # get more defaults
  ##############################################################################

  #get x_transform if NULL
  if (rlang::is_null(x_transform)) {
    if (x_scale_type == "time") x_transform <- scales::transform_hms()
    else if (x_scale_type == "datetime") x_transform <- scales::transform_time()
    else if (x_scale_type == "date") x_transform <- scales::transform_date()
    else x_transform <- scales::transform_identity()
  }
  #get y_transform if NULL
  if (rlang::is_null(y_transform)) {
    if (y_scale_type == "time") y_transform <- scales::transform_hms()
    else if (y_scale_type == "datetime")  y_transform <- scales::transform_time()
    else if (y_scale_type == "date")  y_transform <- scales::transform_date()
    else y_transform <- scales::transform_identity()
  }

  #make drop appropriate to facet scales
  x_drop <- ifelse(facet_scales %in% c("free_x", "free"), TRUE, FALSE)
  y_drop <- ifelse(facet_scales %in% c("free_y", "free"), TRUE, FALSE)

  #get mode if NULL
  if (rlang::is_null(mode)) {
    mode <- get_mode()
  }

  #determine *_symmetric
  if (rlang::is_null(x_symmetric)) {
    if (stringr::str_detect(stat_name, "sf")) x_symmetric <- FALSE
    else if (facet_scales %in% c("free", "free_x")) x_symmetric <- FALSE
    else if (y_scale_type == "discrete" & x_scale_type != "discrete") x_symmetric <- TRUE
    else x_symmetric <- FALSE
  }

  if (rlang::is_null(y_symmetric)) {
    if (stringr::str_detect(stat_name, "sf")) y_symmetric <- FALSE
    else if (facet_scales %in% c("free", "free_y")) y_symmetric <- FALSE
    else if (y_scale_type == "discrete" & x_scale_type != "discrete") y_symmetric <- FALSE
    else y_symmetric <- TRUE
  }

  if (rlang::is_null(mode_orientation)) {
    if (y_scale_type == "discrete" & x_scale_type != "discrete") {
      mode_orientation <- "y"
    }
    else {
      mode_orientation <- "x"
    }
  }

  ##############################################################################
  #abort if necessary
  ##############################################################################

  if (!rlang::is_null(mapping)) {
    if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  if (x_symmetric & y_symmetric) {
    rlang::abort("Both x_symmetric and y_symmetric are not supported: please make one FALSE.")
  }

  ##############################################################################
  # process the data
  ##############################################################################

  data <- data %>%
    #ungroup the data
    dplyr::ungroup() %>%
    #convert to factors class that can handle labels
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!facet, !!facet2) &
                                  (tidyselect::where(is.character) | tidyselect::where(is.factor) | tidyselect::where(is.logical)),
                                function(x) labelled::to_factor(x))) %>%
    #reverse y*, so that reads top low-levels to bottom high-levels
    dplyr::mutate(dplyr::across(c(!!y, !!ymin, !!ymax, !!yend) &
                                  tidyselect::where(is.factor),
                                function(x) forcats::fct_rev(x)))

  #if flipped, order col correctly
  if ((!identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data))) & x_symmetric) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!col & tidyselect::where(is.factor),
                                  function(x) forcats::fct_rev(x)))
  }

  ##############################################################################
  # get the base plot using processed data
  ##############################################################################

  plot <- get_base(
    data = data,
    x = !!x,
    y = !!y,
    col = !!col,
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
    text = !!text,
  ) +
    mode

  ##############################################################################
  # Add geom layer
  ##############################################################################

  if (geom_name == "blank") show_legend <- FALSE
  else show_legend <- TRUE

  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")

    plot <- plot +
      ggplot2::layer_sf(
        geom = geom,
        stat = stat,
        position = position,
        mapping = ggplot2::aes(!!!mapping),
        params = rlang::list2(...),
        show.legend = show_legend,
      ) +
      coord
  }
  else {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_cartesian(clip = "off")

    plot <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        mapping = ggplot2::aes(!!!mapping),
        params = rlang::list2(...),
        show.legend = show_legend,
      ) +
      coord
  }

  if (!rlang::is_null(x_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(x = x_expand_limits)
  }

  if (!rlang::is_null(y_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(y = y_expand_limits)
  }


  ##############################################################################
  # Add facet layer
  ##############################################################################

  #get layout if NULL
  if (rlang::is_null(facet_layout)) {
    if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) facet_layout <- "wrap"
    else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else facet_layout <- "null"
  }

  if (rlang::is_null(facet_axes)) {
    if (x_symmetric) facet_axes <- "all_y"
    else facet_axes <- "all_x"
  }

  #add facet layer. Reverse facet if y is already reversed to keep order correct
  if (y_scale_type == "discrete" & (identical(rlang::eval_tidy(y, data), rlang::eval_tidy(facet, data)))) {
    if (facet_layout == "wrap") {
      if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(forcats::fct_rev(!!facet)),
            scales = facet_scales,
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet2),
            scales = facet_scales,
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet, !!facet2),
            scales = facet_scales,
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
    else if (facet_layout == "grid") {
      if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              cols = ggplot2::vars(forcats::fct_rev(!!facet)),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(cols = ggplot2::vars(forcats::fct_rev(!!facet)),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
  }
  else {
    if (facet_layout == "wrap") {
      if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet),
            scales = facet_scales,
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet2),
            scales = facet_scales,
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet, !!facet2),
            scales = facet_scales,
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
    else if (facet_layout == "grid") {
      if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              cols = ggplot2::vars(!!facet),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(cols = ggplot2::vars(!!facet),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
  }

  #so that col_n can identify complete set of required non-NA colours
  #and work even if col_palette_d sets insufficient colours

  if (col_scale_type %in% c("discrete", "ordinal")) {
    plot <- plot +
      ggplot2::scale_colour_hue(na.value = "grey50") +
      ggplot2::scale_fill_hue(na.value = "grey50")
  }

  ##############################################################################
  # get the plot build and plot data
  ##############################################################################

  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]

      facet_nrows <- length(unique(plot_build$layout$layout$ROW))
      facet_ncols <- length(unique(plot_build$layout$layout$COL))
    })
  })

  ##############################################################################
  # make colour scale
  ##############################################################################

  if (!is.na(col_scale_type)) {
    if (col_scale_type %in% c("date", "datetime", "time", "numeric")) {

      if (rlang::is_null(col_palette)) {
        col_palette <- get_col_palette_c()
        if (rlang::is_null(col_palette)) {
          col_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")(seq(0, 1, length.out = 20))
        }
      } else if (rlang::is_function(col_palette)) col_palette <- col_palette(20)

      if (rlang::is_null(col_palette_na)) {
        col_palette_na <- get_col_palette_na_c()
        if (rlang::is_null(col_palette_na)) col_palette_na <- "grey50"
      }

      #get col_transform if NULL
      if (rlang::is_null(col_transform)) {
        if (col_scale_type == "time") col_transform <- scales::transform_hms()
        else if (col_scale_type == "datetime") col_transform <- scales::transform_time()
        else if (col_scale_type == "date") col_transform <- scales::transform_date()
        else col_transform <- scales::transform_identity()
      }

      #make a tidy name to deal with composed transforms
      if (inherits(col_transform, what = "transform")) {
        col_transform <- col_transform$name %>%
          stringr::str_remove("composition") %>%
          stringr::str_remove("\\(") %>%
          stringr::str_remove("\\)") %>%
          stringr::str_split(",") %>%
          unlist()
      }

      if (rlang::is_null(col_breaks)) {
        if (any(col_transform %in% c("hms", "time", "datetime", "date"))) {
          col_breaks <- scales::breaks_pretty(n = col_breaks_n)
        }
        else {
          col_breaks <- scales::breaks_extended(n = col_breaks_n, only.loose = FALSE)
        }
      }

      if (rlang::is_null(col_labels)) {
        if (any(col_transform %in% c("hms"))) col_labels <- scales::label_time()
        else if (any(col_transform %in% c("date", "datetime", "time"))) col_labels <- scales::label_date_short()
        else col_labels <- scales::label_comma(drop0trailing = TRUE)
      }

      if (isFALSE(col_steps)) {
        plot <- plot +
          ggplot2::scale_colour_gradientn(
            colours = col_palette,
            values = col_rescale,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = scales::oob_keep,
            na.value = col_palette_na,
            aesthetics = c("colour", "fill")
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
            fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
          )
      }
      else if (isTRUE(col_steps)) {
        plot <- plot +
          ggplot2::scale_colour_stepsn(
            colours = col_palette,
            values = col_rescale,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = scales::oob_keep,
            na.value = col_palette_na,
            aesthetics = c("colour", "fill")
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_coloursteps(
              reverse = col_legend_rev,
              theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())),
            fill = ggplot2::guide_coloursteps(
              reverse = col_legend_rev,
              theme = ggplot2::theme(legend.ticks = ggplot2::element_blank()))
          )
      }
    }
    else if (col_scale_type %in% c("discrete", "ordinal")) {

      if (rlang::is_null(col_palette_na)) {
        if (col_scale_type == "discrete") {
          if (rlang::is_null(get_col_palette_na_d())) col_palette_na <- "grey50"
          else col_palette_na <- get_col_palette_na_d()
        }
        else if (col_scale_type == "ordinal") {
          if (rlang::is_null(get_col_palette_na_o())) col_palette_na <- "grey50"
          else col_palette_na <- get_col_palette_na_o()
        }
      }

      colour_distinct <- plot_data %>%
        dplyr::select(tidyselect::any_of("colour")) %>%
        dplyr::distinct()

      if (ncol(colour_distinct) > 0) {
        colour_n <- colour_distinct %>%
          dplyr::filter(.data$colour != "grey50") |>
          dplyr::count() |>
          dplyr::pull()
      } else colour_n <- 1

      fill_distinct <- plot_data %>%
        dplyr::select(tidyselect::any_of("fill")) %>%
        dplyr::distinct()

      if (ncol(fill_distinct) > 0) {
        fill_n <- fill_distinct %>%
          dplyr::filter(.data$fill != "grey50") |>
          dplyr::count() |>
          dplyr::pull()
      } else fill_n <- 1

      col_n <- max(colour_n, fill_n)

      if (rlang::is_null(col_palette)) {
        if (col_scale_type == "discrete") col_palette <- get_col_palette_d()
        else if (col_scale_type == "ordinal") col_palette <- get_col_palette_o()
      }

      if (!rlang::is_null(col_n))  {
        if (rlang::is_function(col_palette)) col_palette <- col_palette(col_n)
        else if (!any(rlang::have_name(col_palette))) col_palette <- col_palette[1:col_n]
      }

      if (x_symmetric) {
        col_legend_rev <- !col_legend_rev
        if (col_scale_type == "discrete") {
          col_palette <- rev(col_palette)
        }
      }

      if (col_scale_type == "ordinal") col_legend_rev <- !col_legend_rev

      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()

      if (!rlang::is_null(col_palette)) {
        if (rlang::is_vector(col_palette)) {

          plot <- plot +
            ggplot2::scale_colour_manual(
              values = col_palette,
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
            ) +
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
        else if (rlang::is_function(col_palette)) {
          plot <- plot +
            ggplot2::discrete_scale(
              palette = col_palette,
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
            ) +
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
      }
      else {
        if (rlang::is_null(col_palette_na)) col_palette_na <- "grey50"

        if (col_scale_type == "discrete") {
          plot <- plot +
            ggplot2::scale_colour_hue(
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
            ) +
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
        else if (col_scale_type == "ordinal") {
          plot <- plot +
            ggplot2::scale_colour_viridis_d(
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
            ) +
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
      }

      if (!rlang::is_null(plot_build$plot$labels$alpha)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
            plot <- plot +
              ggplot2::guides(
                alpha = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
            plot <- plot +
              ggplot2::guides(
                alpha = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$shape)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
            plot <- plot +
              ggplot2::guides(
                shape = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
            plot <- plot +
              ggplot2::guides(
                shape = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$size)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
            plot <- plot +
              ggplot2::guides(
                size = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
            plot <- plot +
              ggplot2::guides(
                size = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$linewidth)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
            plot <- plot +
              ggplot2::guides(
                linewidth = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
            plot <- plot +
              ggplot2::guides(
                linewidth = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$linetype)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
            plot <- plot +
              ggplot2::guides(
                linetype = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
            plot <- plot +
              ggplot2::guides(
                linetype = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$pattern)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
            plot <- plot +
              ggplot2::guides(
                pattern = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
            plot <- plot +
              ggplot2::guides(
                pattern = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

    }

    #expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        colour = col_expand_limits,
        fill = col_expand_limits
      )
  }

  ##############################################################################
  # add positional scales
  ##############################################################################

  #Make x scale
  if (x_scale_type  == "discrete") {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_x_discrete(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        drop = x_drop,
        position = x_position
      )
  }
  else {
    if (stringr::str_detect(stat_name, "sf")) {
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
      if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    }

    if (rlang::is_null(x_breaks_n)) {
      if (facet_ncols == 1) x_breaks_n <- 6
      else if (facet_ncols == 2) x_breaks_n <- 5
      else if (facet_ncols == 3) x_breaks_n <- 4
      else x_breaks_n <- 3
    }

    if (x_symmetric) {
      data_x <- plot_data %>%
        dplyr::select(tidyselect::matches(stringr::regex("^(?!xid|xbin)x.*"))) %>%
        tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "x") %>%
        dplyr::filter(!is.na(.data$x))

      plot <- plot +
        scale_x_symmetric(
          data = data_x,
          x = x,
          symmetric = TRUE,
          breaks = x_breaks,
          breaks_n = x_breaks_n,
          expand = x_expand,
          expand_limits = x_expand_limits,
          labels = x_labels,
          position = x_position,
          sec_axis = x_sec_axis,
          transform = x_transform
        )
    }
    else {
      plot <- plot +
        scale_x_symmetric(
          symmetric = FALSE,
          breaks = x_breaks,
          breaks_n = x_breaks_n,
          expand = x_expand,
          expand_limits = x_expand_limits,
          labels = x_labels,
          position = x_position,
          sec_axis = x_sec_axis,
          transform = x_transform
        )
    }
  }

  #Make y scale
  if (y_scale_type  == "discrete") {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_y_discrete(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels,
        drop = y_drop,
        position = y_position
      )
  }
  else {
    if (stringr::str_detect(stat_name, "sf")) {
      if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
      if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    }

    if (rlang::is_null(y_breaks_n)) {
      if (facet_nrows == 1) y_breaks_n <- 6
      else if (facet_nrows == 2) y_breaks_n <- 5
      else if (facet_nrows == 3) y_breaks_n <- 4
      else y_breaks_n <- 3
    }

    if (y_symmetric) {
      data_y <- plot_data %>%
        dplyr::select(tidyselect::matches(stringr::regex("^(?!yid|ybin)y.*"))) %>%
        tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "y") %>%
        dplyr::filter(!is.na(.data$y))

      plot <- plot +
        scale_y_symmetric(
          data = data_y,
          y = y,
          symmetric = TRUE,
          breaks = y_breaks,
          breaks_n = y_breaks_n,
          expand = y_expand,
          expand_limits = y_expand_limits,
          labels = y_labels,
          position = y_position,
          sec_axis = y_sec_axis,
          transform = y_transform
        )
    }
    else {
      plot <- plot +
        scale_y_symmetric(
          symmetric = FALSE,
          breaks = y_breaks,
          breaks_n = y_breaks_n,
          expand = y_expand,
          expand_limits = y_expand_limits,
          labels = y_labels,
          position = y_position,
          sec_axis = y_sec_axis,
          transform = y_transform
        )
    }
  }

  #############################################################################
  # get x_label, y_label and col_label, if NULL
  #############################################################################

  if (rlang::is_null(x_label)) {
    if ((!rlang::quo_is_null(x))) {
      if (!rlang::is_null(attr(dplyr::pull(data, !!x), "label"))) {
        x_label <- attr(dplyr::pull(data, !!x), "label")
      }
      else {
        if (stringr::str_detect(stat_name, "sf")) {
          x_label <- ""
        }
        else if (!rlang::is_null(plot_build$plot$labels$x)) {
          x_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), label_to_case)
        }
      }
    }
    else {
      if (stringr::str_detect(stat_name, "sf")) {
        x_label <- ""
      }
      else if (!rlang::is_null(plot_build$plot$labels$x)) {
        x_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), label_to_case)
      }
      else x_label <- purrr::map_chr("x", label_to_case)
    }
  }

  if (rlang::is_null(y_label)) {
    if ((!rlang::quo_is_null(y))) {
      if (!rlang::is_null(attr(dplyr::pull(data, !!y), "label"))) {
        y_label <- attr(dplyr::pull(data, !!y), "label")
      }
      else {
        if (stringr::str_detect(stat_name, "sf")) {
          y_label <- ""
        }
        else if (!rlang::is_null(plot_build$plot$labels$y)) {
          y_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$y[1]), label_to_case)
        }
      }
    }
    else {
      if (stringr::str_detect(stat_name, "sf")) {
        y_label <- ""
      }
      else if (!rlang::is_null(plot_build$plot$labels$y)) {
        y_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$y[1]), label_to_case)
      }
      else y_label <- purrr::map_chr("y", label_to_case)
    }
  }

  if (rlang::is_null(col_label)) {
    if ((!rlang::quo_is_null(col))) {
      if (!rlang::is_null(attr(dplyr::pull(data, !!col), "label"))) {
        col_label <- attr(dplyr::pull(data, !!col), "label")
      }
      else {
        if (!rlang::is_null(plot_build$plot$labels$fill)) {
          col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$fill[1]), label_to_case)
        }
        else if (!rlang::is_null(plot_build$plot$labels$colour)) {
          col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$colour[1]), label_to_case)
        }
      }
    }
    else {
      if (!rlang::is_null(plot_build$plot$labels$fill)) {
        col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$fill[1]), label_to_case)
      }
      else if (!rlang::is_null(plot_build$plot$labels$colour)) {
        col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$colour[1]), label_to_case)
      }
    }
  }

  if (!rlang::is_null(plot_build$plot$labels$alpha)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
        alpha_label <- col_label
      }
      else alpha_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
        alpha_label <- col_label
      }
      else alpha_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), label_to_case)
    }
    else {
      alpha_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), label_to_case)
    }
  } else alpha_label <- NULL

  if (!rlang::is_null(plot_build$plot$labels$shape)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
        shape_label <- col_label
      }
      else shape_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
        shape_label <- col_label
      }
      else shape_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), label_to_case)
    }
    else {
      shape_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), label_to_case)
    }
  } else shape_label <- NULL

  if (!rlang::is_null(plot_build$plot$labels$size)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
        size_label <- col_label
      }
      else size_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
        size_label <- col_label
      }
      else size_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), label_to_case)
    }
    else {
      size_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), label_to_case)
    }
  } else size_label <- NULL

  if (!rlang::is_null(plot_build$plot$labels$linewidth)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
        linewidth_label <- col_label
      }
      else linewidth_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linewidth[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
        linewidth_label <- col_label
      }
      else linewidth_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linewidth[1]), label_to_case)
    }
    else {
      linewidth_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linewidth[1]), label_to_case)
    }
  } else linewidth_label <- NULL

  if (!rlang::is_null(plot_build$plot$labels$linetype)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
        linetype_label <- col_label
      }
      else linetype_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
        linetype_label <- col_label
      }
      else linetype_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), label_to_case)
    }
    else {
      linetype_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), label_to_case)
    }
  } else linetype_label <- NULL

  if (!rlang::is_null(plot_build$plot$labels$pattern)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
        pattern_label <- col_label
      }
      else pattern_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$pattern[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
        pattern_label <- col_label
      }
      else pattern_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$pattern[1]), label_to_case)
    }
    else {
      pattern_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$pattern[1]), label_to_case)
    }
  } else pattern_label <- NULL

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_label,
      y = y_label,
      colour = col_label,
      fill = col_label,
      alpha = alpha_label,
      shape = shape_label,
      size = size_label,
      linewidth = linewidth_label,
      linetype = linetype_label,
      pattern = pattern_label
    ) +
    mode

  ##############################################################################
  # mode make transparent some theme components
  ##############################################################################

  if (mode_orientation == "x") {
    plot <- plot +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
        panel.grid.minor.x = ggplot2::element_line(colour = "transparent"),
        axis.line.y = ggplot2::element_line(colour = "transparent"),
        axis.ticks.y = ggplot2::element_line(colour = "transparent")
      )

    if (x_scale_type == "discrete") {
      plot <- plot +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_line(colour = "transparent")
        )
    }
  }
  else if (mode_orientation == "y") {
    plot <- plot +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
        panel.grid.minor.y = ggplot2::element_line(colour = "transparent"),
        axis.line.x = ggplot2::element_line(colour = "transparent"),
        axis.ticks.x = ggplot2::element_line(colour = "transparent")
      )

    if (y_scale_type == "discrete") {
      plot <- plot +
        ggplot2::theme(
          axis.ticks.y = ggplot2::element_line(colour = "transparent")
        )
    }
  }

  ##############################################################################
  # add the theme if globally set
  ##############################################################################

  if (rlang::is_null(mode)) {
    if (rlang::is_null(get_mode())) {
      if (!rlang::is_null(get_theme())) {
        plot <- plot +
          get_theme()
      }
      else {
        plot <- plot +
          ggplot2::theme_grey()

        if (mode_orientation == "x") {
          plot <- plot +
            ggplot2::theme(
              panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
              panel.grid.minor.x = ggplot2::element_line(colour = "transparent"),
              axis.line.y = ggplot2::element_line(colour = "transparent"),
              axis.ticks.y = ggplot2::element_line(colour = "transparent")
            )

          if (x_scale_type == "discrete") {
            plot <- plot +
              ggplot2::theme(
                axis.ticks.x = ggplot2::element_line(colour = "transparent")
              )
          }
        }
        else if (mode_orientation == "y") {
          plot <- plot +
            ggplot2::theme(
              panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
              panel.grid.minor.y = ggplot2::element_line(colour = "transparent"),
              axis.line.x = ggplot2::element_line(colour = "transparent"),
              axis.ticks.x = ggplot2::element_line(colour = "transparent")
            )

          if (y_scale_type == "discrete") {
            plot <- plot +
              ggplot2::theme(
                axis.ticks.y = ggplot2::element_line(colour = "transparent")
              )
          }
        }
      }
    }
  }


  return(plot)
}

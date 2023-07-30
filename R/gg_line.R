#' @title Line ggplot
#'
#' @description Create a line ggplot with a wrapper around ggplot2::geom_line(stat = "identity", ...).
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable.
#' @param group Unquoted group aesthetic variable.
#' @param text Unquoted text aesthetic variable.
#' @param position Position adjustment. Either a character string (e.g."identity"), or a function (e.g. ggplot2::position_identity()).
#' @param coord A coordinate function from ggplot2 (e.g. ggplot2::coord_cartesian(clip = "off")).
#' @param pal Colours to use. A character vector of hex codes (or names).
#' @param pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1.
#' @param ... Other arguments passed to the ggplot2::geom_line function.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param x_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_gridlines TRUE or FALSE for vertical x gridlines. NULL guesses based on the classes of the x and y.
#' @param x_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis.
#' @param x_oob A scales::oob_* function that handles values outside of limits for continuous scales. Defaults to scales::oob_keep.
#' @param x_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param x_trans For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param y_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_gridlines TRUE or FALSE of horizontal y gridlines. NULL guesses based on the classes of the x and y.
#' @param y_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis.
#' @param y_oob A scales::oob_* function that handles values outside of limits for continuous scales. Defaults to scales::oob_keep.
#' @param y_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param y_trans For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param col_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_continuous Type of colouring for a continuous variable. Either "gradient" or "steps". Defaults to "steps" - or just the first letter of these e.g. "g".
#' @param col_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. Either "b" (bottom), "r" (right), "t" (top) or "l" (left).
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_limits A vector to determine the limits of the colour scale.
#' @param col_oob A scales::oob_* function that handles values outside of limits for continuous scales. Defaults to scales::oob_keep.
#' @param col_rescale For a continuous col variable, a scales::rescale function.
#' @param col_title Legend title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param col_trans For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c("value" = "label", ...)).
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of "wrap".
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of "wrap".
#' @param facet_scales Whether facet scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed". Only applies where the facet layout is "grid" and facet scales are not "fixed".
#' @param facet_layout Whether the layout is to be "wrap" or "grid". If NULL and a single facet (or facet2) argument is provided, then defaults to "wrap". If NULL and both facet and facet2 arguments are provided, defaults to "grid".
#' @param facet_switch Whether the facet layout is "grid", whether to switch the facet labels to the opposite side of the plot. Either "x", "y" or "both".
#' @param titles A function to format the x, y and col titles. Defaults to snakecase::to_sentence_case.
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot2::economics |>
#'   gg_line(
#'     x = date,
#'     y = unemploy,
#'     pal = "#9E361B",
#'     y_title = "Unemployment",
#'     y_include = 0
#'   )
#'
gg_line <- function(
    data = NULL,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    text = NULL,
    position = "identity",
    coord = ggplot2::coord_cartesian(clip = "off"),
    pal = NULL,
    pal_na = pal_grey,
    alpha = 1,
    ...,
    title = NULL,
    subtitle = NULL,
    x_breaks = NULL,
    x_expand = NULL,
    x_gridlines = NULL,
    x_include = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_oob = scales::oob_keep,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_trans = "identity",
    y_breaks = NULL,
    y_expand = NULL,
    y_gridlines = NULL,
    y_include = NULL,
    y_labels = NULL,
    y_limits = NULL,
    y_oob = scales::oob_keep,
    y_sec_axis = ggplot2::waiver(),
    y_title = NULL,
    y_trans = "identity",
    col_breaks = NULL,
    col_continuous = "gradient",
    col_include = NULL,
    col_labels = NULL,
    col_legend_place = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_limits = NULL,
    col_oob = scales::oob_keep,
    col_rescale = scales::rescale(),
    col_title = NULL,
    col_trans = "identity",
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    facet_layout = NULL,
    facet_switch = NULL,
    caption = NULL,
    titles = snakecase::to_sentence_case,
    theme = NULL) {

  ##############################################################################
  #Unique code: part 1
  ##############################################################################

  stat <- "identity"

  #quote
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)

  #ungroup
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(
      c(!!x, !!y,
        !!col
      ),
      na_if_inf))

  #get classes
  x_null <- rlang::quo_is_null(x)
  x_forcat <- is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data)) | is.logical(rlang::eval_tidy(x, data))
  x_numeric <- is.numeric(rlang::eval_tidy(x, data))
  x_date <- lubridate::is.Date(rlang::eval_tidy(x, data))
  x_datetime <- lubridate::is.POSIXct(rlang::eval_tidy(x, data))
  x_time <- hms::is_hms(rlang::eval_tidy(x, data))

  y_null <- rlang::quo_is_null(y)
  y_forcat <- is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data)) | is.logical(rlang::eval_tidy(y, data))
  y_numeric <- is.numeric(rlang::eval_tidy(y, data))
  y_date <- lubridate::is.Date(rlang::eval_tidy(y, data))
  y_datetime <- lubridate::is.POSIXct(rlang::eval_tidy(y, data))
  y_time <- hms::is_hms(rlang::eval_tidy(y, data))

  col_null <- rlang::quo_is_null(col)
  col_factor <- is.factor(rlang::eval_tidy(col, data))
  col_forcat <- is.character(rlang::eval_tidy(col, data)) | is.factor(rlang::eval_tidy(col, data)) | is.logical(rlang::eval_tidy(col, data))
  col_numeric <- is.numeric(rlang::eval_tidy(col, data))
  col_date <- lubridate::is.Date(rlang::eval_tidy(col, data))
  col_datetime <- lubridate::is.POSIXct(rlang::eval_tidy(col, data))
  col_time <- hms::is_hms(rlang::eval_tidy(col, data))

  facet_null <- rlang::quo_is_null(facet)
  facet2_null <- rlang::quo_is_null(facet2)

  ##############################################################################
  #Generic code: part 1 (adjust for gg_sf)
  ##############################################################################

  #get default theme if global theme not set
  if (rlang::is_null(theme)) {
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
      theme <- light_mode()
    }
  }

  #process for horizontal
  if (stat != "sf") {
    if (y_forcat) {
      if (!(!col_null &
            (identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data))))) {

        if (is.logical(rlang::eval_tidy(y, data))) {
          data <- data %>%
            dplyr::mutate(dplyr::across(!!y, function(x) as.character(x)))
        }

        data <- data %>%
          dplyr::mutate(dplyr::across(!!y, function(x) forcats::fct_rev(x)))
      }
    }

    if (col_forcat) {
      if (y_forcat) {
        if (is.logical(rlang::eval_tidy(col, data))) {
          data <- data %>%
            dplyr::mutate(dplyr::across(!!col, function(x) as.character(x)))
        }

        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, function(x) forcats::fct_rev(x)))
      }
    }
  }

  ##############################################################################
  #Unique code: part 2
  ##############################################################################

  ###make plot
  if (!x_null & !y_null) {
    if (!col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          # col = "",
          # fill = "",
          group = !!group
        ))
    }
  }
  else if (!x_null & y_null) {
    if (!col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          # col = "",
          # fill = "",
          group = !!group
        ))
    }
  }
  else if (x_null & !y_null) {
    if (!col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          # col = "",
          # fill = "",
          group = !!group
        ))
    }
  }
  else if (x_null & y_null) {
    if (!col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (col_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          # col = "",
          # fill = "",
          group = !!group
        ))
    }
  }

  if (col_null & !stat %in% c("bin2d", "bin_2d", "binhex", "contour_filled", "density2d_filled", "density_2d_filled")) {
    if (rlang::is_null(pal)) pal <-  pal_blue
    else pal <- as.vector(pal[1])

    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(text = !!text), stat = stat,
        position = position,
        alpha = alpha,
        col = pal,
        fill = pal,
        ...
      ) +
      coord +
      theme
  }
  else {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(text = !!text), stat = stat,
        position = position,
        alpha = alpha,
        ...
      ) +
      coord +
      theme
  }

  ##############################################################################
  #Generic code: part 2 (adjust for gg_sf)
  ##############################################################################

  #Add faceting
  if (rlang::is_null(facet_layout)) {
    if (!facet_null & facet2_null) facet_layout <- "wrap"
    else if (!facet2_null & facet_null) facet_layout <- "wrap"
    else if (!facet_null & !facet2_null) facet_layout <- "grid"
    else if (facet_null & facet2_null) facet_layout <- "grid"
    else facet_layout <- "null"
  }

  if (facet_layout == "wrap") {
    if (!facet_null & facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet),
          scales = facet_scales, drop = FALSE,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet2),
          scales = facet_scales, drop = FALSE,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet, !!facet2),
          scales = facet_scales, drop = FALSE,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }
  else if (facet_layout == "grid") {
    if (!facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            rows = ggplot2::vars(!!facet2),
                            cols = ggplot2::vars(!!facet),
                            scales = facet_scales, drop = FALSE,
                            space = facet_space,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            cols = ggplot2::vars(!!facet),
                            scales = facet_scales, drop = FALSE,
                            space = facet_space,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            rows = ggplot2::vars(!!facet2),
                            scales = facet_scales, drop = FALSE,
                            space = facet_space,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  #Get the positional scales right first
  if (stat != "sf") {
    if (x_numeric) {
      if (any(x_trans %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits[c(2, 1)], oob = x_oob)
      } else {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_date) {
      if (any(x_trans %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits[c(2, 1)], oob = x_oob)
      } else {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_datetime) {
      if (any(x_trans %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits[c(2, 1)], oob = x_oob)
      } else {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_time) {
      if (any(x_trans %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_time(limits = x_limits[c(2, 1)], oob = x_oob)
      } else {
        plot <- plot +
          ggplot2::scale_x_time(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_forcat) {
      if (any(x_trans %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_discrete(limits = x_limits[c(2, 1)])
      } else {
        plot <- plot +
          ggplot2::scale_x_discrete(limits = x_limits)
      }
    }

    if (!rlang::is_null(x_include)) {
      plot <- plot +
        ggplot2::expand_limits(x = x_include)
    }

    if (y_numeric) {
      if (any(y_trans %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits[c(2, 1)], oob = y_oob)
      } else {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_date) {
      if (any(y_trans %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits[c(2, 1)], oob = y_oob)
      } else {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_datetime) {
      if (any(y_trans %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits[c(2, 1)], oob = y_oob)
      } else {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_time) {
      if (any(y_trans %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_time(limits = y_limits[c(2, 1)], oob = y_oob)
      } else {
        plot <- plot +
          ggplot2::scale_y_time(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_forcat) {
      if (any(y_trans %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_discrete(limits = y_limits[c(2, 1)])
      } else {
        plot <- plot +
          ggplot2::scale_y_discrete(limits = y_limits)
      }
      if (!rlang::is_null(y_include)) {
        plot <- plot +
          ggplot2::expand_limits(y = y_include)
      }
    }
  }

  #Get plot data and flipped status
  plot_build <- ggplot2::ggplot_build(plot)
  plot_data <- plot_build$data[[1]]

  flippable <- any(stringr::str_detect(colnames(plot_data), "flipped_aes"))

  if (flippable) {
    flipped <- all(plot_data["flipped_aes"])
  }
  else if (y_forcat & (x_null | x_numeric | x_date | x_datetime | x_time)) {
    flipped <- TRUE
  }
  else flipped <- FALSE

  #Make x, y scales
  if (stat != "sf") {
    #Make x scale based on plot_data
    if (x_forcat) {
      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
      if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

      plot <- plot +
        ggplot2::scale_x_discrete(
          expand = x_expand,
          labels = x_labels,
          breaks = x_breaks,
          limits = x_limits,
          drop = FALSE
        )
    }
    else if (x_numeric | x_date | x_datetime | x_time | x_null) {

      if (class(position)[1] == "character") {
        if (position == "fill") {
          if (flipped) x_limits <- c(NA, NA)
        }
      }
      else if (class(position)[1] == "PositionFill") {
        if (flipped) x_limits <- c(NA, NA)
      }

      if (facet_scales %in% c("fixed", "free_y")) {

        x_vars_str <- "^x$|^xmin$|^xmax$|^xend$|^xmin_final$|^xmax_final$"
        y_vars_str <- "^y$|^ymin$|^ymax$|^yend$|^ymin_final$|^ymax_final$"

        x_vctr_temp <- plot_data %>%
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(x_vars_str)), \(x) !rlang::is_na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(x_vars_str)))

        if (ncol(x_vctr_temp) != 0) {
          if (stringr::str_detect(stat, "bin")) {
            x_vctr <- x_vctr_temp %>%
              dplyr::select(tidyselect::matches(stringr::regex("^x$"))) %>%
              tidyr::pivot_longer(cols = tidyselect::everything()) %>%
              dplyr::pull(.data$value)
          }
          else {
            x_vctr <- x_vctr_temp %>%
              tidyr::pivot_longer(cols = tidyselect::everything()) %>%
              dplyr::pull(.data$value)
          }
          if (rlang::is_null(x_include)) {
            if (any(x_trans %in% "reverse")) {
              x_vctr <- c(x_vctr, x_include * -1)
            }
            else {
              x_vctr <- c(x_vctr, x_include)
            }
          }
        } else {
          x_vctr <- NULL
        }

        if (x_date) {
          x_vctr <- lubridate::as_date(x_vctr, origin = "1970-01-01")
        }
        else if (x_datetime) {
          x_vctr <- lubridate::as_datetime(x_vctr, origin = "1970-01-01")
        }
        else if (x_time) {
          x_vctr <- hms::as_hms(x_vctr)
        }

        x_range <- range(x_vctr, na.rm = TRUE)
        if (!rlang::is_null(x_include)) x_range <- range(c(x_range, x_include))
        if (any(x_trans %in% "reverse")) x_range <- sort(x_range, decreasing = TRUE)

        if (rlang::is_null(x_limits)) {
          if (rlang::is_null(x_breaks)) {

            if (!facet_null & !facet2_null) x_breaks_n <- 3
            else if (!facet_null & facet2_null) x_breaks_n <- 3
            else x_breaks_n <- 6

            if (x_time) x_breaks <- ggplot2::waiver()
            else if (any(x_trans == "log10")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)(x_range)
            else if (any(x_trans == "log2")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 2)(x_range)
            else if (any(x_trans == "log")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = exp(1))(x_range)
            else x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)

            if (!flipped) x_limits <- x_range
            else if (any(!x_trans %in% c("identity", "reverse"))) x_limits <- x_range
            else x_limits <- x_breaks[c(1, length(x_breaks))]
          }
          else if (!(rlang::is_null(x_breaks))) {
            if (!flipped) x_limits <- x_range
            else {
              if (any(!x_trans %in% c("identity", "reverse"))) {
                x_limits <- x_range
              } else {
                if (is.function(x_breaks)) x_breaks <- x_breaks(x_range)
                if (is.vector(x_breaks)) {
                  x_limits <- x_breaks[c(1, length(x_breaks))]
                  if (!rlang::is_null(x_include)) x_limits <- range(x_limits, x_include)
                }
              }
            }
          }
          if (any(x_trans %in% "reverse")) x_limits <- sort(x_limits, decreasing = TRUE)
        }
        else if (!rlang::is_null(x_limits)) {
          if (rlang::is_na(x_limits[1])) {
            x_limits[1] <- x_range[1]
          }
          if (rlang::is_na(x_limits[2])) {
            x_limits[2] <- x_range[2]
          }

          if (!rlang::is_null(x_include)) x_limits <- range(c(x_limits, x_include))

          if (any(x_trans %in% "reverse")) x_limits <- sort(x_limits, decreasing = TRUE)

          if (rlang::is_null(x_breaks)) {

            if (!facet_null & !facet2_null) x_breaks_n <- 3
            else if (!facet_null & facet2_null) x_breaks_n <- 3
            else x_breaks_n <- 6

            if (x_time) x_breaks <- ggplot2::waiver
            else if (any(x_trans == "log10")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)(x_limits)
            else if (any(x_trans == "log2")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 2)(x_limits)
            else if (any(x_trans == "log")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = exp(1))(x_limits)
            else x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_limits)
          }
        }
      }
      else if (facet_scales %in% c("free", "free_x")) {
        if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
      }

      if (rlang::is_null(x_expand)) {
        if (any(!x_trans %in% c("identity", "reverse"))) {
          x_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else if (flipped) x_expand <- c(0, 0)
        else if (facet_scales %in% c("fixed", "free_y") &
                 (y_date | y_datetime | y_time | y_numeric | y_null)) {
          x_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else x_expand <- c(0, 0)
      }

      if (rlang::is_null(x_labels)) {
        if (x_numeric | x_null) {
          if (any(x_trans == "log10")) x_labels <- scales::label_log(base = 10)
          else if (any(x_trans == "log2")) x_labels <- scales::label_log(base = 2)
          else if (any(x_trans == "log")) x_labels <- scales::label_math("e"^.x, format = log)
          else x_labels <- scales::label_comma(drop0trailing = TRUE)
        }
        else if (x_date | x_datetime) {
          x_labels <- scales::label_date_short(format = c("%Y", "%b", "%e", "%H:%M"))
        }
        else if (x_time) {
          x_labels <- scales::label_time()
        }
      }

      if (x_numeric | x_null) {
        plot <- plot +
          ggplot2::scale_x_continuous(
            breaks = x_breaks,
            limits = x_limits,
            expand = x_expand,
            labels = x_labels,
            oob = x_oob,
            sec.axis = x_sec_axis,
            trans = x_trans
          )
      }
      else if (x_date) {
        plot <- plot +
          ggplot2::scale_x_date(
            breaks = x_breaks,
            limits = x_limits,
            expand = x_expand,
            labels = x_labels,
            oob = x_oob,
            sec.axis = x_sec_axis
          )
      }
      else if (x_datetime) {
        plot <- plot +
          ggplot2::scale_x_datetime(
            breaks = x_breaks,
            limits = x_limits,
            expand = x_expand,
            labels = x_labels,
            oob = x_oob,
            sec.axis = x_sec_axis
          )
      }
      else if (x_time) {
        plot <- plot +
          ggplot2::scale_x_time(
            breaks = x_breaks,
            limits = x_limits,
            expand = x_expand,
            labels = x_labels,
            oob = x_oob,
            sec.axis = x_sec_axis
          )
      }
    }

    #Make y scale based on plot_data
    if (y_forcat) {
      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
      if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
      if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

      plot <- plot +
        ggplot2::scale_y_discrete(
          expand = y_expand,
          labels = y_labels,
          breaks = y_breaks,
          drop = FALSE
        )
    }
    else if (y_numeric | y_date | y_datetime | y_time | y_null) {

      if (class(position)[1] == "character") {
        if (position == "fill") {
          if (!flipped) y_limits <- c(NA, NA)
        }
      }
      else if (class(position)[1] == "PositionFill") {
        if (!flipped) y_limits <- c(NA, NA)
      }

      if (facet_scales %in% c("fixed", "free_x")) {

        x_vars_str <- "^x$|^xmin$|^xmax$|^xend$|^xmin_final$|^xmax_final$"
        y_vars_str <- "^y$|^ymin$|^ymax$|^yend$|^ymin_final$|^ymax_final$"

        y_vctr_temp <- plot_data %>%
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(y_vars_str)), \(x) !rlang::is_na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(y_vars_str)))

        if (ncol(y_vctr_temp) != 0) {
          if (stringr::str_detect(stat, "bin")) {
            y_vctr <- y_vctr_temp %>%
              dplyr::select(tidyselect::matches(stringr::regex("^y$"))) %>%
              tidyr::pivot_longer(cols = tidyselect::everything()) %>%
              dplyr::pull(.data$value)
          }
          else {
            y_vctr <- y_vctr_temp %>%
              tidyr::pivot_longer(cols = tidyselect::everything()) %>%
              dplyr::pull(.data$value)
          }
          if (rlang::is_null(y_include)) {
            if (any(y_trans %in% "reverse")) {
              y_vctr <- c(y_vctr, y_include * -1)
            }
            else {
              y_vctr <- c(y_vctr, y_include)
            }
          }
        } else {
          y_vctr <- NULL
        }

        if (y_date) {
          y_vctr <- lubridate::as_date(y_vctr, origin = "1970-01-01")
        }
        else if (y_datetime) {
          y_vctr <- lubridate::as_datetime(y_vctr, origin = "1970-01-01")
        }
        else if (y_time) {
          y_vctr <- hms::as_hms(y_vctr)
        }

        y_range <- range(y_vctr, na.rm = TRUE)
        if (!rlang::is_null(y_include)) y_range <- range(c(y_range, y_include))
        if (any(y_trans %in% "reverse")) y_range <- sort(y_range, decreasing = TRUE)

        if (rlang::is_null(y_limits)) {
          if (rlang::is_null(y_breaks)) {

            if (!facet_null & !facet2_null) y_breaks_n <- 6
            else if (facet_null & !facet2_null) y_breaks_n <- 6
            else y_breaks_n <- 8

            if (y_time) y_breaks <- ggplot2::waiver
            else if (any(y_trans == "log10")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)(y_range)
            else if (any(y_trans == "log2")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 2)(y_range)
            else if (any(y_trans == "log")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = exp(1))(y_range)
            else y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)

            if (flipped) y_limits <- y_range
            else if (any(!y_trans %in% c("identity", "reverse"))) y_limits <- y_range
            else y_limits <- y_breaks[c(1, length(y_breaks))]
          }
          else if (!(rlang::is_null(y_breaks))) {
            if (flipped) y_limits <- y_range
            else {
              if (any(!y_trans %in% c("identity", "reverse"))) {
                y_limits <- y_range
              } else {
                if (is.function(y_breaks)) y_breaks <- y_breaks(y_range)
                if (is.vector(y_breaks)) {
                  y_limits <- y_breaks[c(1, length(y_breaks))]
                  if (!rlang::is_null(y_include)) y_limits <- range(y_limits, y_include)
                }
              }
            }
          }
          if (any(y_trans %in% "reverse")) y_limits <- sort(y_limits, decreasing = TRUE)
        }
        else if (!rlang::is_null(y_limits)) {
          if (rlang::is_na(y_limits[1])) y_limits[1] <- y_range[1]
          if (rlang::is_na(y_limits[2])) y_limits[2] <- y_range[2]

          if (!rlang::is_null(y_include)) y_limits <- range(c(y_limits, y_include))

          if (any(y_trans %in% "reverse")) y_limits <- sort(y_limits, decreasing = TRUE)

          if (rlang::is_null(y_breaks)) {

            if (!facet_null & !facet2_null) y_breaks_n <- 6
            else if (facet_null & !facet2_null) y_breaks_n <- 6
            else y_breaks_n <- 8

            if (y_time) y_breaks <- ggplot2::waiver
            else if (any(y_trans == "log10")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)(y_limits)
            else if (any(y_trans == "log2")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 2)(y_limits)
            else if (any(y_trans == "log")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = exp(1))(y_limits)
            else y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_limits)
          }
        }
      }
      else if (facet_scales %in% c("free", "free_y")) {
        if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
      }

      if (rlang::is_null(y_expand)) {
        if (any(!y_trans %in% c("identity", "reverse"))) {
          y_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else if (!flipped) y_expand <- c(0, 0)
        else if (facet_scales %in% c("fixed", "free_x") &
                 (y_date | y_datetime | y_time | y_numeric | y_null)) {
          y_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else y_expand <- c(0, 0)
      }

      if (rlang::is_null(y_labels)) {
        if (y_numeric | y_null) {
          if (any(y_trans == "log10")) y_labels <- scales::label_log(base = 10)
          else if (any(y_trans == "log2")) y_labels <- scales::label_log(base = 2)
          else if (any(y_trans == "log")) y_labels <- scales::label_math("e"^.x, format = log)
          else y_labels <- scales::label_comma(drop0trailing = TRUE)
        }
        else if (y_date | y_datetime) {
          y_labels <- scales::label_date_short(format = c("%Y", "%b", "%e", "%H:%M"))
        }
        else if (y_time) {
          y_labels <- scales::label_time()
        }
      }

      if (y_numeric | y_null) {
        plot <- plot +
          ggplot2::scale_y_continuous(
            breaks = y_breaks,
            limits = y_limits,
            expand = y_expand,
            labels = y_labels,
            oob = y_oob,
            sec.axis = y_sec_axis,
            trans = y_trans
          )
      }
      else if (y_date) {
        plot <- plot +
          ggplot2::scale_y_date(
            breaks = y_breaks,
            limits = y_limits,
            expand = y_expand,
            labels = y_labels,
            oob = y_oob,
            sec.axis = y_sec_axis
          )
      }
      else if (y_datetime) {
        plot <- plot +
          ggplot2::scale_y_datetime(
            breaks = y_breaks,
            limits = y_limits,
            expand = y_expand,
            labels = y_labels,
            oob = y_oob,
            sec.axis = y_sec_axis
          )
      }
      else if (y_time) {
        plot <- plot +
          ggplot2::scale_y_time(
            breaks = y_breaks,
            limits = y_limits,
            expand = y_expand,
            labels = y_labels,
            oob = y_oob,
            sec.axis = y_sec_axis
          )
      }
    }
  }

  #make col scale
  if (!col_null | stat %in% c("bin2d", "bin_2d", "binhex", "contour_filled", "density2d_filled", "density_2d_filled")) {
    if (stat %in% c("bin2d", "bin_2d", "binhex", "contour_filled", "density2d_filled", "density_2d_filled")) {
      if (!rlang::is_null(plot_build$plot$labels$fill)) {
        col_vctr <- dplyr::pull(plot_data, rlang::as_name(plot_build$plot$labels$fill[1]))
      }
      else if (!rlang::is_null(plot_build$plot$labels$colour)) {
        col_vctr <- dplyr::pull(plot_data, rlang::as_name(plot_build$plot$labels$colour[1]))
      }

    }
    else if (!col_null) {
      col_vctr <- data %>%
        dplyr::pull(!!col)
    }

    if (rlang::is_null(col_legend_place)) {
      if (col_numeric | col_date | col_datetime | col_time) col_legend_place <- "right"
      else if (stat %in% c("bin2d", "binhex", "density2d_filled", "density_2d_filled", "contour_filled")) col_legend_place <- "right"
      else col_legend_place <- "bottom"
    }

    if (col_legend_place == "b") col_legend_place <- "bottom"
    if (col_legend_place == "t") col_legend_place <- "top"
    if (col_legend_place == "l") col_legend_place <- "left"
    if (col_legend_place == "r") col_legend_place <- "right"
    if (col_legend_place == "n") col_legend_place <- "none"

    if (col_forcat | stat %in% c("contour_filled", "density2d_filled", "density_2d_filled")) {
      if (!rlang::is_null(col_limits)) col_n <- length(col_limits)
      else if (!rlang::is_null(col_breaks)) col_n <- length(col_breaks)
      else {
        if (col_factor) col_n <- length(levels(col_vctr))
        else {
          col_unique <- unique(col_vctr)
          col_n <- length(col_unique[!rlang::is_na(col_unique)])
        }
      }
      if (rlang::is_null(pal)) {
        if (stat %in% c("contour_filled", "density2d_filled", "density_2d_filled")) pal <- viridis::viridis(col_n)
        else pal <- pal_discrete[1:col_n]
      }
      else if (rlang::is_null(names(pal))) pal <- pal[1:col_n]

      if (y_numeric | y_date | y_datetime | y_time) {
        if (col_forcat) col_legend_rev_auto <- FALSE
        else if (col_legend_place %in% c("top", "bottom")) col_legend_rev_auto <- FALSE
        else col_legend_rev_auto <- TRUE
      }
      else if (y_forcat) {
        if (col_forcat) col_legend_rev_auto <- TRUE
        else if (col_legend_place %in% c("top", "bottom")) col_legend_rev_auto <- TRUE
        else col_legend_rev_auto <- FALSE
        pal <- rev(pal)
      }
      else col_legend_rev_auto <- FALSE

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

      if (col_legend_rev) col_legend_rev_auto <- !col_legend_rev_auto

      plot <- plot +
        ggplot2::scale_colour_manual(
          values = pal, drop = FALSE,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = as.vector(pal_na),
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev_auto,
            title.position = "top",
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        ) +
        ggplot2::scale_fill_manual(
          values = pal, drop = FALSE,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = as.vector(pal_na),
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev_auto,
            title.position = "top",
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        )
    }
    else {
      if (rlang::is_null(col_trans)) {
        if (col_date) col_trans <- "date"
        else if (col_datetime) col_trans <- "time"
        else if (col_time) col_trans <- "hms"
        else col_trans <- "identity"
      }

      if (col_time) col_breaks <- ggplot2::waiver()
      else if (any(col_trans == "log10")) col_breaks <- scales::breaks_log(n = 5, base = 10)
      else if (any(col_trans == "log2")) col_breaks <- scales::breaks_log(n = 5, base = 2)
      else if (any(col_trans == "log")) col_breaks <- scales::breaks_log(n = 5, base = exp(1))
      else col_breaks <- scales::breaks_pretty(n = 5)

      if (rlang::is_null(pal)) pal <- viridis::viridis(10)

      if (rlang::is_null(col_labels)) {
        if (col_numeric | col_null) {
          if (any(col_trans == "log10")) col_labels <- scales::label_log(base = 10)
          else if (any(col_trans == "log2")) col_labels <- scales::label_log(base = 2)
          else if (any(col_trans == "log")) col_labels <- scales::label_math("e"^.x, format = log)
          else col_labels <- scales::label_comma(drop0trailing = TRUE)
        }
        else if (col_date | col_datetime) {
          col_labels <- scales::label_date(format = c("%Y", "%b", "%e"))
        }
        else if (col_time) {
          col_labels <- scales::label_time()
        }
      }

      if (col_continuous == "gradient") {
        plot <- plot +
          ggplot2::scale_colour_gradientn(
            colours = pal,
            values = col_rescale,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            trans = col_trans,
            na.value = as.vector(pal_na),
            guide = ggplot2::guide_colourbar(
              title.position = "top",
              draw.ulim = TRUE,
              draw.llim = TRUE,
              ticks.colour = "#F1F3F5",
              reverse = col_legend_rev
            )
          ) +
          ggplot2::scale_fill_gradientn(
            colours = pal,
            values = col_rescale,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            trans = col_trans,
            na.value = as.vector(pal_na),
            guide = ggplot2::guide_colourbar(
              title.position = "top",
              draw.ulim = TRUE,
              draw.llim = TRUE,
              ticks.colour = "#F1F3F5",
              reverse = col_legend_rev
            )
          )
      }
      else if (col_continuous == "steps") {
        plot <- plot +
          ggplot2::scale_colour_stepsn(
            colours = pal,
            values = col_rescale,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            trans = col_trans,
            oob = col_oob,
            na.value = as.vector(pal_na),
            guide = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev)
          ) +
          ggplot2::scale_fill_stepsn(
            colours = pal,
            values = col_rescale,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            trans = col_trans,
            oob = col_oob,
            na.value = as.vector(pal_na),
            guide = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev)
          )
      }
    }
  }

  #Add titles
  if (rlang::is_null(x_title)) {
    if (!rlang::is_null(plot_build$plot$labels$x)) {
      x_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), titles)
    }
  }
  if (rlang::is_null(y_title)) {
    if (!rlang::is_null(plot_build$plot$labels$y)) {
      y_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$y[1]), titles)
    }
  }
  if (rlang::is_null(col_title)) {
    if (!rlang::is_null(plot_build$plot$labels$fill)) {
      col_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$fill[1]), titles)
    }
    else if (!rlang::is_null(plot_build$plot$labels$colour)) {
      col_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$colour[1]), titles)
    }

  }

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption)

  if (!col_null | stat %in% c("bin2d", "bin_2d", "binhex", "contour_filled", "density2d_filled", "density_2d_filled")) {
    plot <- plot +
      ggplot2::labs(
        col = col_title,
        fill = col_title)
  }

  if (stat != "sf") {
    plot <- plot +
      ggplot2::labs(
        x = x_title,
        y = y_title)

    if (!rlang::is_null(x_title)) {
      if (x_title == "") {
        plot <- plot +
          ggplot2::labs(x = NULL)
      }
    }

    if (!rlang::is_null(y_title)) {
      if (y_title == "") {
        plot <- plot +
          ggplot2::labs(y = NULL)
      }
    }
  }

  if (!rlang::is_null(col_title)) {
    if (col_title == "") {
      plot <- plot +
        ggplot2::labs(colour = NULL, fill = NULL)
    }
  }

  #expand limits if necessary
  if (stat != "sf") {
    if (!rlang::is_null(x_include)) {
      plot <- plot +
        ggplot2::expand_limits(x = x_include)
    }
    if (!rlang::is_null(y_include)) {
      plot <- plot +
        ggplot2::expand_limits(y = y_include)
    }
  }
  if (!rlang::is_null(col_include)) {
    plot <- plot +
      ggplot2::expand_limits(colour = col_include, fill = col_include)
  }

  #Adjust legend
  if (!rlang::is_null(col_legend_place)){
    if (col_legend_place %in% c("top", "bottom")) {
      plot <- plot +
        ggplot2::theme(legend.position = col_legend_place) +
        ggplot2::theme(legend.direction = "horizontal") +
        ggplot2::theme(legend.justification = "left") +
        ggplot2::theme(legend.box.margin = ggplot2::margin(t = -2.5)) +
        ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 7.5))) +
        ggplot2::theme(legend.title = ggplot2::element_text(margin = ggplot2::margin(t = 5)))

      if (col_numeric | stat %in% c("bin2d", "bin_2d", "binhex")) {
        plot <- plot +
          ggplot2::theme(legend.key.width = grid::unit(0.66, "cm")) +
          ggplot2::theme(legend.text.align = 0.5)
      }
    }
    else if (col_legend_place %in% c("left", "right")) {
      plot <- plot +
        ggplot2::theme(legend.position = col_legend_place) +
        ggplot2::theme(legend.direction = "vertical") +
        ggplot2::theme(legend.justification = "left")
    }
    else if (col_legend_place == "none") {
      plot <- plot +
        ggplot2::guides(col = "none", fill = "none")
    }
  }

  #remove gridlines as per x_gridlines and y_gridlines. Guess if NULL
  if (rlang::is_null(x_gridlines)) {
    if (stat == "sf") x_gridlines <- FALSE
    else if ((y_numeric | y_date | y_datetime | y_time) & (x_null)) x_gridlines <- TRUE
    else if ((y_forcat) & (x_numeric | x_null)) x_gridlines <- TRUE
    else if ((y_forcat) & (x_forcat)) x_gridlines <- FALSE
    else x_gridlines <- FALSE
  }

  if (rlang::is_null(y_gridlines)) {
    if (stat == "sf") y_gridlines <- FALSE
    else if ((y_numeric | y_date | y_datetime | y_time) & (x_null)) y_gridlines <- FALSE
    else if ((y_forcat) & (x_numeric | x_null)) y_gridlines <- FALSE
    else if ((y_forcat) & (x_forcat)) y_gridlines <- FALSE
    else y_gridlines <- TRUE
  }

  if (!x_gridlines & !y_gridlines) {
    plot <- plot + #resolve ggplot2 issue #4730
      ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  else {
    if (!x_gridlines) {
      plot <- plot +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    }
    if (!y_gridlines) {
      plot <- plot +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
        ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  }

  #return beautiful plot
  return(plot)
}

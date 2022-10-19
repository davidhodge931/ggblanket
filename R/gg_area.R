#' @title Area ggplot
#'
#' @description Create a area ggplot with a wrapper around the ggplot2::geom_area function.
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable.
#' @param group Unquoted group aesthetic variable.
#' @param text Unquoted text aesthetic variable, which can be used in combination with plotly::ggplotly(., tooltip = "text").
#' @param stat Statistical transformation. A character string (e.g. "identity").
#' @param position Position adjustment. Either a character string (e.g."identity"), or a function (e.g. ggplot2::position_identity()).
#' @param pal Colours to use. A character vector of hex codes (or names).
#' @param pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1.
#' @param ... Other arguments passed to the relevant ggplot2::geom_* function.
#' @param titles A function to format the x, y and col titles. Defaults to snakecase::to_sentence_case.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param x_breaks A function on the limits (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_grid TRUE or FALSE for vertical x gridlines. NULL guesses based on the classes of the x and y.
#' @param x_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis. Alternatively, zoom in using coord = coord_cartesian(xlim = ...).
#' @param x_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param x_trans For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param y_breaks A function on the limits (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_grid TRUE or FALSE of horizontal y gridlines. NULL guesses based on the classes of the x and y.
#' @param y_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis. Alternatively, zoom in using coord = coord_cartesian(ylim = ...).
#' @param y_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param y_trans For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param col_breaks A function on the limits (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_continuous Type of colouring for a continuous variable. Either "gradient" or "steps". Defaults to "steps" - or just the first letter of these e.g. "g".
#' @param col_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels. Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector to determine the limits of the colour scale.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. Either "bottom", "right", "top" or "left" - or just the first letter of these e.g. "b".
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_rescale For a continuous col variable, a vector to rescale the pal non-linearly.
#' @param col_title Legend title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param col_trans For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c("value" = "label", ...)).
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of "wrap".
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of "wrap".
#' @param facet_scales Whether facet scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed". Only applies where the facet layout is "grid" and facet scales are not "fixed".
#' @param facet_layout Whether the layout is to be "wrap" or "grid". If NULL and a single facet (or facet2) argument is provided, then defaults to "wrap". If NULL and both facet and facet2 arguments are provided, defaults to "grid".
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#'
#' huron %>%
#'   gg_area(
#'     x = year,
#'     y = level,
#'     x_labels = ~.x)
#'
#' huron %>%
#'   gg_area(
#'     y = year,
#'     x = level,
#'     x_labels = ~.x,
#'     orientation = "y")
#'
gg_area <- function(
    data = NULL,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    text = NULL,
    stat = "identity",
    position = "stack",
    pal = NULL,
    pal_na = "#7F7F7F",
    alpha = 0.9,
    ...,
    titles = NULL,
    title = NULL,
    subtitle = NULL,
    coord = ggplot2::coord_cartesian(clip = "off"),
    x_breaks = NULL,
    x_expand = NULL,
    x_grid = NULL,
    x_include = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_trans = "identity",
    y_breaks = NULL,
    y_expand = NULL,
    y_grid = NULL,
    y_include = NULL,
    y_labels = NULL,
    y_limits = NULL,
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
    col_rescale = NULL,
    col_title = NULL,
    col_trans = "identity",
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    facet_layout = NULL,
    caption = NULL,
    theme = gg_theme()) {

  #quote
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)

  #stop, warn or message
  rlang::inform(c("i" = "For further ggblanket information, see https://davidhodge931.github.io/ggblanket/"), .frequency = "regularly", .frequency_id = "hello")
  if (rlang::is_null(data)) rlang::abort("data is required.")

  #ungroup
  data <- dplyr::ungroup(data)

  #get classes
  x_null <- rlang::quo_is_null(x)
  x_character <- is.character(rlang::eval_tidy(x, data))
  x_factor <- is.factor(rlang::eval_tidy(x, data))
  x_logical <- is.logical(rlang::eval_tidy(x, data))
  x_numeric <- is.numeric(rlang::eval_tidy(x, data))
  x_date <- lubridate::is.Date(rlang::eval_tidy(x, data))
  x_datetime <- lubridate::is.POSIXct(rlang::eval_tidy(x, data))
  x_time <- hms::is_hms(rlang::eval_tidy(x, data))

  y_null <- rlang::quo_is_null(y)
  y_character <- is.character(rlang::eval_tidy(y, data))
  y_factor <- is.factor(rlang::eval_tidy(y, data))
  y_logical <- is.logical(rlang::eval_tidy(y, data))
  y_numeric <- is.numeric(rlang::eval_tidy(y, data))
  y_date <- lubridate::is.Date(rlang::eval_tidy(y, data))
  y_datetime <- lubridate::is.POSIXct(rlang::eval_tidy(y, data))
  y_time <- hms::is_hms(rlang::eval_tidy(y, data))

  col_null <- rlang::quo_is_null(col)
  col_character <- is.character(rlang::eval_tidy(col, data))
  col_factor <- is.factor(rlang::eval_tidy(col, data))
  col_logical <- is.logical(rlang::eval_tidy(col, data))
  col_numeric <- is.numeric(rlang::eval_tidy(col, data))
  col_date <- lubridate::is.Date(rlang::eval_tidy(col, data))
  col_datetime <- lubridate::is.POSIXct(rlang::eval_tidy(col, data))
  col_time <- hms::is_hms(rlang::eval_tidy(col, data))

  facet_null <- rlang::quo_is_null(facet)

  facet2_null <- rlang::quo_is_null(facet2)

  #process data for logical & horizontal
  if (!x_null) {
    if (x_logical) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x, ~ factor(stringr::str_to_sentence(.x), levels = c("False", "True"))))
    }
  }
  if (!y_null) {
    if (y_logical) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y, ~ factor(stringr::str_to_sentence(.x), levels = c("False", "True"))))
    }
    if (y_character | y_factor | y_logical) {
      if (!col_null &
          (identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data)))) {
      }
      else {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y, ~ forcats::fct_rev(.x)))
      }
    }
  }
  if (!col_null) {
    if (col_logical) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(stringr::str_to_sentence(.x), levels = c("False", "True"))))
    }
    if (col_character | col_factor | col_logical) {
      if (y_character | y_factor | y_logical) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, ~ forcats::fct_rev(.x)))
      }
    }
  }
  if (!facet_null) {
    if (is.logical(class(rlang::eval_tidy(facet, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, ~ factor(stringr::str_to_sentence(.x), levels = c("False", "True"))))
    }
  }
  if (!facet2_null) {
    if (is.logical(class(rlang::eval_tidy(facet2, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet2, ~ factor(stringr::str_to_sentence(.x), levels = c("False", "True"))))
    }
  }

  #get default NULL values
  if (x_null) {
    if (rlang::is_null(x_title)) {
      if (stat %in% c("bin", "count")) x_name <- "count"
      else if (stat %in% c("density", "ydensity")) x_name <- "density"
      else if (stat == "function") x_name <- "x"
      else if (stat == "qq") x_name <- "theoretical"
      else x_name <- ""

      if (rlang::is_null(titles)) x_title <- purrr::map_chr(x_name, snakecase::to_sentence_case)
      else x_title <- purrr::map_chr(x_name, titles)
    }
  }
  else if (rlang::is_null(x_title)) {
    if (rlang::is_null(titles)) x_title <- purrr::map_chr(rlang::as_name(x), snakecase::to_sentence_case)
    else x_title <- purrr::map_chr(rlang::as_name(x), titles)
  }

  if (y_null) {
    if (rlang::is_null(y_title)) {
      if (stat %in% c("bin", "count")) y_name <- "count"
      else if (stat %in% c("density", "ydensity")) y_name <- "density"
      else if (stat == "function") y_name <- "y"
      else if (stat == "qq") y_name <- "sample"
      else y_name <- ""

      if (rlang::is_null(titles)) y_title <- purrr::map_chr(y_name, snakecase::to_sentence_case)
      else y_title <- purrr::map_chr(y_name, titles)
    }
  }
  else if (rlang::is_null(y_title)) {
    if (rlang::is_null(titles)) y_title <- purrr::map_chr(rlang::as_name(y), snakecase::to_sentence_case)
    else y_title <- purrr::map_chr(rlang::as_name(y), titles)
  }

  if ((y_numeric | y_date | y_datetime | y_time) & (x_null)) {
    if (rlang::is_null(x_grid)) x_grid <- TRUE
    if (rlang::is_null(y_grid)) y_grid <- FALSE
  }
  else if ((y_character | y_factor | y_logical) & (x_numeric | x_null)) {
    if (rlang::is_null(x_grid)) x_grid <- TRUE
    if (rlang::is_null(y_grid)) y_grid <- FALSE
  }
  else if ((y_character | y_factor | y_logical) & (x_character | x_factor | x_logical)) {
    if (rlang::is_null(x_grid)) x_grid <- FALSE
    if (rlang::is_null(y_grid)) y_grid <- FALSE
  }
  else {
    if (rlang::is_null(x_grid)) x_grid <- FALSE
    if (rlang::is_null(y_grid)) y_grid <- TRUE
  }

  if (!rlang::is_null(col_legend_place)) {
    if (col_legend_place == "b") col_legend_place <- "bottom"
    if (col_legend_place == "t") col_legend_place <- "top"
    if (col_legend_place == "l") col_legend_place <- "left"
    if (col_legend_place == "r") col_legend_place <- "right"
    if (col_legend_place == "n") col_legend_place <- "none"
  }

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
          col = "",
          fill = "",
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
          col = "",
          fill = "",
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
          col = "",
          fill = "",
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
          col = "",
          fill = "",
          group = !!group
        ))
    }
  }

  plot <- plot +
    ggplot2::geom_area(
      ggplot2::aes(text = !!text),
      stat = stat,
      position = position,
      alpha = alpha,
      ...
    )

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
          scales = facet_scales,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet2),
          scales = facet_scales,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet, !!facet2),
          scales = facet_scales,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }
  else if (facet_layout == "grid") {
    if (!facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet2),
          cols = ggplot2::vars(!!facet),
          scales = facet_scales,
          space = facet_space,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!facet),
          scales = facet_scales,
          space = facet_space,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet2),
          scales = facet_scales,
          space = facet_space,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  #Get layer data for x, y and col scales
  layer_data <- ggplot2::layer_data(plot)

  #Make x scale based on layer_data
  if (x_character | x_factor | x_logical) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  else if (x_numeric | x_date | x_datetime | x_time | x_null) {

    if (facet_scales %in% c("fixed", "free_y")) {
      x_vctr <- layer_data %>%
        dplyr::select(tidyselect::matches(stringr::regex("^x$|^xmin$|^xmax$|^xend$|^xmax_final$"))) %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::pull(.data$value)

      if (x_date) {
        x_vctr <- lubridate::as_date(x_vctr, origin = "1970-01-01")
      }
      else if (x_datetime) {
        x_vctr <- lubridate::as_datetime(x_vctr, origin = "1970-01-01")
      }
      else if (x_time) {
        x_vctr <- hms::as_hms(x_vctr)
      }

      x_range <- x_vctr %>% range(na.rm = TRUE)
      if (!rlang::is_null(x_include)) x_range <- range(c(x_range, x_include))
      if (!rlang::is_null(x_limits)) x_limits <- range(x_limits)

      if (rlang::is_null(x_limits)) {
        if (rlang::is_null(x_breaks)) {
          if (x_time | !x_trans %in% c("identity", "reverse")) {
            x_breaks <- ggplot2::waiver()
            x_limits <- x_range
          }
          else {
            if (!facet_null & !facet2_null) x_breaks_n <- 3
            else if (!facet_null & facet2_null) x_breaks_n <- 3
            else x_breaks_n <- 5

            x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)

            if (y_date | y_datetime | y_time | y_numeric | y_null) {
              x_limits <- x_range
            }
            else if (y_character | y_factor | y_logical) {
              x_limits <- c(min(x_breaks), max(x_breaks))
            }
          }
        }
        else if (!rlang::is_null(x_breaks)) {
          if (y_date | y_datetime | y_time | y_numeric | y_null) {
            x_limits <- x_range
          }
          else if (y_character | y_factor | y_logical) {
            if (is.vector(x_breaks)) x_limits <- c(min(x_breaks), max(x_breaks))
            else if (is.function(x_breaks)) {
              x_limits <- list(x_range) %>%
                purrr::map(.f = x_breaks) %>%
                unlist() %>%
                range()
            }
          }
        }
      }
      else if (!rlang::is_null(x_limits)) {
        x_limits <- x_limits
        if (is.na(x_limits)[1]) x_limits[1] <- min(x_range)
        if (is.na(x_limits)[2]) x_limits[2] <- max(x_range)
        if (!rlang::is_null(x_include)) {
          x_limits <- range(c(x_limits, x_include))
        }

        if (rlang::is_null(x_breaks)) {
          if (x_time) x_breaks <- ggplot2::waiver()
          else if (!x_trans %in% c("identity", "reverse")) x_breaks <- ggplot2::waiver()
          else {
            if (!facet_null & !facet2_null) x_breaks_n <- 3
            else if (!facet_null & facet2_null) x_breaks_n <- 3
            else x_breaks_n <- 5

            x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_limits)
          }
        }
      }

      if (x_trans == "reverse") x_limits <- rev(x_limits)
    }
    else if (facet_scales %in% c("free", "free_x")) {
      if (rlang::is_null(x_limits)) x_limits <- NULL
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
    }

    if (rlang::is_null(x_expand)) {
      if (facet_scales %in% c("fixed", "free_y") &
          (y_date | y_datetime | y_time | y_numeric | y_null)) {
        x_expand <- c(0.05, 0.05)
      }
      else if (!x_trans %in% c("identity", "reverse")) x_expand <- ggplot2::expansion(mult = c(0, 0.05))
      else x_expand <- c(0, 0)
    }

    if (rlang::is_null(x_labels)) {
      if (x_numeric | x_null) x_labels <- scales::label_comma()
      else if (x_date | x_datetime | x_time) x_labels <- scales::label_date_short()
    }

    if (x_numeric | x_null) {
      plot <- plot +
        ggplot2::scale_x_continuous(
          breaks = x_breaks,
          limits = x_limits,
          expand = x_expand,
          labels = x_labels,
          oob = scales::oob_keep,
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
          oob = scales::oob_keep,
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
          oob = scales::oob_keep,
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
          oob = scales::oob_keep,
          sec.axis = x_sec_axis
        )
    }
  }

  #Make y scale based on layer_data
  if (y_character | y_factor | y_logical) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
  }
  else if (y_numeric | y_date | y_datetime | y_time | y_null) {

    if (facet_scales %in% c("fixed", "free_x")) {
      y_vctr <- layer_data %>%
        dplyr::select(tidyselect::matches(stringr::regex("^y$|^ymin$|^ymax$|^yend$|^ymax_final$"))) %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::pull(.data$value)

      if (y_date) {
        y_vctr <- lubridate::as_date(y_vctr, origin = "1970-01-01")
      }
      else if (y_datetime) {
        y_vctr <- lubridate::as_datetime(y_vctr, origin = "1970-01-01")
      }
      else if (y_time) {
        y_vctr <- hms::as_hms(y_vctr)
      }

      y_range <- y_vctr %>% range(na.rm = TRUE)
      if (!rlang::is_null(y_include)) y_range <- range(c(y_range, y_include))
      if (!rlang::is_null(y_limits)) y_limits <- range(y_limits)

      if (rlang::is_null(y_limits)) {
        if (rlang::is_null(y_breaks)) {
          if (y_time | !y_trans %in% c("identity", "reverse")) {
            y_breaks <- ggplot2::waiver()
            y_limits <- y_range
          }
          else {
            if (!facet_null & !facet2_null) y_breaks_n <- 4
            else if (facet_null & !facet2_null) y_breaks_n <- 4
            else y_breaks_n <- 5

            y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)
            y_limits <- c(min(y_breaks), max(y_breaks))
          }
        }
        else if (!rlang::is_null(y_breaks)) {
          if (y_trans %in% c("identity", "reverse")) {
            if (is.vector(y_breaks)) y_limits <- c(min(y_breaks), max(y_breaks))
            else if (is.function(y_breaks)) {
              y_limits <- list(y_range) %>%
                purrr::map(.f = y_breaks) %>%
                unlist() %>%
                range()
            }
          }
          else y_limits <- y_range
        }
      }
      else if (!rlang::is_null(y_limits)) {
        y_limits <- y_limits
        if (is.na(y_limits)[1]) y_limits[1] <- min(y_range)
        if (is.na(y_limits)[2]) y_limits[2] <- max(y_range)
        if (!rlang::is_null(y_include)) y_limits <- range(c(y_limits, y_include))

        if (rlang::is_null(y_breaks)) {
          if (y_time) y_breaks <- ggplot2::waiver()
          else if (!y_trans %in% c("identity", "reverse")) y_breaks <- ggplot2::waiver()
          else {
            if (!facet_null & !facet2_null) y_breaks_n <- 4
            else if (facet_null & !facet2_null) y_breaks_n <- 4
            else y_breaks_n <- 5

            y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_limits)
          }
        }
      }

      if (y_trans == "reverse") y_limits <- rev(y_limits)
    }
    else if (facet_scales %in% c("free", "free_y")) {
      if (rlang::is_null(y_limits)) y_limits <- NULL
      if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
    }

    if (rlang::is_null(y_expand)) {
      if (!y_trans %in% c("identity", "reverse")) y_expand <- ggplot2::expansion(mult = c(0, 0.05))
      else y_expand <- c(0, 0)
    }

    if (rlang::is_null(y_labels)) {
      if (y_numeric | y_null) y_labels <- scales::label_comma()
      else if (y_date | y_datetime | y_time) y_labels <- scales::label_date_short()
    }

    if (y_numeric | y_null) {
      plot <- plot +
        ggplot2::scale_y_continuous(
          breaks = y_breaks,
          limits = y_limits,
          expand = y_expand,
          labels = y_labels,
          oob = scales::oob_keep,
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
          oob = scales::oob_keep,
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
          oob = scales::oob_keep,
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
          oob = scales::oob_keep,
          sec.axis = y_sec_axis
        )
    }
  }

  #make col scale based on layer_data
  if (col_null & !stat %in% c("bin2d", "binhex")) {
    if (rlang::is_null(pal)) pal <-  pal_viridis_mix(1)
    else pal <- pal[1]

    plot <- plot +
      ggplot2::scale_colour_manual(
        values = pal,
        na.value = pal_na,
      ) +
      ggplot2::scale_fill_manual(
        values = pal,
        na.value = pal_na,
      )

    col_legend_place <- "none"
  }
  else {
    if (rlang::is_null(col_title)) {
      if (stat %in% c("bin2d", "binhex")) col_name <- "count"
      else col_name <- rlang::as_name(col)

      if (rlang::is_null(titles)) col_title <- purrr::map_chr(col_name, snakecase::to_sentence_case)
      else col_title <- purrr::map_chr(col_name, titles)
    }

    if (rlang::is_null(col_legend_place)) {
      if (stat %in% c("bin2d", "binhex")) col_legend_place <- "right"
      else {
        if (
          (!x_null & (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(x, data)))) |
          (!y_null & (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(y, data)))) |
          (!facet_null & (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet, data)))) |
          (!facet2_null & (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet2, data))))) {

          col_legend_place <- "none"
        }
        else if (col_numeric) col_legend_place <- "right"
        else col_legend_place <- "bottom"
      }
    }

    if (stat %in% c("bin2d", "binhex")) col_vctr <- layer_data %>% dplyr::pull(.data$count)
    else col_vctr <- data %>% dplyr::pull(!!col)

    if (col_numeric | stat %in% c("bin2d", "binhex")) {

      if (rlang::is_null(col_limits)) {
        col_limits <- col_vctr %>% range(na.rm = TRUE)
        if (!rlang::is_null(col_include)) col_limits <- range(c(col_limits, col_include))
      }
      else if (!rlang::is_null(col_include)) {
        col_limits <- range(c(col_limits, col_include))
      }
      else col_limits <- range(col_limits)

      if (col_trans == "reverse") col_limits <- rev(col_limits)

      if (rlang::is_null(col_breaks)) {
        if (!col_trans %in% c("identity", "reverse")) col_breaks <- ggplot2::waiver()
        else col_breaks <- scales::breaks_pretty(4)
      }

      if (rlang::is_null(pal)) pal <- viridis::viridis(10)

      if (rlang::is_null(col_labels)) {
        if (col_numeric | col_null) col_labels <- scales::label_comma()
        else if (col_date | col_datetime | col_time) col_labels <- scales::label_date_short()
      }

      if (rlang::is_null(col_breaks)) {
        if (!col_trans %in% c("identity", "reverse")) col_breaks <- ggplot2::waiver()
        else col_breaks <- scales::breaks_pretty(4)
      }

      if (!rlang::is_null(col_rescale)) {
        col_rescale <- scales::rescale(col_rescale)
      }
      else col_rescale <- NULL

      if (col_continuous == "gradient") {
        plot <- plot +
          ggplot2::scale_colour_gradientn(
            colours = pal,
            values = col_rescale,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            trans = col_trans,
            na.value = pal_na,
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
            na.value = pal_na,
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
            na.value = pal_na,
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
            na.value = pal_na,
            guide = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev)
          )
      }
    }
    else if (col_character | col_factor | col_logical) {
      if (!rlang::is_null(col_limits)) col_n <- length(col_limits)
      else if (!rlang::is_null(col_breaks)) col_n <- length(col_breaks)
      else {
        if (col_character | col_logical) {
          col_unique <- unique(col_vctr)
          col_n <- length(col_unique[!is.na(col_unique)])
        }
        else if (col_factor) {
          col_n <- length(levels(col_vctr))
        }
      }

      if (rlang::is_null(pal)) pal <- pal_d3_mix(col_n)
      else pal <- pal[1:col_n]

      if (y_numeric | y_date | y_datetime | y_time) {
        if (col_character | col_factor | col_logical) col_legend_rev_auto <- FALSE
        else if (col_legend_place %in% c("top", "bottom")) col_legend_rev_auto <- FALSE
        else col_legend_rev_auto <- TRUE
      }
      else if (y_character | y_factor | y_logical) {
        if (col_character | col_factor | col_logical) col_legend_rev_auto <- TRUE
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
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev_auto,
            title.position = "top",
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        ) +
        ggplot2::scale_fill_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev_auto,
            title.position = "top",
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        )
    }
  }

  #Add coord, theme and titles
  plot <- plot +
    coord +
    theme +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      colour = col_title,
      fill = col_title,
      caption = caption
    )

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
  if (!rlang::is_null(col_title)) {
    if (col_title == "") {
      plot <- plot +
        ggplot2::labs(colour = NULL, fill = NULL)
    }
  }

  #expand the limits if necessary
  if (!rlang::is_null(x_include)) {
    plot <- plot +
      ggplot2::expand_limits(x = x_include)
  }
  if (!rlang::is_null(y_include)) {
    plot <- plot +
      ggplot2::expand_limits(y = y_include)
  }
  if (!rlang::is_null(col_include)) {
    plot <- plot +
      ggplot2::expand_limits(colour = col_include, fill = col_include)
  }

  #adjust the legend
  if (col_legend_place %in% c("top", "bottom")) {
    plot <- plot +
      ggplot2::theme(legend.position = col_legend_place) +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.justification = "left") +
      ggplot2::theme(legend.box.margin = ggplot2::margin(t = -2.5)) +
      ggplot2::theme(legend.text = ggplot2::element_text(
        margin = ggplot2::margin(r = 7.5, unit = "pt")))

    if (col_numeric | stat %in% c("bin2d", "binhex")) {
      plot <- plot +
        ggplot2::theme(legend.key.width = grid::unit(0.66, "cm")) +
        ggplot2::theme(legend.text.align = 0.5)
    }
  }
  else if (col_legend_place %in% c("left", "right")) {
    plot <- plot +
      ggplot2::theme(legend.position = col_legend_place) +
      ggplot2::theme(legend.direction = "vertical") +
      ggplot2::theme(legend.justification = "left") +
      ggplot2::theme(legend.box.margin = ggplot2::margin(t = 0)) +
      ggplot2::theme(legend.text = ggplot2::element_text(
        margin = ggplot2::margin(r = 0)))

    if (col_numeric | stat %in% c("bin2d", "binhex")) {
      plot <- plot +
        ggplot2::theme(legend.title = ggplot2::element_text(vjust = 1))
    }
  }
  else if (col_legend_place == "none") {
    plot <- plot +
      ggplot2::theme(legend.position = col_legend_place)
  }

  #remove gridlines not needed
  if (!x_grid) {
    plot <- plot +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
  }
  if (!y_grid) {
    plot <- plot +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  #return beautiful plot
  return(plot)
}

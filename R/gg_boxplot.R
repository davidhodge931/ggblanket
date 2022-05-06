#' @title Boxplot ggplot.
#'
#' @description Create a boxplot plot with a wrapper around the ggplot2::geom_boxplot function.
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param group Unquoted group aesthetic variable.
#' @param stat Statistical transformation. A character string (e.g. "identity").
#' @param position Position adjustment. Either a character string (e.g."identity"), or a function (e.g. ggplot2::position_identity()).
#' @param pal Colours to use. A character vector of hex codes (or names).
#' @param pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1.
#' @param size Size. A number 0 upwards.
#' @param width Width. A number 0 upwards.
#' @param bins Number of bins. An integer 0 upwards.
#' @param ... Other arguments passed to the relevant ggplot2::geom_* function.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param x_breaks For a numeric or date variable, a vector of breaks for the axis.
#' @param x_breaks_n For a numeric or date variable, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param x_breaks_width For a numeric or date variable, the width of breaks, as calculated by the scales::fullseq function.
#' @param x_expand Add padding to the limits with the ggplot2::expansion function, or a vector of length 2.
#' @param x_oob A scales::oob_* function for how to deal with out-of-bounds values.
#' @param x_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformation. If numeric, accepts a vector. If categorical, accepts a named vector (e.g. c(value = "label", ...)).
#' @param x_limits For a numeric or date variable, a vector of length 2 to determine the limits of the axis. Use c(NA, NA) for the min and max.
#' @param x_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param x_zero For a numeric variable, TRUE or FALSE of whether the axis should include zero. Defaults to FALSE.
#' @param x_zero_mid For a numeric variable, TRUE or FALSE of whether to put zero in the middle of the axis. Defaults to FALSE.
#' @param y_breaks For a numeric or date variable, a vector of breaks for the axis.
#' @param y_breaks_n For a numeric or date variable, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param y_breaks_width For a numeric or date variable, the width of breaks, as calculated by the scales::fullseq function.
#' @param y_expand Add padding to the limits with the ggplot2::expansion function, or a vector of length 2.
#' @param y_oob A scales::oob_* function for how to deal with out-of-bounds values.
#' @param y_labels A functiyon to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformation. If numeric, accepts a vector. If categorical, accepts a named vector (e.g. c(value = "label", ...)).
#' @param y_limits For a numeric or date variable, a vector of length 2 to determine the limits of the axis. Use c(NA, NA) for the min and max.
#' @param y_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param y_zero For a numeric variable, TRUE or FALSE of whether the axis should include zero. Defaults to FALSE.
#' @param y_zero_mid For a numeric variable, TRUE or FALSE of whether to put zero in the middle of the axis. Defaults to FALSE.
#' @param col_breaks A vector of breaks. For a categorical col variable, this links pal values with col variable values dropping those not used. For a numeric variable where col_intervals is NULL, this only affects the labels on the legend.
#' @param col_breaks_n For a numeric variable where col_intervals is NULL, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param col_breaks_width For a numeric variable, the width of breaks, as calculated by the scales::fullseq function.
#' @param col_intervals A function to cut or chop the numeric variable into intervals, including in rlang lambda format (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param col_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformation. If categorical, accepts a named vector (e.g. c(value = "label", ...)). Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector of limits. For a categorical col variable, this links pal values with col variable values keeping those not used. For a numeric variable where col_intervals is NULL, this will make all values outside the limits coloured NA.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "r" for right, "b" for bottom, "t" for top, or "l" for left.
#' @param col_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param facet_intervals A function to cut or chop the numeric variable into intervals, including in rlang lambda format (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param facet_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformation. If categorical, accepts a named vector (e.g. c(value = "label", ...)).
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
#' library(ggplot2)
#' gg_boxplot(mpg, x = class, y = hwy)
#' gg_boxplot(mpg, x = hwy, y = class)
#' gg_boxplot(mpg, x = hwy, y = class, notch = TRUE)
#' gg_boxplot(mpg, x = hwy, y = class, varwidth = TRUE)
#' gg_boxplot(mpg, x = hwy, y = class, pal = "#3366FF", fill = "white")
#' gg_boxplot(mpg, x = hwy, y = class, outlier.colour = "red", outlier.shape = 1)
#'
#' gg_boxplot(mpg, x = hwy, y = class, outlier.shape = NA) +
#'   geom_jitter(width = 0.2)
#'
#' gg_boxplot(mpg, x = hwy, y = class, col = drv)
#'
#' gg_boxplot(diamonds, x = carat, y = price)
#'
#' gg_boxplot(diamonds, x = carat, y = price)
#'
#' gg_boxplot(diamonds, carat, price, group = ggplot2::cut_width(carat, 0.25))
#'
#' gg_boxplot(diamonds, carat, price, group = ggplot2::cut_width(carat, 0.25),
#'            outlier.alpha = 0.1)
#'
gg_boxplot <- function(data = NULL,
                       x = NULL,
                       y = NULL,
                       col = NULL,
                       facet = NULL,
                       group = NULL,
                       stat = "boxplot",
                       position = "dodge2",
                       pal = NULL,
                       pal_na = "#7F7F7F",
                       alpha = 0.5,
                       size = 0.5,
                       width = NULL,
                       bins = 40,
                       ...,
                       title = NULL,
                       subtitle = NULL,
                       coord = ggplot2::coord_cartesian(clip = "off"),
                       x_breaks = NULL,
                       x_breaks_n = NULL,
                       x_breaks_width = NULL,
                       x_expand = NULL,
                       x_labels = NULL,
                       x_limits = NULL,
                       x_oob = scales::oob_squish,
                       x_title = NULL,
                       x_zero = NULL,
                       x_zero_mid = FALSE,
                       y_breaks = NULL,
                       y_breaks_n = NULL,
                       y_breaks_width = NULL,
                       y_expand = NULL,
                       y_labels = NULL,
                       y_limits = NULL,
                       y_oob = scales::oob_squish,
                       y_title = NULL,
                       y_zero = NULL,
                       y_zero_mid = FALSE,
                       col_breaks = NULL,
                       col_breaks_n = NULL,
                       col_breaks_width = NULL,
                       col_intervals = NULL,
                       col_labels = NULL,
                       col_legend_place = NULL,
                       col_legend_ncol = NULL,
                       col_legend_nrow = NULL,
                       col_limits = NULL,
                       col_title = NULL,
                       facet_intervals = NULL,
                       facet_labels = snakecase::to_sentence_case,
                       facet_ncol = NULL,
                       facet_nrow = NULL,
                       facet_scales = "fixed",
                       caption = NULL,
                       theme = NULL) {

  #quote
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  group <- rlang::enquo(group)

  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required")
  if (!rlang::quo_is_null(col)) rlang::inform(c("i" = "{ggblanket} merges col and fill aesthetics into a single col aesthetic"))
  if (!rlang::quo_is_null(facet)) rlang::inform(c("i" = "{ggblanket} treats faceting as an aesthetic"))

  ###ungroup
  data <- dplyr::ungroup(data)

  ###get default NULL values
  if (rlang::is_null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x))
  if (rlang::is_null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y))

  if (rlang::is_null(theme)) {
    x_grid <- ifelse(
      is.numeric(rlang::eval_tidy(x, data)) |
        lubridate::is.Date(rlang::eval_tidy(x, data)) |
        rlang::quo_is_null(x),
      TRUE,
      FALSE
    )

    y_grid <- ifelse(
      is.numeric(rlang::eval_tidy(y, data)) |
        lubridate::is.Date(rlang::eval_tidy(y, data)) |
        rlang::quo_is_null(y),
      TRUE,
      FALSE
    )

    theme <- gg_theme(x_grid = x_grid, y_grid = y_grid)
  }

  if (rlang::is_null(width)) {
    if (lubridate::is.Date(rlang::eval_tidy(x, data)) |
        lubridate::is.Date(rlang::eval_tidy(y, data)) |
        (is.numeric(rlang::eval_tidy(x, data)) &
         is.numeric(rlang::eval_tidy(y, data)))) {
      width <- NULL
    }
    else
      width <- 0.5
  }

  if (rlang::is_null(x_zero)) x_zero <- FALSE
  if (rlang::is_null(y_zero)) y_zero <- FALSE

  ###process plot data
  ###factorise logical, reverse for horizontal, and chop intervals
  if (!rlang::quo_is_null(x)) {
    if (is.logical(rlang::eval_tidy(x, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }
  }

  if (!rlang::quo_is_null(y)) {
    if (is.logical(rlang::eval_tidy(y, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {

      if (!rlang::quo_is_null(col) &
          (identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data)))) {
      }
      else {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y, ~ forcats::fct_rev(.x)))
      }
    }
  }

  if (!rlang::quo_is_null(col)) {

    if (is.logical(rlang::eval_tidy(col, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (is.factor(rlang::eval_tidy(col, data)) | is.character(rlang::eval_tidy(col, data))) {
      if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, ~ forcats::fct_rev(.x)))
      }
    }
  }

  if (!rlang::quo_is_null(facet)) {
    if (is.logical(class(rlang::eval_tidy(facet, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (!rlang::is_null(facet_intervals)) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, facet_intervals))
    }
  }

  ###make col scale
  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(pal)) pal <-  pal_viridis_mix(1)
    else pal <- pal[1]

    col_scale <- ggplot2::scale_colour_manual(
      values = pal,
      na.value = pal_na,
      aesthetics = c("col", "fill")
    )

    col_legend_place <- "n"
  }
  else {
    if (rlang::is_null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col))
    col_title_position <- ifelse(col_title == "", "right", "top")

    if (rlang::is_null(col_legend_place)) {
      if (!rlang::quo_is_null(x) &
          (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(x, data)))) {
        col_legend_place <- "n"
      }
      else if (!rlang::quo_is_null(y) &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(y, data)))) {
        col_legend_place <- "n"
      }
      else if (!rlang::quo_is_null(facet) &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet, data)))) {
        col_legend_place <- "n"
      }
      else
        col_legend_place <- "r"
    }

    if (is.numeric(rlang::eval_tidy(col, data))) {
      if (rlang::is_null(col_intervals)) { #continuous col
        if (rlang::is_null(col_breaks)) {
          col_vctr <- dplyr::pull(data, !!col)
          col_min_max <- c(min(col_vctr, na.rm = TRUE), max(col_vctr, na.rm = TRUE))
          if (!rlang::is_null(col_limits)) col_min_max <- col_limits

          if (!rlang::is_null(col_breaks_width)) {
            col_breaks <- scales::fullseq(col_min_max, size = col_breaks_width)
          }
          else {
            if (rlang::is_null(col_breaks_n)) {
              if (col_legend_place %in% c("b", "t")) col_breaks_n <- 3
              else col_breaks_n <- 4
            }
            col_breaks <- pretty(col_min_max, n = col_breaks_n)
          }
        }

        if (rlang::is_null(pal)) pal <- viridis::viridis(100)
        if (rlang::is_null(col_labels)) col_labels <- scales::label_comma()

        col_scale <- ggplot2::scale_colour_gradientn(
          colors = pal,
          labels = col_labels,
          breaks = col_breaks,
          limits = col_limits,
          na.value = pal_na,
          name = col_title,
          aesthetics = c("col", "fill"),
          guide = ggplot2::guide_colorbar(title.position = col_title_position)
        )
      }
      else { #intervals col
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, col_intervals))

        col_levels <- levels(rlang::eval_tidy(col, data))
        col_n <- length(col_levels)

        if (rlang::is_null(pal)) pal <- pal_viridis_mix(col_n)
        else pal <- pal[1:col_n]

        if (is.numeric(rlang::eval_tidy(y, data)) |
            lubridate::is.Date(rlang::eval_tidy(y, data))) {

          if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
          else col_legend_rev <- TRUE
        }
        else if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
          if (col_legend_place %in% c("b", "t")) col_legend_rev <- TRUE
          else col_legend_rev <- FALSE
          pal <- rev(pal)
        }
        else col_legend_rev <- FALSE

        if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
        if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

        col_scale <- ggplot2::scale_colour_manual(
          values = pal,
          breaks = col_levels,
          limits = col_levels,
          labels = col_labels,
          na.value = pal_na,
          name = col_title,
          aesthetics = c("col", "fill"),
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        )
      }
    }
    else { #categorical col
      if (!rlang::is_null(col_limits)) col_n <- length(col_limits)
      else if (!rlang::is_null(col_breaks)) col_n <- length(col_breaks)
      else {
        if (is.factor(rlang::eval_tidy(col, data))) {
          col_n <- length(levels(rlang::eval_tidy(col, data)))
        }
        else col_n <- length(unique(rlang::eval_tidy(col, data)))
      }

      if (rlang::is_null(pal)) pal <- pal_d3_mix(col_n)
      else pal <- pal[1:col_n]

      if (is.numeric(rlang::eval_tidy(y, data)) |
          lubridate::is.Date(rlang::eval_tidy(y, data))) {

        if (is.factor(rlang::eval_tidy(col, data)) | is.character(rlang::eval_tidy(col, data))) {
          col_legend_rev <- FALSE
        }
        else if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
        else col_legend_rev <- TRUE
      }
      else if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
        if (is.factor(rlang::eval_tidy(col, data)) | is.character(rlang::eval_tidy(col, data))) {
          col_legend_rev <- TRUE
        }
        else if (col_legend_place %in% c("b", "t")) col_legend_rev <- TRUE
        else col_legend_rev <- FALSE
        pal <- rev(pal)
      }
      else col_legend_rev <- FALSE

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
      if (rlang::is_null(col_labels)) col_labels <- snakecase::to_sentence_case

      col_scale <- ggplot2::scale_colour_manual(
        values = pal,
        breaks = col_breaks,
        limits = col_limits,
        labels = col_labels,
        na.value = pal_na,
        name = col_title,
        aesthetics = c("col", "fill"),
        guide = ggplot2::guide_legend(
          reverse = col_legend_rev,
          title.position = col_title_position,
          ncol = col_legend_ncol,
          nrow = col_legend_nrow,
          byrow = TRUE)
      )
    }
  }

  ###make plot
  if (!rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
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
  else if (!rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          col = "",
          fill = "",
          group = !!group
        ))
    }
  }
  else if (rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          col = "",
          fill = "",
          group = !!group
        ))
    }
  }
  else if (rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          col = !!col,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          col = "",
          fill = "",
          group = !!group
        ))
    }
  }

  plot <- plot +
    ggplot2::geom_boxplot(
      stat = stat,
      position = position,
      alpha = alpha,
      size = size,
      width = width,
      bins = bins,
      ...
    )

  if (!rlang::quo_is_null(facet)) {
    if (!rlang::is_null(facet_intervals)) {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(!!facet),
          scales = facet_scales,
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    }
    else {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(!!facet),
          labeller = ggplot2::as_labeller(facet_labels),
          scales = facet_scales,
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    }
  }

  ###Add x scale for where y is NULL
  if (!rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data))) {
      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
      if (rlang::is_null(x_labels)) x_labels <- snakecase::to_sentence_case

      x_scale <- ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
    }
    else {
      if (facet_scales %in% c("fixed", "free_y")) {
        x_vctr <- dplyr::pull(data, !!x)
        x_min <- min(x_vctr, na.rm = TRUE)
        x_max <- max(x_vctr, na.rm = TRUE)

        if (rlang::is_null(x_breaks)) {
          x_min_max <- c(x_min, x_max)
          if (x_zero) x_min_max <- c(0, x_min_max)
          if (x_zero_mid) x_min_max <- c(-x_min_max, x_min_max)
          if (!rlang::is_null(x_limits) & !any(is.na(x_limits))) x_min_max <- x_limits

          if (!rlang::is_null(x_breaks_width)) {
            x_breaks <- scales::fullseq(x_min_max, size = x_breaks_width)
          }
          else {
            if (rlang::is_null(x_breaks_n)) {
              x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
            }
            x_breaks <- pretty(x_min_max, n = x_breaks_n)
          }
        }

        if (rlang::is_null(x_limits)) x_limits <- c(min(x_breaks), max(x_breaks))
        if (rlang::is_null(x_expand)) x_expand <- c(0, 0)
      }
      else if (facet_scales %in% c("free", "free_x")) {
        if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
        x_limits <- NULL
        if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
      }

      if (rlang::is_null(x_labels)) {
        if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) x_labels <- scales::label_comma()
        else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date_short()
        else x_labels <- ggplot2::waiver()
      }

      if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) {
        x_scale <- ggplot2::scale_x_continuous(
          breaks = x_breaks,
          limits = x_limits,
          expand = x_expand,
          labels = x_labels,
          oob = x_oob
        )
      }
      else if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
        x_scale <- ggplot2::scale_x_date(
          breaks = x_breaks,
          limits = x_limits,
          expand = x_expand,
          labels = x_labels,
          oob = x_oob
        )
      }
    }

    plot <- plot +
      x_scale
  }

  ###Add y scale where x is NULL
  if (!rlang::quo_is_null(y) & rlang::quo_is_null(x)) {
    if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
      if (rlang::is_null(y_labels)) y_labels <- snakecase::to_sentence_case

      y_scale <- ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
    }
    else {
      if (facet_scales %in% c("fixed", "free_x")) {

        y_vctr <- dplyr::pull(data, !!y)

        y_min <- min(y_vctr, na.rm = TRUE)
        y_max <- max(y_vctr, na.rm = TRUE)

        if (rlang::is_null(y_breaks)) {
          y_min_max <- c(y_min, y_max)
          if (y_zero) y_min_max <- c(0, y_min_max)
          if (y_zero_mid) y_min_max <- c(-y_min_max, y_min_max)
          if (!rlang::is_null(y_limits) & !any(is.na(y_limits))) y_min_max <- y_limits

          if (!rlang::is_null(y_breaks_width)) {
            y_breaks <- scales::fullseq(y_min_max, size = y_breaks_width)
          }
          else {
            if (rlang::is_null(y_breaks_n)) {
              y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 4)
            }
            y_breaks <- pretty(y_min_max, n = y_breaks_n)
          }
        }

        if (rlang::is_null(y_limits)) y_limits <- c(min(y_breaks), max(y_breaks))
        if (rlang::is_null(y_expand)) y_expand <- c(0, 0)
      }
      else if (facet_scales %in% c("free", "free_y")) {
        if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
        y_limits <- NULL
        if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
      }

      if (rlang::is_null(y_labels)) {
        if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) y_labels <- scales::label_comma()
        else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date_short()
        else y_labels <- ggplot2::waiver()
      }

      if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) {
        y_scale <- ggplot2::scale_y_continuous(
          breaks = y_breaks,
          limits = y_limits,
          expand = y_expand,
          labels = y_labels,
          oob = y_oob
        )
      }
      else if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
        y_scale <- ggplot2::scale_y_date(
          breaks = y_breaks,
          limits = y_limits,
          expand = y_expand,
          labels = y_labels,
          oob = y_oob
        )
      }
    }

    plot <- plot +
      y_scale
  }

  ###Get layer plot to finalise x and y scales
  layer_data <- ggplot2::layer_data(plot)

  ###Make x scale based on layer_data
  if (is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data))) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- snakecase::to_sentence_case

    x_scale <- ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  else {
    if (facet_scales %in% c("fixed", "free_y")) {

      x_vctr <- layer_data %>%
        dplyr::select(tidyselect::matches(stringr::regex("^x$|^xmin$|^xmax$|^xend$|^xmax_final$"))) %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::pull(.data$value)

      if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
        x_vctr <- as.Date(x_vctr, origin = "1970-01-01")
      }

      x_min <- min(x_vctr, na.rm = TRUE)
      x_max <- max(x_vctr, na.rm = TRUE)

      if (rlang::is_null(x_breaks)) {
        x_min_max <- c(x_min, x_max)
        if (x_zero) x_min_max <- c(0, x_min_max)
        if (x_zero_mid) x_min_max <- c(-x_min_max, x_min_max)

        if (!rlang::is_null(x_breaks_width)) {
          x_breaks <- scales::fullseq(x_min_max, size = x_breaks_width)
        }
        else {
          if (rlang::is_null(x_breaks_n)) {
            x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
          }
          x_breaks <- pretty(x_min_max, n = x_breaks_n)
        }
      }

      if (length(class(position)) == 1) {
        if (position == "fill") x_limits <- c(NA, NA)
      }
      else if (class(position)[1] == "PositionFill"){
        x_limits <- c(NA, NA)
      }

      if (rlang::is_null(x_limits)) x_limits <- c(min(x_breaks), max(x_breaks))
      if (rlang::is_null(x_expand)) x_expand <- c(0, 0)
    }
    else if (facet_scales %in% c("free", "free_x")) {
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
      x_limits <- NULL
      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    }

    if (rlang::is_null(x_labels)) {
      if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) x_labels <- scales::label_comma()
      else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date_short()
      else x_labels <- ggplot2::waiver()
    }

    if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) {
      x_scale <- ggplot2::scale_x_continuous(
        breaks = x_breaks,
        limits = x_limits,
        expand = x_expand,
        labels = x_labels,
        oob = x_oob
      )
    }
    else if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
      x_scale <- ggplot2::scale_x_date(
        breaks = x_breaks,
        limits = x_limits,
        expand = x_expand,
        labels = x_labels,
        oob = x_oob
      )
    }
  }

  plot <- plot +
    x_scale

  ###Make y scale based on layer_data
  if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- snakecase::to_sentence_case

    y_scale <- ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
  }
  else {
    if (facet_scales %in% c("fixed", "free_x")) {

      y_vctr <- layer_data %>%
        dplyr::select(tidyselect::matches(stringr::regex("^y$|^ymin$|^ymax$|^yend$|^ymax_final$"))) %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::pull(.data$value)

      if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
        y_vctr <- as.Date(y_vctr, origin = "1970-01-01")
      }

      y_min <- min(y_vctr, na.rm = TRUE)
      y_max <- max(y_vctr, na.rm = TRUE)

      if (rlang::is_null(y_breaks)) {
        y_min_max <- c(y_min, y_max)
        if (y_zero) y_min_max <- c(0, y_min_max)
        if (y_zero_mid) y_min_max <- c(-y_min_max, y_min_max)
        if (!rlang::is_null(y_limits) & !any(is.na(y_limits))) y_min_max <- y_limits

        if (!rlang::is_null(y_breaks_width)) {
          y_breaks <- scales::fullseq(y_min_max, size = y_breaks_width)
        }
        else {
          if (rlang::is_null(y_breaks_n)) {
            y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 4)
          }
          y_breaks <- pretty(y_min_max, n = y_breaks_n)
        }
      }

      if (length(class(position)) == 1) {
        if (position == "fill") y_limits <- c(NA, NA)
      }
      else if (class(position)[1] == "PositionFill"){
        y_limits <- c(NA, NA)
      }

      if (rlang::is_null(y_limits)) y_limits <- c(min(y_breaks), max(y_breaks))
      if (rlang::is_null(y_expand)) y_expand <- c(0, 0)
    }
    else if (facet_scales %in% c("free", "free_y")) {
      if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
      y_limits <- NULL
      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    }

    if (rlang::is_null(y_labels)) {
      if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) y_labels <- scales::label_comma()
      else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date_short()
      else y_labels <- ggplot2::waiver()
    }

    if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) {
      y_scale <- ggplot2::scale_y_continuous(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = y_oob
      )
    }
    else if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
      y_scale <- ggplot2::scale_y_date(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = y_oob
      )
    }
  }

  plot <- plot +
    y_scale

  #make the plot
  plot <- plot +
    col_scale +
    coord +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      caption = caption
    ) +
    theme

  ###adjust legend
  if (col_legend_place == "b") {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.position = "bottom")
  }
  else if (col_legend_place == "t") {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.position = "top")
  }
  else if (col_legend_place == "n" | rlang::quo_is_null(col)) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }
  else if (col_legend_place == "l") {
    plot <- plot +
      ggplot2::theme(legend.position = "left")
  }

  #return beautiful plot
  return(plot)
}

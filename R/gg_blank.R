#' @title Blank ggplot.
#'
#' @description Blank ggplot.
#' @param data A data frame in a structure to be plotted untransformed. Required input.
#' @param x Unquoted x variable to be on the x scale (i.e. character, factor, logical, numeric, or date).
#' @param y Unquoted y variable to be on the y scale (i.e. character, factor, logical, numeric, or date).
#' @param col Unquoted variable to col and fill by (i.e. character, factor, logical, numeric)
#' @param facet Unquoted facet variable (i.e. character, factor, logical)
#' @param xmin Unquoted xmin variable (i.e. numeric).
#' @param xmax Unquoted xmax variable (i.e. numeric).
#' @param xend Unquoted xend variable (i.e. numeric).
#' @param ymin Unquoted ymin variable (i.e. numeric).
#' @param ymax Unquoted ymax variable (i.e. numeric).
#' @param yend Unquoted xend variable (i.e. numeric).
#' @param group Unquoted group variable.
#' @param text Unquoted text variable to be used as a tooltip with plotly::ggplotly(..., tooltip = "text").
#' @param stat Statistical transformation. A character string (e.g. "identity").
#' @param position Position adjustment. Either a character string (e.g."identity"), or a function (e.g. ggplot2::position_identity()).
#' @param pal Character vector of hex codes.
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param alpha Opacity. A number between 0 and 1.
#' @param size Size. A number 0 upwards.
#' @param width Width. A number above 0 upwards.
#' @param bins Number of bins to transform the data into.
#' @param ... Other arguments passed to the relevant ggplot2::geom_* function.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param x_breaks For a numeric or date variable, a vector of breaks for the axis.
#' @param x_breaks_n For a numeric or date variable, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param x_breaks_width For a numeric or date variable, the width of breaks, as calculated by the scales::fulseq function.
#' @param x_expand Add padding to the limits with the ggplot2::ggplot2::expansion function, or a vector of length 2.
#' @param x_oob A scales::oob_* function for how to deal with out-of-bounds values.
#' @param x_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformations. Also accepts a vector.
#' @param x_limits For a numeric or date variable, a vector of length 2 to determine the limits of the axis. Use c(NA, NA) for the min and max.
#' @param x_na_rm TRUE or FALSE of whether to include NA values. Defaults to FALSE.
#' @param x_rev For a categorical variable, TRUE or FALSE of whether the variable should be reversed. Defaults to FALSE.
#' @param x_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param x_zero For a numeric variable, TRUE or FALSE of whether the axis should include zero. Defaults to FALSE.
#' @param x_zero_mid For a numeric variable, TRUE or FALSE of whether to put zero in the middle of the axis. Defaults to FALSE.
#' @param y_breaks For a numeric or date variable, a vector of breaks for the axis.
#' @param y_breaks_n For a numeric or date variable, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param y_breaks_width For a numeric or date variable, the width of breaks, as calculated by the scales::fulseq function.
#' @param y_expand Add padding to the limits with the ggplot2::ggplot2::expansion function, or a vector of length 2.
#' @param y_oob A scales::oob_* function for how to deal with out-of-bounds values.
#' @param y_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformations. Also accepts a vector.
#' @param y_limits For a numeric or date variable, a vector of length 2 to determine the limits of the axis. Use c(NA, NA) for the min and max.
#' @param y_na_rm TRUE or FALSE of whether to include NA values. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the variable should be reversed. Defaults to FALSE.
#' @param y_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param y_zero For a numeric variable, TRUE or FALSE of whether the axis should include zero. Defaults to FALSE.
#' @param y_zero_mid For a numeric variable, TRUE or FALSE of whether to put zero in the middle of the axis. Defaults to FALSE.
#' @param col_breaks A vector of breaks. For a categorical col variable, this links pal values with col variable values dropping those not used. For a numeric variable where col_intervals is NULL, this only affects the labels on the legend.
#' @param col_breaks_n For a numeric variable where col_intervals is NULL, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param col_breaks_width For a numeric variable, the width of breaks, as calculated by the scales::fulseq function.
#' @param col_intervals A function to cut or chop the numeric variable into intervals, including in rlang lambda format (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param col_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformations. Also accepts a vector. Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector of limits. For a categorical col variable, this links pal values with col variable values keeping those not used. For a numeric variable where col_intervals is NULL, this will make all values outside the limits coloured NA.
#' @param col_na_rm TRUE or FALSE of whether to include NA values. Defaults to FALSE.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "r" for right, "b" for bottom, "t" for top, "l" for left, or "m" for a mobile-friendly legend.
#' @param col_rev For a categorical variable, TRUE or FALSE of whether the variable should be reversed. Defaults to FALSE.
#' @param col_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param facet_intervals A function to cut or chop the numeric variable into intervals, including in rlang lambda format (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param facet_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformations. Also accepts a vector.
#' @param facet_na_rm TRUE or FALSE of whether to include facet NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_rev TRUE or FALSE of whether the variable should be reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
gg_blank <- function(data = NULL,
                     x = NULL,
                     y = NULL,
                     col = NULL,
                     facet = NULL,
                     group = NULL,
                     xmin = NULL,
                     xmax = NULL,
                     xend = NULL,
                     ymin = NULL,
                     ymax = NULL,
                     yend = NULL,
                     text = NULL,
                     stat = "identity",
                     position = "identity",
                     pal = NULL,
                     pal_na = "#7F7F7F",
                     alpha = 1,
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
                     x_na_rm = FALSE,
                     x_oob = scales::oob_censor,
                     x_rev = FALSE,
                     x_title = NULL,
                     x_zero = NULL,
                     x_zero_mid = FALSE,
                     y_breaks = NULL,
                     y_breaks_n = NULL,
                     y_breaks_width = NULL,
                     y_expand = NULL,
                     y_labels = NULL,
                     y_limits = NULL,
                     y_na_rm = FALSE,
                     y_oob = scales::oob_censor,
                     y_rev = FALSE,
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
                     col_na_rm = FALSE,
                     col_rev = FALSE,
                     col_title = NULL,
                     facet_intervals = NULL,
                     facet_labels = snakecase::to_sentence_case,
                     facet_na_rm = FALSE,
                     facet_ncol = NULL,
                     facet_nrow = NULL,
                     facet_rev = FALSE,
                     facet_scales = "fixed",
                     caption = NULL,
                     theme = NULL) {

  #quote
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  group <- rlang::enquo(group)

  xmin <- rlang::enquo(xmin)
  xmax <- rlang::enquo(xmax)
  xend <- rlang::enquo(xend)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  yend <- rlang::enquo(yend)

  text <- rlang::enquo(text)

  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required")
  if (!rlang::quo_is_null(col)) rlang::inform(c("i" = "Note in {ggbilly}, the {ggplot2} fill aesthetic inherits from col"))

  ###ungroup
  data <- dplyr::ungroup(data)

  #zero's
  if (rlang::is_null(x_zero)) x_zero <- FALSE
  if (rlang::is_null(y_zero)) y_zero <- FALSE

  ###x var process
  if (!rlang::quo_is_null(x)) {
    if (x_na_rm) {
      data <- data %>%
        dplyr::filter(!is.na(!!x))
    }

    if (is.logical(rlang::eval_tidy(x, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (x_rev) {
      if (is.factor(rlang::eval_tidy(x, data)) | is.character(rlang::eval_tidy(x, data))) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!x, ~ forcats::fct_rev(.x)))
      }
    }
  }

  ###y process
  if (!rlang::quo_is_null(y)) {
    if (y_na_rm) {
      data <- data  %>%
        dplyr::filter(!is.na(!!y))
    }

    if (is.logical(rlang::eval_tidy(y, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
      if (!y_rev) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y, ~ forcats::fct_rev(.x)))
      }
    }
  }

  ###facet process
  if (!rlang::quo_is_null(facet)) {
    if (facet_na_rm) {
      data <- data %>%
        dplyr::filter(!is.na(!!facet))
    }

    if (is.logical(class(rlang::eval_tidy(facet, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (!rlang::is_null(facet_intervals)) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, facet_intervals))
    }

    if (facet_rev) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, ~ forcats::fct_rev(.x)))
    }
  }

  ###col process
  if (!rlang::quo_is_null(col)) {

    if (col_na_rm) {
      data <- data %>%
        dplyr::filter(!is.na(!!col))
    }

    if (is.logical(rlang::eval_tidy(col, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (is.factor(rlang::eval_tidy(col, data)) | is.character(rlang::eval_tidy(col, data))) {
      if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
        if (!col_rev) {
          data <- data %>%
            dplyr::mutate(dplyr::across(!!col, ~ forcats::fct_rev(.x)))
        }
      }
      else if (col_rev) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, ~ forcats::fct_rev(.x)))
      }
    }
  }

  ###theme
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

  ###width
  if (rlang::is_null(width)) {
    if (is.factor(rlang::eval_tidy(x, data)) | is.character(rlang::eval_tidy(x, data)) |
        is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {

      width <- 0.75
    }
  }

  ##col scale
  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(pal)) pal <-  pal_viridis_reorder(1)
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
      if (!rlang::quo_is_null(facet) &
          (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet, data)))) {
        col_legend_place <- "n"
      }
      else col_legend_place <- "r"
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

        if (rlang::is_null(pal)) pal <- pal_viridis_reorder(col_n)
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

      if (rlang::is_null(pal)) pal <- pal_d3_reorder(col_n)
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

  ###plot
  ###no xmin, xmax, xend, ymin, ymax, yend
  if (!rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          col = !!col,
          fill = !!col,
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          col = "1",
          fill = "1",
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
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
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          col = "1",
          fill = "1",
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
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
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          col = "1",
          fill = "1",
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
        ))
    }
  }
  else if (rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          col = !!col,
          fill = !!col,
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          col = "1",
          fill = "1",
          group = !!group,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
        ))
    }
  }

  plot <- plot +
    ggplot2::geom_blank(
      ggplot2::aes(text = !!text),
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

  ###x scale where y is NULL #xscale1
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
          if (!rlang::is_null(x_limits)) x_min_max <- x_limits

          if (!rlang::is_null(x_breaks_width)) {
            x_breaks <- scales::fullseq(x_min_max, size = x_breaks_width)
          }
          else {
            if (rlang::is_null(x_breaks_n)) {
              x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 2)
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
        else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date()
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

  ###y scale where x is NULL #yscale1
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
          if (!rlang::is_null(y_limits)) y_min_max <- y_limits

          if (!rlang::is_null(y_breaks_width)) {
            y_breaks <- scales::fullseq(y_min_max, size = y_breaks_width)
          }
          else {
            if (rlang::is_null(y_breaks_n)) {
              y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
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
        else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date()
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

  #build plot
  layer_data <- ggplot2::layer_data(plot)
  # return(layer_data)

  ###x scale where y not NULL #xscale2
  if (!rlang::quo_is_null(y)) {
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
          if (!rlang::is_null(x_limits)) x_min_max <- x_limits

          if (!rlang::is_null(x_breaks_width)) {
            x_breaks <- scales::fullseq(x_min_max, size = x_breaks_width)
          }
          else {
            if (rlang::is_null(x_breaks_n)) {
              x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 2)
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
        else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date()
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

  ###y scale where x not NULL #yscale2
  if (!rlang::quo_is_null(x)) {
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
          if (!rlang::is_null(y_limits)) y_min_max <- y_limits

          if (!rlang::is_null(y_breaks_width)) {
            y_breaks <- scales::fullseq(y_min_max, size = y_breaks_width)
          }
          else {
            if (rlang::is_null(y_breaks_n)) {
              y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
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
        else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date()
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

  ###titles
  if (rlang::quo_is_null(x)) {
    if (rlang::is_null(x_title)) {
      if (stat %in% c("bin", "count")) x_title <- "Count"
      else if (stat == "density") x_title <- "Density"
      else if (stat == "function") x_title <- "X"
    }
  }
  else if (rlang::is_null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x))

  if (rlang::quo_is_null(y)) {
    if (rlang::is_null(y_title)) {
      if (stat %in% c("bin", "count")) y_title <- "Count"
      else if (stat == "density") y_title <- "Density"
      else if (stat == "function") y_title <- "Y"
    }
  }
  else if (rlang::is_null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y))

  #make the plot
  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      caption = caption
    ) +
    theme +
    coord +
    col_scale

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
  else if (col_legend_place == "m") {
    plot <- plot +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.direction = "vertical")
  }

  return(plot)
}

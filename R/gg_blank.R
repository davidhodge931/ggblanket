#' @title Blank ggplot.
#'
#' @description Blank ggplot.
#' @param data A data frame in a structure to be plotted untransformed. Required input.
#' @param x Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, or date). Required input.
#' @param y Unquoted numeric variable to be on the y scale. Required input.
#' @param col Unquoted variable to col and fill by.
#' @param facet Unquoted categorical variable to facet by.
#' @param xmin
#' @param xmax
#' @param xend
#' @param ymin
#' @param ymax
#' @param yend
#' @param tooltip Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(..., tooltip = "text").
#' @param position How overlapping geom's should be positioned with a character string (e.g."identity", "dodge", "dodge2", "fill"), or a function (e.g. ggplot2::position_*()).
#' @param stat
#' @param bins
#' @param binwidth
#' @param palette Character vector of hex codes.
#' @param palette_na The hex code or name of the NA col to be used.
#' @param alpha Opacity argument per ggplot2::geom_* function.
#' @param size Size argument per ggplot2::geom_* function. Defaults to 0.5.
#' @param width Width of any polygons. Defaults to 0.75 if x or y is categorical. Otherwise NULL.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks For a numeric or date x variable, a vector of breaks for the x axis. Note the x_limits will be the min and max of this vector.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions.
#' @param x_oob scales function for how to deal with out-of-bounds values on the x axis. See ggplot2::scale_x_continuous for further information.
#' @param x_labels A function or named vector to modify x scale labels. Use function(x) x to keep labels untransformed.
#' @param x_limits For a numeric or date x variable, a vector of length 2 to determine the limits of the x axis. Use c(NA, NA) for the min and max of the x variable.
#' @param x_na_rm TRUE or FALSE of whether to include x NA values. Defaults to FALSE.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title x scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero.
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks For a numeric or date y variable, a vector of breaks for the y axis.
#' @param y_breaks_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions.
#' @param y_labels A function or named vector to modify y scale labels. Use function(x) x to keep labels untransformed.
#' @param y_limits For a numeric or date y variable, a vector of length 2 to determine the limits of the y axis. Use c(NA, NA) for the min and max of the y variable.
#' @param y_na_rm TRUE or FALSE of whether to remove y NA values. Defaults to FALSE.
#' @param y_oob scales function for how to deal with out-of-bounds values on the y axis. See ggplot2::scale_y_continuous for further information.
#' @param y_rev For a categorical x variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param col_breaks For a numeric col variable, a vector of breaks. If "continuous" col_method is selected, this only affects the labels. If "bin" or "quantile" is selected, then this also affects the categories that col is applied to. If "bin" col_method is selected, the vector should start with -Inf and finish with Inf. If "quantile" col_method is selected, the vector should start with 0 and finish with 1.
#' @param col_breaks_n For a numeric col variable, the desired number of intervals on the col scale.
#' @param col_intervals_left For a numeric col variable of "bin" or "quantile" col_method, TRUE or FALSE of whether bins or quantiles are to be cut left-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify col scale labels. Defaults to snakecase::to_sentence_case for categorical col variables and scales::label_comma() for numeric. Use function(x) x to keep labels untransformed.
#' @param col_legend_bottom TRUE or FALSE of whether to position the legend horizontally on the bottom. Defaults to FALSE.
#' @param col_legend_mobile TRUE or FALSE of whether to position the legend for mobile vertically on the bottom. Defaults to FALSE.
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_method The method of coling features, either "bin", "quantile", "continuous", or "factor". If numeric, defaults to "continuous".
#' @param col_na_rm TRUE or FALSE of whether to include col NA values. Defaults to FALSE.
#' @param col_rev TRUE or FALSE of whether the col scale is reversed. Defaults to FALSE.
#' @param col_title col title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use function(x) x to keep labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
#' library(palmerpenguins)
#'
#' gg_blank(
#'   penguins,
#'   x = bill_length_mm,
#'   y = body_mass_g
#' )
#'
#' gg_blank(
#'   penguins,
#'   x = bill_length_mm,
#'   y = body_mass_g,
#'   col = sex,
#' )
#'
#' gg_blank(
#'   penguins,
#'   x = bill_length_mm,
#'   y = body_mass_g,
#'   facet = species
#' )
#'
#' gg_blank(
#'   penguins,
#'   x = bill_length_mm,
#'   y = body_mass_g,
#'   col = sex,
#'   facet = species
#' )
#'
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
                     tooltip = NULL,
                     palette = NULL,
                     palette_na = "#7F7F7F",
                     position = "identity",
                     stat = "identity",
                     alpha = 1,
                     size = 0.5,
                     width = NULL,
                     bins = 40,
                     ...,
                     title = NULL,
                     subtitle = NULL,
                     x_balance = FALSE,
                     x_breaks = NULL,
                     x_breaks_n = NULL,
                     x_breaks_width = NULL,
                     x_expand = NULL,
                     x_labels = NULL,
                     x_limits = NULL,
                     x_na_rm = FALSE,
                     x_oob = scales::oob_keep,
                     x_rev = FALSE,
                     x_title = NULL,
                     x_zero = NULL,
                     y_balance = FALSE,
                     y_breaks = NULL,
                     y_breaks_n = NULL,
                     y_breaks_width = NULL,
                     y_expand = NULL,
                     y_labels = NULL,
                     y_limits = NULL,
                     y_na_rm = FALSE,
                     y_oob = scales::oob_keep,
                     y_rev = FALSE,
                     y_title = NULL,
                     y_zero = TRUE,
                     col_breaks = NULL,
                     col_breaks_n = NULL,
                     col_intervals_left = TRUE,
                     col_labels = NULL,
                     col_legend_bottom = FALSE,
                     col_legend_none = FALSE,
                     col_legend_mobile = FALSE,
                     col_legend_ncol = NULL,
                     col_legend_nrow = NULL,
                     col_method = NULL,
                     col_na_rm = FALSE,
                     col_rev = FALSE,
                     col_title = NULL,
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

  tooltip <- rlang::enquo(tooltip)

  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required")
  # if (rlang::quo_is_null(x)) rlang::abort("x is required")
  # if (rlang::quo_is_null(y)) rlang::abort("y is required")
  if (!rlang::quo_is_null(col)) rlang::inform(c("i" = "Note in {ggblanket}, the {ggplot2} fill aesthetic inherits from col"))
  # rlang::inform(c("i" = "Note {ggblanket} gg_bar uses the {ggplot2} geom_blank function"))
  # if (is.null(position)) rlang::inform(c("i" = "Note {ggblanket} gg_bar uses a default of 'dodge2', where {ggplot2} uses a default of 'stack'"))

  ###ungroup
  data <- dplyr::ungroup(data)

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
  if (is.null(width)) {
    if (is.factor(rlang::eval_tidy(x, data)) | is.character(rlang::eval_tidy(x, data)) |
        is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {

      width <- 0.75
    }
  }

  ##col scale
  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(palette)) palette <-  pal_viridis_reorder(1)
    else palette <- palette[1]

    if (is.null(col_breaks)) col_breaks <- ggplot2::waiver()

    col_scale <- ggplot2::scale_colour_manual(
      values = palette,
      na.value = palette_na, #check
      aesthetics = c("col", "fill")
    )
  }
  else if (rlang::is_null(col_method)) {
    if (!is.numeric(rlang::eval_tidy(col, data))) col_method <- "factor"
    else if (is.numeric(rlang::eval_tidy(col, data))) col_method <- "continuous"
  }

  if (!rlang::quo_is_null(col)) {
    if (rlang::is_null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col))

    if (col_method == "continuous") {
      if (is.null(col_breaks_n)) {
        if (col_legend_bottom) col_breaks_n <- 3
        if (!col_legend_bottom) col_breaks_n <- 4
      }
      if (rlang::is_null(palette)) palette <- viridis::viridis(100)
      if (rlang::is_null(col_breaks)) col_breaks <- pretty(rlang::eval_tidy(col, data), col_breaks_n)
      if (rlang::is_null(col_labels)) col_labels <- scales::label_comma()
    }
    else if (col_method %in% c("quantile", "bin", "factor")) {
      if (col_method %in% c("quantile", "bin")) {
        if (is.null(col_breaks_n)) col_breaks_n <- 4
        if (col_method == "quantile") {
          if (rlang::is_null(col_breaks)) col_breaks <- seq(0, 1, 1 / col_breaks_n)
          col_breaks <- stats::quantile(rlang::eval_tidy(col, data), probs = col_breaks, na.rm = TRUE)
          if (anyDuplicated(col_breaks) > 0) stop("col_breaks do not provide unique breaks")
        }
        else if (col_method == "bin") {
          if (rlang::is_null(col_breaks)) col_breaks <- pretty(rlang::eval_tidy(col, data), col_breaks_n)
        }

        if (rlang::is_null(col_labels)) col_labels <- scales::label_comma()

        if (is.vector(col_labels)) {
          santoku_labeller <- santoku::lbl_dash(
            symbol = "\u2013", first = "<{r}", last = "\u2265{r}")
        }
        else {
          santoku_labeller <- santoku::lbl_dash(
            fmt = col_labels,
            symbol = "\u2013", first = "<{r}", last = "\u2265{r}")

          col_labels <- ggplot2::waiver()
        }

        data <- data %>%
          dplyr::mutate(dplyr::across(
            !!col,
            ~ santoku::chop(
              .x,
              breaks = col_breaks,
              left = TRUE,
              close_end = TRUE,
              drop = FALSE,
              labels = santoku_labeller
            )
          ))

        col_n <- length(col_breaks) - 1
        if (rlang::is_null(palette)) palette <- pal_viridis_reorder(col_n)
        else palette <- palette[1:col_n]
      }
      else if (col_method == "factor") {
        if (is.factor(rlang::eval_tidy(col, data)) & !rlang::is_null(levels(rlang::eval_tidy(col, data)))) {
          col_n <- length(levels(rlang::eval_tidy(col, data)))
        }
        else col_n <- length(unique(rlang::eval_tidy(col, data)))

        if (rlang::is_null(palette)) palette <- pal_d3_reorder(col_n)
        else palette <- palette[1:col_n]

        if (rlang::is_null(col_labels)) col_labels <- snakecase::to_sentence_case
      }
    }

    col_title_position <- ifelse(col_title == "", "right", "top")

    if (col_method == "continuous") {
      col_scale <- ggplot2::scale_colour_gradientn(
        colors = palette,
        labels = col_labels,
        breaks = col_breaks,
        na.value = palette_na,
        name = col_title,
        aesthetics = c("col", "fill"),
        guide = ggplot2::guide_colorbar(title.position = col_title_position)
      )
    }
    else if (col_method %in% c("quantile", "bin", "factor")) {
      if (is.numeric(rlang::eval_tidy(y, data)) |
          lubridate::is.Date(rlang::eval_tidy(y, data))) {
        if (col_method == "factor") col_legend_rev <- FALSE
        else if (col_method %in% c("bin", "quantile") & col_legend_bottom) col_legend_rev <- FALSE
        else col_legend_rev <- TRUE
      }
      else if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
        if (col_method == "factor") col_legend_rev <- TRUE
        else if (col_method %in% c("bin", "quantile") & col_legend_bottom) col_legend_rev <- TRUE
        else col_legend_rev <- FALSE
        palette <- rev(palette) #there was 2 rev palettes here ??
      }
      else col_legend_rev <- FALSE

      if (is.null(col_breaks)) col_breaks <- ggplot2::waiver()

      col_scale <- ggplot2::scale_colour_manual(
        values = palette,
        breaks = col_breaks,
        labels = col_labels,
        na.value = palette_na,
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
  else if (!rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
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
      ggplot2::aes(text = !!tooltip),
      width = width,
      alpha = alpha,
      size = size,
      position = position,
      stat = stat,
      bins = bins,
      ...
    )

  if (!rlang::quo_is_null(facet)) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(!!facet),
        labeller = ggplot2::as_labeller(facet_labels),
        scales = facet_scales,
        ncol = facet_ncol,
        nrow = facet_nrow
      )
  }

  ###x scale where y is NULL #xscale1
  if (rlang::quo_is_null(y)) {
    if (is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data))) {
      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
      if (rlang::is_null(x_labels)) x_labels <- snakecase::to_sentence_case

      x_scale <- ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
    }
    else {
      if (facet_scales %in% c("fixed", "free_y")) {
        if (is.null(x_zero)) {
          if ((is.numeric(rlang::eval_tidy(x, data)) |
               lubridate::is.Date(rlang::eval_tidy(x, data))) &
              !(is.numeric(rlang::eval_tidy(y, data)) |
                lubridate::is.Date(rlang::eval_tidy(y, data))))
            x_zero <- FALSE
          else x_zero <- TRUE
        }

        x_vctr <- dplyr::pull(data, !!x)
        x_min <- min(x_vctr, na.rm = TRUE)
        x_max <- max(x_vctr, na.rm = TRUE)

        if ((x_min < 0 & x_max > 0)) x_zero <- FALSE

        if (rlang::is_null(x_breaks)) {
          x_min_max <- c(x_min, x_max)
          if (x_zero) x_min_max <- c(0, x_min_max)
          if (x_balance) x_min_max <- c(-x_min_max, x_min_max)

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
  if (rlang::quo_is_null(x)) {
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

        if ((y_min < 0 & y_max > 0)) y_zero <- FALSE

        if (rlang::is_null(y_breaks)) {
          y_min_max <- c(y_min, y_max)
          if (y_zero) y_min_max <- c(0, y_min_max)
          if (y_balance) y_min_max <- c(-y_min_max, y_min_max)

          if (!rlang::is_null(y_breaks_width)) {
            y_breaks <- scales::fullseq(y_min_max, size = y_breaks_width)
          }
          else {
            if (rlang::is_null(y_breaks_n)) {
              y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 2)
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
        if (is.null(x_zero)) {
          if ((is.numeric(rlang::eval_tidy(x, data)) |
               lubridate::is.Date(rlang::eval_tidy(x, data))) &
              !(is.numeric(rlang::eval_tidy(y, data)) |
                lubridate::is.Date(rlang::eval_tidy(y, data))))
            x_zero <- FALSE
          else x_zero <- TRUE
        }

        x_temp_data <- layer_data %>%
          dplyr::select(tidyselect::matches(stringr::regex("^x$|^xmin$|^xmax$|^xend$|^outliers$")))

        if (any(stringr::str_detect(names(x_temp_data), stringr::regex("^outliers$")))) {
          x_temp_data <- x_temp_data %>%
            tidyr::unnest_longer(col = .data$outliers)
        }

        x_vctr <- x_temp_data %>%
          tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "x") %>%
          dplyr::pull(.data$x)

        if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
          x_vctr <- as.Date(x_vctr, origin = "1970-01-01")
        }

        x_min <- min(x_vctr, na.rm = TRUE)
        x_max <- max(x_vctr, na.rm = TRUE)

        if ((x_min < 0 & x_max > 0)) x_zero <- FALSE

        if (rlang::is_null(x_breaks)) {
          x_min_max <- c(x_min, x_max)
          if (x_zero) x_min_max <- c(0, x_min_max)
          if (x_balance) x_min_max <- c(-x_min_max, x_min_max)

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

  ###y scale where x not NULL #yscale2
  if (!rlang::quo_is_null(x)) {
    if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
      if (rlang::is_null(y_labels)) y_labels <- snakecase::to_sentence_case

      y_scale <- ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
    }
    else {
      if (facet_scales %in% c("fixed", "free_x")) {
        y_temp_data <- layer_data %>%
          dplyr::select(tidyselect::matches(stringr::regex("^y$|^ymin$|^ymax$|^yend$|^outliers$")))

        if (any(stringr::str_detect(names(y_temp_data), stringr::regex("^outliers$")))) {
          y_temp_data <- y_temp_data %>%
            tidyr::unnest_longer(col = .data$outliers)
        }

        y_vctr <- y_temp_data %>%
          tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "y") %>%
          dplyr::pull(.data$y)

        if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
          y_vctr <- as.Date(y_vctr, origin = "1970-01-01")
        }

        y_min <- min(y_vctr, na.rm = TRUE)
        y_max <- max(y_vctr, na.rm = TRUE)

        if ((y_min < 0 & y_max > 0)) y_zero <- FALSE

        if (rlang::is_null(y_breaks)) {
          y_min_max <- c(y_min, y_max)
          if (y_zero) y_min_max <- c(0, y_min_max)
          if (y_balance) y_min_max <- c(-y_min_max, y_min_max)

          if (!rlang::is_null(y_breaks_width)) {
            y_breaks <- scales::fullseq(y_min_max, size = y_breaks_width)
          }
          else {
            if (rlang::is_null(y_breaks_n)) {
              y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 2)
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

  ###titles
  if (rlang::is_null(x_title) & !rlang::quo_is_null(x)) x_title <- snakecase::to_sentence_case(rlang::as_name(x))
  if (rlang::is_null(y_title) & !rlang::quo_is_null(y)) y_title <- snakecase::to_sentence_case(rlang::as_name(y))

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
    ggplot2::coord_cartesian(clip = "off") +
    col_scale

  if (col_legend_bottom) {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.position = "bottom")
  }
  if (col_legend_mobile) {
    plot <- plot +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.direction = "vertical")
  }
  if (col_legend_none | rlang::quo_is_null(col)) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }

  return(plot)
}

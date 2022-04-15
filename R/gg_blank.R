#' @title Blank ggplot.
#'
#' @description Blank ggplot.
#' @param data A data frame in a structure to be plotted untransformed. Required input.
#' @param x Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or POSIXt). Required input.
#' @param y Unquoted numeric variable to be on the y scale. Required input.
#' @param dye Unquoted variable to dye and fill by.
#' @param facet Unquoted categorical variable to facet by.
#' @param tooltip Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(..., tooltip = "text").
#' @param position How overlapping geom's should be positioned with a character string (e.g."identity", "dodge", "dodge2", "fill"), or a function (e.g. ggplot2::position_*()).
#' @param stat
#' @param pal Character vector of hex codes.
#' @param pal_na The hex code or name of the NA dye to be used.
#' @param alpha_point Opacity of any points.
#' @param alpha_line Opacity of any lines.
#' @param alpha_fill Opacity of any polygons.
#' @param size_line Size of any lines. Defaults to 1.
#' @param size_point Size of any points. Defaults to 1.5.
#' @param size_width Width of any polygons. Defaults to 0.75 if x or y is categorical. Otherwise NULL.
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
#' @param y_breaks For a numeric or date x variable, a vector of breaks for the x axis. Note the x_limits will be the min and max of this vector.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions.
#' @param y_labels A function or named vector to modify y scale labels. Use function(x) x to keep labels untransformed.
#' @param y_limits For a numeric or date y variable, a vector of length 2 to determine the limits of the y axis. Use c(NA, NA) for the min and max of the y variable.
#' @param y_na_rm TRUE or FALSE of whether to remove y NA values. Defaults to FALSE.
#' @param y_oob scales function for how to deal with out-of-bounds values on the y axis. See ggplot2::scale_y_continuous for further information.
#' @param y_rev For a categorical x variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param dye_breaks For a numeric dye variable, a vector of breaks. If "continuous" dye_method is selected, this only affects the labels. If "bin" or "quantile" is selected, then this also affects the categories that dye is applied to. If "bin" dye_method is selected, the vector should start with -Inf and finish with Inf. If "quantile" dye_method is selected, the vector should start with 0 and finish with 1.
#' @param dye_breaks_n For a numeric dye variable, the desired number of intervals on the dye scale.
#' @param dye_intervals_left For a numeric dye variable of "bin" or "quantile" dye_method, TRUE or FALSE of whether bins or quantiles are to be cut left-closed. Defaults to TRUE.
#' @param dye_labels A function or named vector to modify dye scale labels. Defaults to snakecase::to_sentence_case for categorical dye variables and scales::label_comma() for numeric. Use function(x) x to keep labels untransformed.
#' @param dye_legend_bottom TRUE or FALSE of whether to position the legend horizontally on the bottom. Defaults to FALSE.
#' @param dye_legend_mobile TRUE or FALSE of whether to position the legend for mobile vertically on the bottom. Defaults to FALSE.
#' @param dye_legend_none TRUE or FALSE of whether to remove the legend.
#' @param dye_legend_ncol The number of dyeumns for the legend elements.
#' @param dye_legend_nrow The number of rows for the legend elements.
#' @param dye_method The method of dyeing features, either "bin", "quantile", "continuous", or "factor". If numeric, defaults to "continuous".
#' @param dye_na_rm TRUE or FALSE of whether to include dye NA values. Defaults to FALSE.
#' @param dye_rev TRUE or FALSE of whether the dye scale is reversed. Defaults to FALSE.
#' @param dye_title dye title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use function(x) x to keep labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet NA values. Defaults to FALSE.
#' @param facet_ncol The number of dyeumns of facetted plots.
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
#'   dye = sex,
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
#'   dye = sex,
#'   facet = species
#' )
#'
gg_blank <- function(data = NULL,
                   x = NULL,
                   y = NULL,
                   dye = NULL,
                   facet = NULL,
                   tooltip = NULL,
                   position = "identity",
                   stat = "identity",
                   pal = NULL,
                   pal_na = "#7F7F7F",
                   alpha_fill = 1,
                   alpha_line = 1,
                   alpha_point = 1,
                   size_line = 0.5,
                   size_point = 1.5,
                   size_width = NULL,
                   title = NULL,
                   subtitle = NULL,
                   x_balance = FALSE,
                   x_breaks = NULL,
                   x_breaks_n = NULL,
                   x_expand = NULL,
                   x_labels = NULL,
                   x_limits = NULL,
                   x_na_rm = FALSE,
                   x_oob = scales::oob_keep,
                   x_rev = FALSE,
                   x_title = NULL,
                   x_zero = FALSE,
                   y_balance = FALSE,
                   y_breaks = NULL,
                   y_breaks_n = NULL,
                   y_expand = NULL,
                   y_labels = NULL,
                   y_limits = NULL,
                   y_na_rm = FALSE,
                   y_oob = scales::oob_keep,
                   y_rev = FALSE,
                   y_title = NULL,
                   y_zero = FALSE,
                   dye_breaks = NULL,
                   dye_breaks_n = NULL,
                   dye_intervals_left = TRUE,
                   dye_labels = NULL,
                   dye_legend_bottom = FALSE,
                   dye_legend_none = FALSE,
                   dye_legend_mobile = FALSE,
                   dye_legend_ncol = NULL,
                   dye_legend_nrow = NULL,
                   dye_method = NULL,
                   dye_na_rm = FALSE,
                   dye_rev = FALSE,
                   dye_title = NULL,
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
  dye <- rlang::enquo(dye)
  facet <- rlang::enquo(facet)
  tooltip <- rlang::enquo(tooltip)

  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required")
  # if (rlang::quo_is_null(x)) rlang::abort("x is required")
  # if (rlang::quo_is_null(y)) rlang::abort("y is required")
  if (!rlang::quo_is_null(dye)) rlang::inform(c("i" = "Note in {ggbilly} dye refers to both the {ggplot2} dye & fill aesthetics"))
  rlang::inform(c("i" = "Note {ggbilly} gg_bar uses the {ggplot2} geom_blank function"))
  if (is.null(position)) rlang::inform(c("i" = "Note {ggbilly} gg_bar uses a default of 'dodge2', where {ggplot2} uses a default of 'stack'"))

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

  ###dye process
  if (!rlang::quo_is_null(dye)) {

    if (dye_na_rm) {
      data <- data %>%
        dplyr::filter(!is.na(!!dye))
    }

    if (is.logical(rlang::eval_tidy(dye, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!dye, ~ factor(.x, levels = c("TRUE", "FALSE"))))
    }

    if (is.factor(rlang::eval_tidy(dye, data)) | is.character(rlang::eval_tidy(dye, data))) {
      if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
        if (!dye_rev) {
          data <- data %>%
            dplyr::mutate(dplyr::across(!!dye, ~ forcats::fct_rev(.x)))
        }
      }
      else if (dye_rev) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!dye, ~ forcats::fct_rev(.x)))
      }
    }
  }

  ###theme
  if (rlang::is_null(theme)) {
    y_grid <- ifelse(is.numeric(rlang::eval_tidy(y, data)) |
                       lubridate::is.Date(rlang::eval_tidy(y, data)) |
                       lubridate::is.POSIXt(rlang::eval_tidy(y, data)), TRUE, FALSE)

    x_grid <- ifelse(is.numeric(rlang::eval_tidy(x, data)) |
                       lubridate::is.Date(rlang::eval_tidy(x, data)) |
                       lubridate::is.POSIXt(rlang::eval_tidy(x, data)), TRUE, FALSE)

    theme <- gg_theme(y_grid = y_grid, x_grid = x_grid)
  }

  ###width
  if (is.null(size_width)) {
    if (is.factor(rlang::eval_tidy(x, data)) | is.character(rlang::eval_tidy(x, data)) |
        is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {

      size_width <- 0.75
    }
  }

  ##dye scale
  if (rlang::quo_is_null(dye)) {
    if (rlang::is_null(pal)) pal <-  pal_viridis_reorder(1)
    else pal <- pal[1]

    # alpha <- alpha_point #check
    alpha <- alpha_line #check

    pal_col <- scales::alpha(pal, alpha = alpha)
    pal_na_col <- scales::alpha(pal_na, alpha = alpha)
    pal_fill <- scales::alpha(pal, alpha = alpha_fill)
    pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)

    dye_scale <- ggplot2::scale_colour_manual(
      values = pal_col,
      drop = FALSE,
      labels = NULL,
      na.value = pal_na_col, #check
      name = NULL,
      aesthetics = c("col", "fill")
    )
  }
  else if (rlang::is_null(dye_method)) {
    if (!is.numeric(rlang::eval_tidy(dye, data))) dye_method <- "factor"
    else if (is.numeric(rlang::eval_tidy(dye, data))) dye_method <- "continuous"
  }

  if (!rlang::quo_is_null(dye)) {
    if (rlang::is_null(dye_title)) dye_title <- snakecase::to_sentence_case(rlang::as_name(dye))

    if (dye_method == "continuous") {
      if (is.null(dye_breaks_n)) dye_breaks_n <- 2
      if (rlang::is_null(pal)) pal <- viridis::viridis(100)
      if (rlang::is_null(dye_breaks)) dye_breaks <- pretty(rlang::eval_tidy(dye, data), dye_breaks_n)
      if (rlang::is_null(dye_labels)) dye_labels <- scales::label_comma()
    }
    else if (dye_method %in% c("quantile", "bin", "factor")) {
      if (dye_method %in% c("quantile", "bin")) {
        if (is.null(dye_breaks_n)) dye_breaks_n <- 4
        if (dye_method == "quantile") {
          if (rlang::is_null(dye_breaks)) dye_breaks <- seq(0, 1, 1 / dye_breaks_n)
          dye_breaks <- stats::quantile(rlang::eval_tidy(dye, data), probs = dye_breaks, na.rm = TRUE)
          if (anyDuplicated(dye_breaks) > 0) stop("dye_breaks do not provide unique breaks")
        }
        else if (dye_method == "bin") {
          if (rlang::is_null(dye_breaks)) dye_breaks <- pretty(rlang::eval_tidy(dye, data), dye_breaks_n)
        }

        if (rlang::is_null(dye_labels)) dye_labels <- scales::label_comma()

        if (is.vector(dye_labels)) {
          santoku_labeller <- santoku::lbl_dash(
            symbol = "\u2013", first = "<{r}", last = "\u2265{r}")
        }
        else {
          santoku_labeller <- santoku::lbl_dash(
            fmt = dye_labels,
            symbol = "\u2013", first = "<{r}", last = "\u2265{r}")

          dye_labels <- ggplot2::waiver()
        }

        data <- data %>%
          dplyr::mutate(dplyr::across(
            !!dye,
            ~ santoku::chop(
              .x,
              breaks = dye_breaks,
              left = TRUE,
              close_end = TRUE,
              drop = FALSE,
              labels = santoku_labeller
            )
          ))

        dye_n <- length(dye_breaks) - 1
        if (rlang::is_null(pal)) pal <- pal_viridis_reorder(dye_n)
        else pal <- pal[1:dye_n]
      }
      else if (dye_method == "factor") {
        if (is.factor(rlang::eval_tidy(dye, data)) & !rlang::is_null(levels(rlang::eval_tidy(dye, data)))) {
          dye_n <- length(levels(rlang::eval_tidy(dye, data)))
        }
        else dye_n <- length(unique(rlang::eval_tidy(dye, data)))

        if (rlang::is_null(pal)) pal <- pal_d3_reorder(dye_n)
        else pal <- pal[1:dye_n]

        if (rlang::is_null(dye_labels)) dye_labels <- snakecase::to_sentence_case
      }
    }

    # alpha <- alpha_point #check
    alpha <- alpha_line #check

    pal_col <- scales::alpha(pal, alpha = alpha)
    pal_na_col <- scales::alpha(pal_na, alpha = alpha)
    pal_fill <- scales::alpha(pal, alpha = alpha_fill)
    pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)

    dye_title_position <- ifelse(dye_title == "", "right", "top")

    if (dye_method == "continuous") {
      dye_scale <- ggplot2::scale_colour_gradientn(
        colors = pal_col,
        labels = dye_labels,
        breaks = dye_breaks,
        na.value = pal_na_col,
        name = dye_title,
        aesthetics = c("dye", "fill"),
        guide = ggplot2::guide_colorbar(title.position = dye_title_position)
      )
    }
    else if (dye_method %in% c("quantile", "bin", "factor")) {
      if (is.numeric(rlang::eval_tidy(y, data)) |
          lubridate::is.Date(rlang::eval_tidy(y, data)) |
          lubridate::is.POSIXt(rlang::eval_tidy(y, data))) {
        if (dye_method == "factor") dye_legend_rev <- FALSE
        else if (dye_method %in% c("bin", "quantile") & dye_legend_bottom) dye_legend_rev <- FALSE
        else dye_legend_rev <- TRUE
      }
      else if (is.factor(rlang::eval_tidy(y, data)) | is.character(rlang::eval_tidy(y, data))) {
        if (dye_method == "factor") dye_legend_rev <- TRUE
        else if (dye_method %in% c("bin", "quantile") & dye_legend_bottom) dye_legend_rev <- TRUE
        else dye_legend_rev <- FALSE
        pal_col <- rev(pal_col)
        pal_fill <- rev(pal_fill)
      }
      else dye_legend_rev <- FALSE

      dye_scale <- ggplot2::scale_colour_manual(
        values = pal_col,
        drop = FALSE,
        labels = dye_labels,
        na.value = pal_na_col, #check
        name = dye_title,
        aesthetics = c("col", "fill"),
        guide = ggplot2::guide_legend(
          reverse = dye_legend_rev,
          title.position = dye_title_position,
          ncol = dye_legend_ncol,
          nrow = dye_legend_nrow,
          byrow = TRUE
        )
      )
    }
  }

  ###plot
  if (!rlang::quo_is_null(dye)) {
    plot <- data %>%
      ggplot2::ggplot(mapping = ggplot2::aes(
        x = !!x,
        y = !!y,
        col = !!dye,
        fill = !!dye #check
      ))
  }
  else {
    plot <- data %>%
      ggplot2::ggplot(mapping = ggplot2::aes(
        x = !!x,
        y = !!y,
        col = "1",
        fill = "1" #check
      ))
  }

  plot <- plot +
    ggplot2::geom_blank(
      ggplot2::aes(text = !!tooltip),
      # alpha = alpha_point,  #check
      # alpha = alpha_line,  #check
      alpha = alpha_fill, #check
      # size = size_point, #check
      size = size_line, #check
      width = size_width, #check
      position = position,
      stat = stat
    ) +
    dye_scale

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

  if (dye_legend_bottom) {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.position = "bottom")
  }
  if (dye_legend_mobile) {
    plot <- plot +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.direction = "vertical")
  }
  if (dye_legend_none) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }

  #build plot
  build_data <- ggplot2::ggplot_build(plot)$data[[1]]

  ###x scale
  # if (is.null(x_zero)) {
  #   if ((!is.character(rlang::eval_tidy(x, data)) & !is.factor(rlang::eval_tidy(x, data))) &
  #       (!is.character(rlang::eval_tidy(y, data)) & !is.factor(rlang::eval_tidy(y, data)))) {
  #     x_zero <- FALSE
  #   }
  #   else x_zero <- TRUE
  # }

  if (is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data))) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- snakecase::to_sentence_case

    x_scale <- ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  if (is.numeric(rlang::eval_tidy(x, data)) |
      lubridate::is.Date(rlang::eval_tidy(x, data)) |
      lubridate::is.POSIXt(rlang::eval_tidy(x, data))) {

    if (facet_scales %in% c("fixed", "free_y")) {

      if (any(stringr::str_detect(names(build_data), "xmin"))) {
        x_min <- min(build_data$xmin, na.rm = TRUE)
      } else  x_min <- min(build_data$x, na.rm = TRUE)

      if (any(stringr::str_detect(names(build_data), "xmax"))) {
        x_max <- max(build_data$xmax, na.rm = TRUE)
      } else  x_max <- max(build_data$x, na.rm = TRUE)

      if ((x_min < 0 & x_max > 0)) x_zero <- FALSE

      if (rlang::is_null(x_breaks)) {
        x_min_max <- c(x_min, x_max)
        if (x_zero) x_min_max <- c(0, x_min_max)
        if (x_balance) x_min_max <- c(-x_min_max, x_min_max)
        if (rlang::is_null(x_breaks_n)) x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 2)
        x_breaks <- pretty(x_min_max, n = x_breaks_n)
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
      if (is.numeric(rlang::eval_tidy(x, data))) x_labels <- scales::label_comma()
      else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date()
      else x_labels <- ggplot2::waiver()
    }

    if (is.numeric(rlang::eval_tidy(x, data))) {
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
    else if (lubridate::is.POSIXt(rlang::eval_tidy(x, data))) {
      x_scale <- ggplot2::scale_x_datetime(
        breaks = x_breaks,
        limits = x_limits,
        expand = x_expand,
        labels = x_labels,
        oob = x_oob
      )
    }

    if (rlang::is_null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x))
  }

  ###y scale
  if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- snakecase::to_sentence_case

    y_scale <- ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
  }
  if (is.numeric(rlang::eval_tidy(y, data)) |
      lubridate::is.Date(rlang::eval_tidy(y, data)) |
      lubridate::is.POSIXt(rlang::eval_tidy(y, data))) {

    if (facet_scales %in% c("fixed", "free_x")) {

      if (any(stringr::str_detect(names(build_data), "ymin"))) {
        y_min <- min(build_data$ymin, na.rm = TRUE)
      } else  y_min <- min(build_data$y, na.rm = TRUE)

      if (any(stringr::str_detect(names(build_data), "ymax"))) {
        y_max <- max(build_data$ymax, na.rm = TRUE)
      } else  y_max <- max(build_data$y, na.rm = TRUE)

      if ((y_min < 0 & y_max > 0)) y_zero <- FALSE

      if (rlang::is_null(y_breaks)) {
        y_min_max <- c(y_min, y_max)
        if (y_zero) y_min_max <- c(0, y_min_max)
        if (y_balance) y_min_max <- c(-y_min_max, y_min_max)
        if (rlang::is_null(y_breaks_n)) y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
        y_breaks <- pretty(y_min_max, n = y_breaks_n)
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
      if (is.numeric(rlang::eval_tidy(y, data))) y_labels <- scales::label_comma()
      else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date()
      else y_labels <- ggplot2::waiver()
    }

    if (is.numeric(rlang::eval_tidy(y, data))) {
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
    else if (lubridate::is.POSIXt(rlang::eval_tidy(y, data))) {
      y_scale <- ggplot2::scale_y_datetime(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = y_oob
      )
    }

    if (rlang::is_null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y))
  }

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      caption = caption
    ) +
    theme +
    x_scale +
    y_scale +
    ggplot2::coord_cartesian(clip = "off")

  if (rlang::quo_is_null(dye)) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }


  return(plot)
}

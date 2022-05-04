#' @title Sf ggplot.
#'
#' @description Create a sf plot with a wrapper around the ggplot2::geom_sf function.
#' @param data A sf object.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param group Unquoted group aesthetic variable.
#' @param stat Statistical transformation. A character string (e.g. "identity").
#' @param position Position adjustment. Either a character string (e.g."identity"), or a function (e.g. ggplot2::position_identity()).
#' @param pal Colours to use. A character vector of hex codes (or names).
#' @param pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1.
#' @param size Size. A number 0 upwards.
#' @param ... Other arguments passed to the relevant ggplot2::geom_* function.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param col_breaks A vector of breaks. For a categorical col variable, this links pal values with col variable values dropping those not used. For a numeric variable where col_intervals is NULL, this only affects the labels on the legend.
#' @param col_breaks_n For a numeric variable where col_intervals is NULL, an integer guiding the number of breaks, as calculated by the pretty function.
#' @param col_breaks_width For a numeric variable, the width of breaks, as calculated by the scales::fullseq function.
#' @param col_intervals A function to cut or chop the numeric variable into intervals, including in rlang lambda format (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param col_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformation. If categorical, accepts a named vector (e.g. c(value = "label", ...)). Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector of limits. For a categorical col variable, this links pal values with col variable values keeping those not used. For a numeric variable where col_intervals is NULL, this will make all values outside the limits coloured NA.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "r" for right, "b" for bottom, "t" for top, "l" for left, or "m" for a mobile-friendly legend.
#' @param col_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param facet_intervals A function to cut or chop the numeric variable into intervals, including in rlang lambda format (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param facet_labels A function to format the scale labels, including in rlang lambda format. Use ~.x to remove default transformation. If categorical, accepts a named vector (e.g. c(value = "label", ...)).
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
#'
gg_sf <- function(data = NULL,
                  col = NULL,
                  facet = NULL,
                  group = NULL,
                  stat = "sf",
                  position = "identity",
                  pal = NULL,
                  pal_na = "#7F7F7F",
                  alpha = 0.9,
                  size = NULL,
                  ...,
                  title = NULL,
                  subtitle = NULL,
                  coord = ggplot2::coord_sf(),
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
                  caption = NULL,
                  theme = NULL) {

  #quote
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  group <- rlang::enquo(group)

  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required")
  if (!rlang::quo_is_null(col)) rlang::inform(c("i" = "{ggblanket} merges col and fill aesthetics into a single col aesthetic"))
  if (!rlang::quo_is_null(facet)) rlang::inform(c("i" = "{ggblanket} treats faceting as an aesthetic"))
  if(!(requireNamespace("sf"))) stop("gg_sf() requires the sf package to be installed")

  ###ungroup
  data <- dplyr::ungroup(data)

  ###get default NULL values
  if (rlang::is_null(theme)) {
    theme <- gg_theme(void = TRUE)
  }

  sf_geometry <- sf::st_geometry_type(data)

  if (rlang::is_null(alpha)) {
    if (any(sf_geometry %in% c("POLYGON", "MULTIPOLYGON"))) {
      alpha <- 0.9
    }
    else alpha <- 1
  }

  if (rlang::is_null(size)) {
    if (any(sf_geometry %in% c("POINT", "MULTIPOINT"))) {
      size <- 1.5
    }
    else size <- 0.5
  }

  ###process plot data
  ###factorise logical, reverse for horizontal, and chop intervals
  if (!rlang::quo_is_null(col)) {

    if (is.logical(rlang::eval_tidy(col, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(.x, levels = c("TRUE", "FALSE"))))
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

        if (rlang::is_null(pal)) pal <- pal_viridis_reorder(col_n)
        else pal <- pal[1:col_n]

        if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
        else col_legend_rev <- TRUE

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

      if (is.factor(rlang::eval_tidy(col, data)) | is.character(rlang::eval_tidy(col, data))) {
        col_legend_rev <- FALSE
      }
      else if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
      else col_legend_rev <- TRUE

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

  plot <- plot +
    ggplot2::geom_sf(
      stat = stat,
      position = position,
      alpha = alpha,
      size = size,
      ...
    )

  if (!rlang::quo_is_null(facet)) {
    if (!rlang::is_null(facet_intervals)) {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(!!facet),
          scales = "fixed",
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    }
    else {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(!!facet),
          labeller = ggplot2::as_labeller(facet_labels),
          scales = "fixed",
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    }
  }

  #make the plot
  plot <- plot +
    col_scale +
    coord +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
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
  else if (col_legend_place == "m") {
    plot <- plot +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.direction = "vertical")
  }

  #return beautiful plot
  return(plot)
}

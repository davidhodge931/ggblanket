#' @title Sf ggplot.
#'
#' @description Create a sf plot with a wrapper around the ggplot2:: %>%  function.
#' @param data A sf object.
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
#' @param titles A function to format the x, y and col titles, including in rlang lambda format. Defaults to snakecase::to_sentence_case.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param col_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_continuous Type of colouring for a continuous variable. Either "gradient" or "steps". Defaults to "steps".
#' @param col_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels. Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector to determine the limits of the colour scale.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "b" for bottom, "r" for right, "t" for top, or "l" for left.
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_legend_place The place for the legend. "b" for bottom, "r" for right, "t" for top, or "l" for left. Defaults to "b".
#' @param col_rescale For a continuous col variable, a vector to rescale the pal non-linearly.
#' @param col_title Legend title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c("value" = "label", ...)).
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of "wrap".
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of "wrap".
#' @param facet_layout Whether the layout is to be "wrap" or "grid". If NULL and a single facet (or facet2) argument is provided, then defaults to "wrap". If NULL and both facet and facet2 arguments are provided, defaults to "grid".
#' @param caption Caption title string.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   library(ggplot2)
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#'   gg_sf(nc, col = AREA, col_legend_place = "b")
#' }
#'
gg_sf <- function(
    data = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    text = NULL,
    stat = "sf",
    position = "identity",
    pal = NULL,
    pal_na = "#7F7F7F",
    alpha = 0.9,
    #linewidth = 0.5,
    #size = 1.5,
    ...,
    titles = NULL,
    title = NULL,
    subtitle = NULL,
    coord = ggplot2::coord_sf(),
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
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_layout = NULL,
    caption = NULL,
    theme = NULL) {

  #quote
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)

  #stop, warn or message
  rlang::inform(c("i" = "For further ggblanket information, see https://davidhodge931.github.io/ggblanket/"), .frequency = "regularly", .frequency_id = "hello")
  if (rlang::is_null(data)) rlang::abort("data is required.")

  ###ungroup
  data <- dplyr::ungroup(data)

  #get classes
  x_character <- FALSE
  x_factor <- FALSE
  x_logical <- FALSE
  x_date <- FALSE
  x_datetime <- FALSE
  x_time <- FALSE
  x_numeric <- TRUE
  x_null <- FALSE

  y_character <- FALSE
  y_factor <- FALSE
  y_logical <- FALSE
  y_date <- FALSE
  y_datetime <- FALSE
  y_time <- FALSE
  y_numeric <- TRUE
  y_null <- FALSE

  col_character <- is.character(rlang::eval_tidy(col, data))
  col_factor <- is.factor(rlang::eval_tidy(col, data))
  col_logical <- is.logical(rlang::eval_tidy(col, data))
  col_numeric <- is.numeric(rlang::eval_tidy(col, data))
  col_null <- rlang::quo_is_null(col)

  facet_null <- rlang::quo_is_null(facet)
  facet2_null <- rlang::quo_is_null(facet2)

  ###process data for logical & horizontal
  if (!col_null) {
    if (col_logical) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(stringr::str_to_sentence(.x), levels = c("False", "True"))))
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

  ###get default NULL values
  if (rlang::is_null(alpha)) {
    if (any(sf::st_geometry_type(data) %in% c("POLYGON", "MULTIPOLYGON"))) {
      alpha <- 0.9
    }
    else alpha <- 1
  }

  if (rlang::is_null(theme)) {
    theme <- gg_theme(grid_v = TRUE, grid_h = TRUE, void = TRUE)
  }

  ###make plot
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

  plot <- plot +
    ggplot2::geom_sf(
      ggplot2::aes(text = !!text),
      stat = stat,
      position = position,
      alpha = alpha,
      #linewidth = linewidth,
      #size = size,
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
          scales = "fixed",
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet2),
          scales = "fixed",
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet, !!facet2),
          scales = "fixed",
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
          scales = "fixed",
          space = "fixed",
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!facet),
          scales = "fixed",
          space = "fixed",
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet2),
          scales = "fixed",
          space = "fixed",
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  ###make col scale
  if (col_null) {
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

    if (rlang::is_null(col_legend_place)) col_legend_place <- "n"
  }
  else if (!col_null) {
    if (rlang::is_null(col_title)) {
      if (rlang::is_null(titles)) col_title <- purrr::map_chr(rlang::as_name(col), snakecase::to_sentence_case)
      else col_title <- purrr::map_chr(rlang::as_name(col), titles)
    }
    col_title_position <- ifelse(col_title == "", "right", "top")

    if (rlang::is_null(col_legend_place)) {
      if (!facet_null &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet, data)))) {
        col_legend_place <- "n"
      }
      else if (!facet2_null &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet2, data)))) {
        col_legend_place <- "n"
      }
      else {
        col_legend_place <- "b"
      }
    }

    if (col_numeric) {
      col_min <- data %>% dplyr::pull(!!col) %>% min(na.rm = TRUE)
      col_max <- data %>% dplyr::pull(!!col) %>% max(na.rm = TRUE)

      if (!rlang::is_null(col_limits)) {
        if (is.na(col_limits)[1]) col_limits[1] <- col_min
        if (is.na(col_limits)[2]) col_limits[2] <- col_max
      }

      if (rlang::is_null(col_limits)) col_limits <- c(col_min, col_max)
      if (!rlang::is_null(col_include)) col_limits <- range(c(col_include, col_limits))

      if (rlang::is_null(pal)) pal <- viridis::viridis(10)
      if (rlang::is_null(col_labels)) col_labels <- scales::label_comma()

      if (col_continuous == "gradient") {
        if (rlang::is_null(col_breaks)) {
          if (col_legend_place %in% c("b", "t")) {
            col_breaks <- function(x) c(x, stats::median(x))
            draw_llim <- TRUE #should be FALSE
            draw_ulim <- FALSE
          }
          else if (col_legend_place %in% c("l", "r")) {
            col_breaks <- scales::breaks_pretty(4)
            draw_llim <- TRUE
            draw_ulim <- TRUE
          }
        }
        else {
          draw_llim <- TRUE
          draw_ulim <- TRUE
        }

        plot <- plot +
          ggplot2::scale_colour_gradientn(
            colors = pal,
            values = scales::rescale(col_rescale),
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_colourbar(
              title.position = col_title_position,
              draw.ulim = draw_ulim,
              draw.llim = draw_llim,
              ticks.colour = "#F1F3F5",
              reverse = col_legend_rev)) +
          ggplot2::scale_fill_gradientn(
            colors = pal,
            values = scales::rescale(col_rescale),
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_colourbar(
              title.position = col_title_position,
              draw.ulim = draw_ulim,
              draw.llim = draw_llim,
              ticks.colour = "#F1F3F5",
              reverse = col_legend_rev))
      }
      else if (col_continuous == "steps") {
        if (rlang::is_null(col_breaks)) {
          col_breaks <- scales::breaks_pretty(n = 4)
        }

        plot <- plot +
          ggplot2::scale_colour_stepsn(
            colors = pal,
            values = scales::rescale(col_rescale),
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_coloursteps(
              title.position = col_title_position,
              reverse = col_legend_rev)) +
          ggplot2::scale_fill_stepsn(
            colors = pal,
            values = scales::rescale(col_rescale),
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_coloursteps(
              title.position = col_title_position,
              reverse = col_legend_rev))
      }
    }
    else if (col_character | col_factor | col_logical) { #categorical col  
      if (!rlang::is_null(col_limits)) col_n <- length(col_limits)
      else if (!rlang::is_null(col_breaks)) col_n <- length(col_breaks)
      else {
        if (col_factor) {
          col_n <- length(levels(rlang::eval_tidy(col, data)))
        }
        else {
          col_unique <- unique(rlang::eval_tidy(col, data))
          col_n <- length(col_unique[!is.na(col_unique)])
        }
      }

      if (rlang::is_null(pal)) pal <- pal_d3_mix(col_n)
      else pal <- pal[1:col_n]

      if (y_numeric | y_date | y_datetime | y_time) {
        if (col_character | col_factor | col_logical) {
          col_legend_rev_auto <- FALSE
        }
        else if (col_legend_place %in% c("b", "t")) col_legend_rev_auto <- FALSE
        else col_legend_rev_auto <- TRUE
      }
      else if (y_character | y_factor | y_logical) {
        if (col_character | col_factor | col_logical) col_legend_rev_auto <- TRUE
        else if (col_legend_place %in% c("b", "t")) col_legend_rev_auto <- TRUE
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
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE)) +
        ggplot2::scale_fill_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev_auto,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE))
    }
  }

  #Add the xy scales and titles
  plot <- plot +
    coord +
    theme +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      col = col_title,
      fill = col_title,
      caption = caption
    )

  ###adjust the legend
  if (col_legend_place %in% c("b", "t")) {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal")

    if (col_numeric) {
      plot <- plot +
        ggplot2::theme(legend.key.width = grid::unit(0.66, "cm")) +
        ggplot2::theme(legend.text.align = 0.5)
    }

    if (col_legend_place == "b") {
      plot <- plot +
        ggplot2::theme(legend.position = "bottom")
    }
    else if (col_legend_place == "t") {
      plot <- plot +
        ggplot2::theme(legend.position = "top")
    }
  }
  else if (col_legend_place == "n" | col_null) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }
  else if (col_legend_place == "l") {
    plot <- plot +
      ggplot2::theme(legend.position = "left")
  }
  else if (col_legend_place == "r") {
    plot <- plot +
      ggplot2::theme(legend.position = "right")
  }

  plot <- plot +
    ggplot2::theme(legend.justification = "left")

  #return beautiful plot
  return(plot)
}


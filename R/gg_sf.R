#' @title Sf ggplot.
#'
#' @description Create a sf plot with a wrapper around the ggplot2::geom_sf function.
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
#' @param titles A function to format the x, y and col titles. Defaults to snakecase::to_sentence_case.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param x_grid TRUE or FALSE for vertical x gridlines. NULL guesses based on the classes of the x and y.
#' @param y_grid TRUE or FALSE of horizontal y gridlines. NULL guesses based on the classes of the x and y.
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
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed". Only applies where the facet layout is "grid" and facet scales are not "fixed".
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
    alpha = NULL,
    ...,
    titles = NULL,
    title = NULL,
    subtitle = NULL,
    coord = ggplot2::coord_sf(),
    x_grid = NULL,
    y_grid = NULL,
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
    facet_space = "fixed",
    facet_layout = NULL,
    caption = NULL,
    theme = gg_theme(void = TRUE)) {

  #quote
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

  #get default NULL values
  if (rlang::is_null(x_grid)) x_grid <- TRUE
  if (rlang::is_null(y_grid)) y_grid <- FALSE

  if (!rlang::is_null(col_legend_place)) {
    if (col_legend_place == "b") col_legend_place <- "bottom"
    if (col_legend_place == "t") col_legend_place <- "top"
    if (col_legend_place == "l") col_legend_place <- "left"
    if (col_legend_place == "r") col_legend_place <- "right"
    if (col_legend_place == "n") col_legend_place <- "none"
  }

  if (rlang::is_null(alpha)) {
    if (any(sf::st_geometry_type(data) %in% c("POLYGON", "MULTIPOLYGON"))) {
      alpha <- 0.9
    }
    else alpha <- 1
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
          cols = ggplot2::vars(!!facet),
          scales = "fixed",
          space = facet_space,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!facet_null & facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!facet),
          scales = "fixed",
          space = facet_space,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (facet_null & !facet2_null) {
      plot <- plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet2),
          scales = "fixed",
          space = facet_space,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  #Get layer data for x, y and col scales
  layer_data <- ggplot2::layer_data(plot)

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

      if (col_character | col_factor | col_logical) col_legend_rev_auto <- FALSE
      else if (col_legend_place %in% c("top", "bottom")) col_legend_rev_auto <- FALSE
      else col_legend_rev_auto <- TRUE

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
      colour = col_title,
      fill = col_title,
      caption = caption
    )

  if (!rlang::is_null(col_title)) {
    if (col_title == "") {
      plot <- plot +
        ggplot2::labs(colour = NULL, fill = NULL)
    }
  }

  #expand the limits if necessary
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
  if (!x_grid & !y_grid) {
    plot <- plot +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  else if (!x_grid & y_grid) {
    plot <- plot +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
  }
  else if (x_grid & !y_grid) {
    plot <- plot +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  #return beautiful plot
  return(plot)
}

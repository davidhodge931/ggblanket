#' @title Sf ggplot.
#'
#' @description Create a sf plot with a wrapper around the ggplot2:: %>%  function.
#' @param data A sf object.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable for a facet grid of facet by facet2 variables.
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
#' @param col_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param col_intervals A function to cut or chop the numeric variable into intervals (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels. Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector to determine the limits of the axis.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "b" for bottom, "r" for right, "t" for top, or "l" for left.
#' @param col_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c(value = "label", ...)).
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
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
    col_include = NULL,
    col_intervals = NULL,
    col_labels = NULL,
    col_legend_place = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_limits = NULL,
    col_title = NULL,
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    caption = NULL,
    theme = NULL) {

  #quote
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)

  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required.")
  if (rlang::is_null(titles)) rlang::inform(c("i" = "{ggblanket} converts unspecified titles using snakecase::to_sentence_case. Use titles = ~.x to leave unspecified titles as is, and/or specify individual titles manually using *_title arguments."))
  if (!rlang::quo_is_null(col)) rlang::inform(c("i" = "{ggblanket} merges col and fill aesthetics into a single col aesthetic."))
  if (!rlang::quo_is_null(facet)) rlang::inform(c("i" = "{ggblanket} treats faceting as an aesthetic."))

  ###ungroup
  data <- dplyr::ungroup(data)

  ###get default NULL values
  sf_geometry <- sf::st_geometry_type(data)

  if (rlang::is_null(alpha)) {
    if (any(sf_geometry %in% c("POLYGON", "MULTIPOLYGON"))) {
      alpha <- 0.9
    }
    else alpha <- 1
  }

  if (rlang::is_null(theme)) {
    theme <- gg_theme(grid_v = TRUE, grid_h = TRUE, void = TRUE)
  }

  ###process plot data
  ###factorise logical, reverse for horizontal, and chop intervals
  if (!rlang::quo_is_null(col)) {

    if (is.logical(rlang::eval_tidy(col, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
  }

  if (!rlang::quo_is_null(facet)) {
    if (is.logical(class(rlang::eval_tidy(facet, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
  }
  if (!rlang::quo_is_null(facet2)) {
    if (is.logical(class(rlang::eval_tidy(facet2, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet2, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
  }

  ###make col scale
  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(pal)) pal <-  pal_viridis_mix(1)
    else pal <- pal[1]

    col_scale <- list(
      ggplot2::scale_colour_manual(
        values = pal,
        na.value = pal_na,
      ),
      ggplot2::scale_fill_manual(
        values = pal,
        na.value = pal_na,
      )
    )


    col_legend_place <- "n"
  }
  else {
    if (rlang::is_null(col_title)) {
      if (rlang::is_null(titles)) col_title <- purrr::map_chr(rlang::as_name(col), snakecase::to_sentence_case)
      else col_title <- purrr::map_chr(rlang::as_name(col), titles)
    }
    col_title_position <- ifelse(col_title == "", "right", "top")

    if (rlang::is_null(col_legend_place)) {
      if (!rlang::quo_is_null(facet) &
          (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet, data)))) {
        col_legend_place <- "n"
      }
      else if (!rlang::quo_is_null(facet2) &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet2, data)))) {
        col_legend_place <- "n"
      }
      else
        col_legend_place <- "b"
    }

    if (is.numeric(rlang::eval_tidy(col, data))) {
      if (rlang::is_null(col_intervals)) { #continuous col
        col_min <- data %>% dplyr::pull(!!col) %>% min(na.rm = TRUE)
        col_max <- data %>% dplyr::pull(!!col) %>% max(na.rm = TRUE)

        if (!rlang::is_null(col_limits)) {
          if (is.na(col_limits)[1]) col_limits[1] <- col_min
          if (is.na(col_limits)[2]) col_limits[2] <- col_max
        }

        if (rlang::is_null(col_limits)) col_limits <- c(col_min, col_max)
        if (!rlang::is_null(col_include)) col_limits <- range(c(col_include, col_limits))

        if (rlang::is_null(col_breaks)) {
          if (col_legend_place %in% c("b", "t")) col_breaks_n <- 3
          else col_breaks_n <- 4
          col_breaks <- scales::breaks_pretty(n = col_breaks_n)(col_limits)
        }

        if (rlang::is_null(pal)) pal <- viridis::viridis(100)
        if (rlang::is_null(col_labels)) col_labels <- scales::label_comma()

        col_scale <- list(
          ggplot2::scale_colour_gradientn(
            colors = pal,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_colorbar(title.position = col_title_position)
          ),
          col_scale <- ggplot2::scale_fill_gradientn(
            colors = pal,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_colorbar(title.position = col_title_position)
          )

        )
      }
      else { #intervals col
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, col_intervals))

        col_levels <- levels(rlang::eval_tidy(col, data))
        col_n <- length(col_levels)

        if (rlang::is_null(pal)) pal <- pal_viridis_mix(col_n)
        else pal <- pal[1:col_n]

        if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
        else col_legend_rev <- TRUE

        if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
        if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

        col_scale <- list(
          ggplot2::scale_colour_manual(
            values = pal,
            breaks = col_levels,
            limits = col_levels,
            labels = col_labels,
            na.value = pal_na,
            guide = ggplot2::guide_legend(
              reverse = col_legend_rev,
              title.position = col_title_position,
              ncol = col_legend_ncol,
              nrow = col_legend_nrow,
              byrow = TRUE
            )
          ),
          ggplot2::scale_fill_manual(
            values = pal,
            breaks = col_levels,
            limits = col_levels,
            labels = col_labels,
            na.value = pal_na,
            # aesthetics = c("col", "fill"),
            guide = ggplot2::guide_legend(
              reverse = col_legend_rev,
              title.position = col_title_position,
              ncol = col_legend_ncol,
              nrow = col_legend_nrow,
              byrow = TRUE
            )
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

      if (is.character(rlang::eval_tidy(col, data)) | is.factor(rlang::eval_tidy(col, data))) {
        col_legend_rev <- FALSE
      }
      else if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
      else col_legend_rev <- TRUE

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

      col_scale <- list(
        ggplot2::scale_colour_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE)
        ),
        ggplot2::scale_fill_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE)
        )
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
      ggplot2::aes(text = !!text),
      stat = stat,
      position = position,
      alpha = alpha,
      #linewidth = linewidth,
      #size = size,
      ...
    )

  if (!rlang::quo_is_null(facet)) {
    if (rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(!!facet),
          scales = "fixed",
          labeller = ggplot2::as_labeller(facet_labels),
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    }
    else {
      plot <- plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet2),
          cols = ggplot2::vars(!!facet),
          labeller = ggplot2::as_labeller(facet_labels),
          scales = "fixed", space = "fixed"
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
      col = col_title,
      fill = col_title,
      caption = caption
    ) +
    theme

  ###adjust legend
  if (col_legend_place %in% c("b", "t")) {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal")

    if (is.numeric(rlang::eval_tidy(col, data))) {
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

  else if (col_legend_place == "n" | rlang::quo_is_null(col)) {
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

  #return beautiful plot
  return(plot)
}

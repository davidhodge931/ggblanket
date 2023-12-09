#' @title Blank ggplot
#'
#' @description Create a blank ggplot with a wrapper around ggplot2::geom_blank().
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable.
#' @param xmin Unquoted xmin aesthetic variable.
#' @param xmax Unquoted xmax aesthetic variable.
#' @param xend Unquoted xend aesthetic variable.
#' @param ymin Unquoted ymin aesthetic variable.
#' @param ymax Unquoted ymax aesthetic variable.
#' @param yend Unquoted yend aesthetic variable.
#' @param z Unquoted z aesthetic variable.
#' @param group Unquoted group aesthetic variable.
#' @param subgroup Unquoted subgroup aesthetic variable.
#' @param label Unquoted label aesthetic variable.
#' @param text Unquoted text aesthetic variable.
#' @param sample Unquoted sample aesthetic variable.
#' @param mapping Map additional aesthetics using the ggplot2::aes function (e.g. shape). Excludes colour, fill or alpha_pal.
#' @param col_pal Colours to use. A character vector of hex codes (or names).
#' @param col_pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha_pal Opacity. A number between 0 and 1.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param x_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_gridlines TRUE or FALSE for vertical x gridlines. NULL guesses based on the classes of the x and y.
#' @param x_expand_limits For a continuous x variable, any values that the limits should encompass (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis.
#' @param x_oob For a continuous x variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param x_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Use "" for no title.
#' @param x_transform For a numeric x variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param y_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_gridlines TRUE or FALSE of horizontal y gridlines. NULL guesses based on the classes of the x and y.
#' @param y_expand_limits For a continuous y variable, any values that the limits should encompass (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis.
#' @param y_oob For a continuous y variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param y_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Use "" for no title.
#' @param y_transform For a numeric y variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param col_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param col_expand_limits For a continuous col variable, any values that the limits should encompass (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. Either "bottom", "right", "top" or "left". Or just the first letter of each.
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_limits A vector to determine the limits of the colour scale.
#' @param col_oob For a continuous col variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param col_rescale For a continuous col variable, a scales::rescale function.
#' @param col_steps For a continuous col variable, whether to colour in steps. Defaults to FALSE (i.e. a gradient).
#' @param col_title Legend title string. Use "" for no title.
#' @param col_transform For a numeric col variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c("value" = "label", ...)).
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of "wrap".
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of "wrap".
#' @param facet_scales Whether facet scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed". Only applies where the facet layout is "grid" and facet scales are not "fixed".
#' @param facet_layout Whether the layout is to be "wrap" or "grid". If NULL and a single facet (or facet2) argument is provided, then defaults to "wrap". If NULL and both facet and facet2 arguments are provided, defaults to "grid".
#' @param facet_switch Whether the facet layout is "grid", whether to switch the facet labels to the opposite side of the plot. Either "x", "y" or "both".
#' @param linetype_title Legend title string. Use "" for no title.
#' @param shape_title Legend title string. Use "" for no title.
#' @param size_title Legend title string. Use "" for no title.
#' @param caption Caption title string.
#' @param titles A function to format unspecified titles. Defaults to snakecase::to_sentence_case.
#' @param geom The geometric object to use to display the data as a ggproto Geom subclass.
#' @param stat A ggplot2 character string stat.
#' @param position Position adjustment function (e.g. ggplot2::position_identity()).
#' @param coord A coordinate function from ggplot2 (e.g. ggplot2::coord_cartesian(clip = "off")).
#' @param theme A ggplot2 theme.
#' @param ... Other arguments passed to within a params list in the layer function.
#'
#' @return A ggplot object.
#' @export
#' @examples
#'
#'
gg_blanket <- function(
    data = NULL,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    xmin = NULL,
    xmax = NULL,
    xend = NULL,
    ymin = NULL,
    ymax = NULL,
    yend = NULL,
    z = NULL,
    group = NULL,
    subgroup = NULL,
    label = NULL,
    text = NULL,
    sample = NULL,
    mapping = NULL,
    coord = NULL,
    theme = NULL,
    geom = ggplot2::GeomBlank,
    stat = ggplot2::StatIdentity,
    position = ggplot2::position_identity(),
    col_pal = NULL,
    col_pal_na = "#bebebe",
    alpha_pal = 1,
    title = NULL,
    subtitle = NULL,
    x_breaks = NULL,
    x_expand = NULL,
    x_gridlines = NULL,
    x_expand_limits = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_oob = scales::oob_keep,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_transform = scales::transform_identity(),
    y_breaks = NULL,
    y_expand = NULL,
    y_gridlines = NULL,
    y_expand_limits = NULL,
    y_labels = NULL,
    y_limits = NULL,
    y_oob = scales::oob_keep,
    y_sec_axis = ggplot2::waiver(),
    y_title = NULL,
    y_transform = scales::transform_identity(),
    col_breaks = NULL,
    col_expand_limits = NULL,
    col_expand = ggplot2::waiver(),
    col_labels = NULL,
    col_legend_place = "right",
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_limits = NULL,
    col_oob = scales::oob_keep,
    col_rescale = scales::rescale(),
    col_steps = FALSE,
    col_title = NULL,
    col_transform = NULL,
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    facet_layout = NULL,
    facet_switch = NULL,
    linetype_title = NULL,
    shape_title = NULL,
    size_title = NULL,
    caption = NULL,
    titles = snakecase::to_sentence_case,
    ...
) {

  ##############################################################################
  #quote
  ##############################################################################

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  xmin <- rlang::enquo(xmin)
  xmax <- rlang::enquo(xmax)
  xend <- rlang::enquo(xend)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  yend <- rlang::enquo(yend)
  z <- rlang::enquo(z)
  group <- rlang::enquo(group)
  subgroup <- rlang::enquo(subgroup)
  label <- rlang::enquo(label)
  text <- rlang::enquo(text)
  sample <- rlang::enquo(sample)

  ##############################################################################
  #stop, warn and inform
  ##############################################################################

  if (!rlang::is_null(mapping)) {
    if (any(names(unlist(mapping)) %in% c("colour", "fill", "alpha_pal"))) {
      rlang::abort("mapping argument does not support colour, fill or alpha_pal aesthetics")
    }
    if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  ##############################################################################
  #get default theme, if global theme not set
  ##############################################################################

  if (rlang::is_null(theme)) {
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
      theme <- light_mode()
    }
  }

  ##############################################################################
  #determine classes
  ##############################################################################

  x_null <- inherits(
      rlang::eval_tidy(x, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("NULL")
    )

  x_numeric <- inherits(
    rlang::eval_tidy(x, data),
    what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("numeric", "double", "integer")
    )

  x_date <- inherits(
    rlang::eval_tidy(x, data),
    what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("Date")
    )

  x_posixct <- inherits(
    rlang::eval_tidy(x, data),
    what = c("POSIXct")
  ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("POSIXct")
    )

  x_hms <- inherits(
    rlang::eval_tidy(x, data),
    what = c("hms")
  ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("hms")
    )

  x_continuous <- x_null | x_numeric | x_date | x_posixct | x_hms

  y_null <- inherits(
      rlang::eval_tidy(y, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("NULL")
    )

  y_numeric <- inherits(
    rlang::eval_tidy(y, data),
    what = c("numeric", "double", "integer")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("numeric", "double", "integer")
    )

  y_date <- inherits(
    rlang::eval_tidy(y, data),
    what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("Date")
    )

  y_posixct <- inherits(
    rlang::eval_tidy(y, data),
    what = c("datetime")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("datetime")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("datetime")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("datetime")
    )

  y_hms <- inherits(
    rlang::eval_tidy(y, data),
    what = c("hms")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("hms")
    )

  y_continuous <- y_null | y_numeric | y_date | y_posixct | y_hms

  ##############################################################################
  #determine if flipped
  ##############################################################################

  flipped <- ifelse(x_continuous & !y_continuous, TRUE, FALSE)

  if (x_null & !y_null) flipped <- TRUE
  else if ((x_numeric | x_date | x_posixct | x_hms) &
           !(y_null | y_numeric | y_date | y_posixct | y_hms)) {
    flipped <- TRUE
  }
  else flipped <- FALSE

  ##############################################################################
  #process the data
  ##############################################################################

  data <- data %>%
    #ungroup the data
    dplyr::ungroup() %>%
    #make infinite values NA
    dplyr::mutate(dplyr::across(
      c(!!x, !!xmin, !!xmax, !!xend,
        !!y, !!ymin, !!ymax, !!yend,
        !!col, !!facet, !!facet2,
        !!group, !!subgroup, !!label, !!sample),
      na_if_inf)) |>
    #convert logicals to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!facet, !!facet2) &
                                  tidyselect::where(is.logical), function(x)
                                    factor(x, levels = c(TRUE, FALSE)))) |>
    #convert characters to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!facet, !!facet2) &
                                  tidyselect::where(is.character), function(x)
                                    factor(x))) |>
    #reverse y*, so that reads top low-levels to bottom high-levels
    dplyr::mutate(dplyr::across(c(!!y, !!ymin, !!ymax, !!yend) &
                                  tidyselect::where(is.factor),
                                function(x) forcats::fct_rev(x)))

  #if flipped, order col correctly
  if (!identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data))) {
    if (flipped) {
      data <- data |>
        dplyr::mutate(dplyr::across(!!col & tidyselect::where(is.factor),
                                    function(x) forcats::fct_rev(x)))
    }
  }

  ##############################################################################
  #yet more defaults
  ##############################################################################

  if (!rlang::quo_is_null(facet)) {
    facet_n <- data %>%
      dplyr::pull(!!facet) %>%
      levels() %>%
      length()

    if (any(is.na(data %>% dplyr::pull(!!facet)))) facet_n <- facet_n + 1
  }

  if (!rlang::quo_is_null(facet2)) {
    facet2_n <- data %>%
      dplyr::pull(!!facet2) %>%
      levels() %>%
      length()

    if (any(is.na(data %>% dplyr::pull(!!facet2)))) facet2_n <- facet2_n + 1
  }

  x_drop <- ifelse(facet_scales %in% c("free_x", "free"), TRUE, FALSE)
  y_drop <- ifelse(facet_scales %in% c("free_y", "free"), TRUE, FALSE)

  if (stringr::str_detect(class(stat)[1], "Sf")) {
    # sf <- TRUE
    geometry <- sf::st_geometry(data)
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")
  }
  else {
    # sf <- FALSE
    geometry <- NULL
    if (rlang::is_null(coord)) ggplot2::coord_cartesian(clip = "off")
  }

  ##############################################################################
  # add ggplot() with aesthetics
  ##############################################################################

  if (rlang::quo_is_null(col)) {
    if (!x_null & !y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
    else if (!x_null & y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
    else if (x_null & !y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
    else if (x_null & y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
  }
  else {
    if (!x_null & !y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          col = !!col,
          fill = !!col,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
    else if (!x_null & y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          col = !!col,
          fill = !!col,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
    else if (x_null & !y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          col = !!col,
          fill = !!col,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
    else if (x_null & y_null) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          col = !!col,
          fill = !!col,
          xmin = !!xmin,
          xmax = !!xmax,
          xend = !!xend,
          ymin = !!ymin,
          ymax = !!ymax,
          yend = !!yend,
          z = !!z,
          group = !!group,
          subgroup = !!subgroup,
          sample = !!sample,
          label = !!label,
          geometry = geometry,
          !!!mapping
        ))
    }
  }

  ##############################################################################
  # add faceting
  ##############################################################################

  if (rlang::is_null(facet_layout)) {
    if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) facet_layout <- "wrap"
    else if (!rlang::quo_is_null(facet2) & rlang::quo_is_null(facet)) facet_layout <- "wrap"
    else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else if (rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else facet_layout <- "null"
  }

  if (facet_layout == "wrap") {
    if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet),
          scales = facet_scales,
          drop = FALSE,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet2),
          scales = facet_scales,
          drop = FALSE,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet, !!facet2),
          scales = facet_scales,
          drop = FALSE,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }
  else if (facet_layout == "grid") {
    if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            rows = ggplot2::vars(!!facet2),
                            cols = ggplot2::vars(!!facet),
                            scales = facet_scales,
                            space = facet_space,
                            drop = FALSE,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            cols = ggplot2::vars(!!facet),
                            scales = facet_scales,
                            space = facet_space,
                            drop = FALSE,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            rows = ggplot2::vars(!!facet2),
                            scales = facet_scales,
                            space = facet_space,
                            drop = FALSE,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  ##############################################################################
  # Add positional scales pre getting plot data
  ##############################################################################

  if (!stringr::str_detect(class(stat)[1], "Sf")) {

    #limits reversed to deal with when trans is reverse
    if (x_numeric) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_date) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_posixct) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_hms) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits[c(2, 1)], oob = x_oob, trans ="hms")
      }
      else {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits, oob = x_oob, trans ="hms")
      }
    }
    else if (!x_continuous) {
      if (!rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_discrete(limits = x_limits, drop = x_drop)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_discrete(drop = x_drop)
      }
    }
    if (!rlang::is_null(x_expand_limits)) {
      plot <- plot +
        ggplot2::expand_limits(x = x_expand_limits)
    }

    if (y_numeric) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_date) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_posixct) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_hms) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits[c(2, 1)], oob = y_oob, trans ="hms")
      }
      else {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits, oob = y_oob, trans ="hms")
      }
    }
    else if (!y_continuous) {
      if (!rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_discrete(limits = y_limits, drop = y_drop)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_discrete(drop = y_drop)
      }
    }
    if (!rlang::is_null(y_expand_limits)) {
      plot <- plot +
        ggplot2::eypand_limits(y = y_expand_limits)
    }
  }

  ##############################################################################
  # Add layer
  ##############################################################################

  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(col_pal)) col_pal1 <- "#357BA2"
    else col_pal1 <- col_pal[1]

    plot2 <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        params = list(colour = col_pal1, fill = col_pal1, alpha = alpha_pal, ...)
      ) +
      coord +
      theme
  }
  else {
    plot2 <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        params = list(alpha = alpha_pal, ...)
      ) +
      coord +
      theme
  }

  ##############################################################################
  # Get plot build and data
  ##############################################################################

  #Get plot data and flipped status
  plot_build <- ggplot2::ggplot_build(plot2)
  plot_data <- plot_build$data[[1]]

  ##############################################################################
  # Add col scale
  ##############################################################################

  #sf documents scales differently, and this fixes
  if (stringr::str_detect(class(stat)[1], "Sf")) {
    if (class(rlang::eval_tidy(col, data)) %in%
             c("NULL", "numeric", "double", "integer","date", "datetime","hms")) {
      col_continuous <- TRUE
    }
    if (class(rlang::eval_tidy(col, data)) %in%
             c("character", "logical", "factor")) {
      col_continuous <- FALSE
    }
    else col_continuous <- NA

    plot <- plot2
  }
  #correct for plots where col is null, but there should be a colour scale
  else {
    col_scale <- purrr::map_chr(plot_build$plot$scales$scales, \(x) rlang::call_name((x[["call"]])))

    if (any(col_scale %in%
            c("scale_colour_continuous", "scale_colour_gradientn", "scale_colour_stepsn",
              "scale_fill_continuous", "scale_fill_gradientn", "scale_fill_stepsn"))) {
      col_continuous <- TRUE
    }
    else if (any(col_scale %in%
                   c("scale_colour_discrete", "scale_colour_manual", "scale_colour_ordinal",
                     "scale_fill_discrete", "scale_fill_manual", "scale_fill_ordinal"))) {
      col_continuous <- FALSE
    }
    else col_continuous <- NA

    if (rlang::quo_is_null(col) & !is.na(col_continuous)) {
      plot <- plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = list(alpha = alpha_pal, ...)
        ) +
        coord +
        theme
    }
    else {
      plot <- plot2
    }
  }

  #make col scale
  if (!is.na(col_continuous)) {
    if (col_continuous) {
      if (rlang::is_null(col_pal)) {
        col_pal <- viridisLite::mako(18, direction = -1)
      }

      if (rlang::is_null(col_transform)) {
        if (inherits(rlang::eval_tidy(col, data), what = "hms")) col_transform <- scales::transform_hms()
        else if (inherits(rlang::eval_tidy(col, data), what = "date")) col_transform <- scales::transform_date()
        else col_transform <- scales::transform_identity()
      }

      if (rlang::is_null(col_breaks)) {
        if (!any(col_transform$name %in% c("identity", "reverse"))) col_breaks <- ggplot2::waiver()
        else col_breaks <- scales::breaks_pretty(n = 5)
      }

      if (rlang::is_null(col_labels)) {
        if (col_transform$name == "date") col_labels <- scales::label_date(format = c("%Y", "%b", "%e"))
        else if (col_transform$name == "hms") col_labels <- scales::label_hms(format = "%H:%M")
        else if (!any(col_transform$name %in% c("identity", "reverse"))) col_labels <- ggplot2::waiver()
        else col_labels <- scales::label_comma(drop0trailing = TRUE)
      }

      if (!col_steps) {
        plot <- plot +
          ggplot2::scale_fill_gradientn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            trans = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::scale_colour_gradientn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            trans = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(
              title.position = "top", #keep for now
              ticks.colour = "#fcfdfe", #keep for now
              reverse = col_legend_rev,
              order = 1
            ),
            fill = ggplot2::guide_colourbar(
              title.position = "top", #keep for now
              ticks.colour = "#fcfdfe", #keep for now
              reverse = col_legend_rev,
              order = 1
            )
          )

      }
      else if (col_steps) {
        plot <- plot +
          ggplot2::scale_fill_stepsn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            trans = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::scale_colour_stepsn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            trans = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev,
              order = 1
            ),
            fill = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev,
              order = 1
            )
          )
      }
    }
    else if (!col_continuous) {
      if (!rlang::quo_is_null(col)) {
        col_n <- data %>%
          dplyr::pull(!!col) %>%
          levels() %>%
          length()
        if (rlang::is_null(col_pal)) col_pal <- guardian()
        col_pal <- col_pal[1:col_n]
      } else {
        if (!rlang::is_null(plot_build$plot$labels$fill)) {
          col_n <- length(levels(dplyr::pull(plot_data, rlang::as_name(plot_build$plot$labels$fill[1]))))
        }
        else if (!rlang::is_null(plot_build$plot$labels$colour)) {
          col_n <- length(levels(dplyr::pull(plot_data, rlang::as_name(plot_build$plot$labels$colour[1]))))
        }

        if (rlang::is_null(col_pal)) col_pal <- viridisLite::mako(col_n, direction = -1)
      }

      if (flipped) {
        col_legend_rev <- !col_legend_rev
        col_pal <- rev(col_pal)
      }

      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()

      plot <- plot +
        ggplot2::scale_fill_manual(
          values = col_pal,
          limits = col_limits,
          expand = col_expand,
          breaks = col_breaks,
          labels = col_labels,
          na.value = col_pal_na,
          drop = FALSE, #consider add argument
        ) +
        ggplot2::scale_colour_manual(
          values = col_pal,
          limits = col_limits,
          expand = col_expand,
          breaks = col_breaks,
          labels = col_labels,
          na.value = col_pal_na,
          drop = FALSE, #consider add argument
        ) +
        ggplot2::guides(
          colour = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = "top", #keep for now
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE, #keep for now
            key.spacing = grid::unit(11 * 0.33, "pt"), #keep for now
            order = 1
          ),
          fill = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = "top", #keep for now
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE, #keep for now
            key.spacing = grid::unit(11 * 0.33, "pt"), #keep for now
            order = 1
          )
        )
    }
  }

  #expand limits if necessary
  plot <- plot +
    ggplot2::expand_limits(
      colour = col_expand_limits,
      fill = col_expand_limits
    )

  ##############################################################################
  # Positional scales
  ##############################################################################

  #Make x, y scales
  if (!stringr::str_detect(class(stat)[1], "Sf")) {
    # if (stat != "sf") {
    #Make x scale based on plot_data
    if (!x_continuous) {
      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
      if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

      suppressMessages({
        plot <- plot +
          ggplot2::scale_x_discrete(
            expand = x_expand,
            labels = x_labels,
            breaks = x_breaks,
            limits = x_limits,
            drop = x_drop
          )
      })
    }
    else if (x_numeric | x_date | x_posixct | x_hms | x_null) {

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
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(x_vars_str)), \(x) !is.na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(x_vars_str)))

        if (ncol(x_vctr_temp) != 0) {
          if (!flipped & stringr::str_detect(class(stat)[1], "Bin")) {
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
          if (rlang::is_null(x_expand_limits)) {
            if (any(x_transform$name %in% "reverse")) {
              x_vctr <- c(x_vctr, x_expand_limits * -1)
            }
            else {
              x_vctr <- c(x_vctr, x_expand_limits)
            }
          }
        } else {
          x_vctr <- NULL
        }

        if (x_date) {
          x_vctr <- lubridate::as_date(x_vctr, origin = "1970-01-01")
        }
        else if (x_posixct) {
          x_vctr <- lubridate::as_posixct(x_vctr, origin = "1970-01-01")
        }
        else if (x_hms) {
          x_vctr <- hms::as_hms(x_vctr)
        }

        x_range <- range(x_vctr, na.rm = TRUE)
        if (x_hms) x_range <- hms::as_hms(x_range)
        if (!rlang::is_null(x_expand_limits)) x_range <- range(c(x_range, x_expand_limits))
        if (any(x_transform$name %in% "reverse")) x_range <- sort(x_range, decreasing = TRUE)

        if (rlang::is_null(x_breaks)) {
          if (rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
            x_breaks_n <- 7
          }
          else if (facet_layout == "wrap") {
            if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
              if (facet_n * facet2_n <= 1) x_breaks_n <- 7
              # else if (facet_n * facet2_n == 2) x_breaks_n <- 5
              else if (facet_n * facet2_n <= 6) x_breaks_n <- 4
              else x_breaks_n <- 4
            }
            else if (!rlang::quo_is_null(facet)) {
              if (facet_n <= 1) x_breaks_n <- 7
              # else if (facet_n == 2) x_breaks_n <- 5
              else if (facet_n <= 6) x_breaks_n <- 4
              else x_breaks_n <- 3
            }
            else if (!rlang::quo_is_null(facet2)) {
              if (facet2_n <= 1) x_breaks_n <- 7
              # else if (facet2_n == 2) x_breaks_n <- 5
              else if (facet2_n <= 6) x_breaks_n <- 4
              else x_breaks_n <- 3
            }
          }
          else if (facet_layout == "grid") {
            if (rlang::quo_is_null(facet)) {
              x_breaks_n <- 7
            }
            else if (!rlang::quo_is_null(facet)) {
              if (facet_n <= 1) x_breaks_n <- 7
              else if (facet_n == 2) x_breaks_n <- 5
              else if (facet_n <= 3) x_breaks_n <- 4
              else x_breaks_n <- 3
            }
          }

          if (flipped) x_breaks_n <- x_breaks_n - 1
        }

        if (rlang::is_null(x_limits)) {
          if (rlang::is_null(x_breaks)) {
            if (!flipped | !rlang::is_null(x_expand)) {
              if (x_hms) x_breaks <- scales::hms_transform()
              else if (any(x_transform == "log10")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)
              else if (any(x_transform == "log2")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 2)
              else if (any(x_transform == "log")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = exp(1))
              else x_breaks <- scales::breaks_pretty(n = x_breaks_n)
            }
            else if (flipped) {
              if (x_hms) x_breaks <- scales::hms_transform()$breaks(x_range)
              else if (any(x_transform == "log10")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)(x_range)
              else if (any(x_transform == "log2")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 2)(x_range)
              else if (any(x_transform == "log")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = exp(1))(x_range)
              else x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)
            }

            if (!flipped) x_limits <- x_range
            else if (any(!x_transform$name %in% c("identity", "reverse"))) x_limits <- x_range
            else if (!rlang::is_null(x_expand)) x_limits <- x_breaks(x_range)[c(1, length(x_breaks(x_range)))]
            else x_limits <- x_breaks[c(1, length(x_breaks))]
          }
          else if (!(rlang::is_null(x_breaks))) {
            if (!flipped) x_limits <- x_range
            else {
              if (any(!x_transform$name %in% c("identity", "reverse"))) {
                x_limits <- x_range
              } else {
                if (is.function(x_breaks)) x_breaks <- x_breaks(x_range)
                if (is.vector(x_breaks)) {
                  x_limits <- x_breaks[c(1, length(x_breaks))]
                  if (!rlang::is_null(x_expand_limits)) x_limits <- range(x_limits, x_expand_limits)
                }
              }
            }
          }
          if (any(x_transform$name %in% "reverse")) x_limits <- sort(x_limits, decreasing = TRUE)
        }
        else if (!rlang::is_null(x_limits)) {
          if (is.na(x_limits[1])) {
            x_limits[1] <- x_range[1]
          }
          if (is.na(x_limits[2])) {
            x_limits[2] <- x_range[2]
          }

          if (!rlang::is_null(x_expand_limits)) x_limits <- range(c(x_limits, x_expand_limits))

          if (any(x_transform$name %in% "reverse")) x_limits <- sort(x_limits, decreasing = TRUE)

          if (rlang::is_null(x_breaks)) {
            if (!flipped | !rlang::is_null(x_expand)) {
              if (x_hms) x_breaks <- scales::hms_transform()
              else if (any(x_transform == "log10")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)
              else if (any(x_transform == "log2")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 2)
              else if (any(x_transform == "log")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = exp(1))
              else x_breaks <- scales::breaks_pretty(n = x_breaks_n)
            }
            else if (flipped) {
              if (x_hms) x_breaks <- scales::hms_transform()$breaks(x_limits)
              else if (any(x_transform == "log10")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)(x_limits)
              else if (any(x_transform == "log2")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 2)(x_limits)
              else if (any(x_transform == "log")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = exp(1))(x_limits)
              else x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_limits)
            }
          }
        }
      }
      else if (facet_scales %in% c("free", "free_x")) {
        if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
      }

      if (rlang::is_null(x_expand)) {
        if (any(!x_transform$name %in% c("identity", "reverse"))) {
          x_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else if (flipped) x_expand <- c(0, 0)
        else if (facet_scales %in% c("fixed", "free_y") &
                 (y_date | y_posixct | y_hms | y_numeric | y_null)) {
          x_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else x_expand <- c(0, 0)
      }

      if (rlang::is_null(x_labels)) {
        if (x_numeric | x_null) {
          if (any(x_transform == "log10")) x_labels <- scales::label_log(base = 10)
          else if (any(x_transform == "log2")) x_labels <- scales::label_log(base = 2)
          else if (any(x_transform == "log")) x_labels <- scales::label_math("e"^.x, format = log)
          else x_labels <- scales::label_comma(drop0trailing = TRUE)
        }
        else if (x_date | x_posixct) {
          x_labels <- scales::label_date_short(format = c("%Y", "%b", "%e", "%H:%M"))
        }
        else if (x_hms) {
          x_labels <- scales::label_hms(format = "%H:%M")
        }
      }

      if (x_numeric | x_null) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_x_continuous(
              breaks = x_breaks,
              limits = x_limits,
              expand = x_expand,
              labels = x_labels,
              oob = x_oob,
              sec.axis = x_sec_axis,
              trans = x_transform
            )
        })
      }
      else if (x_date) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_x_date(
              breaks = x_breaks,
              limits = x_limits,
              expand = x_expand,
              labels = x_labels,
              oob = x_oob,
              sec.axis = x_sec_axis
            )
        })
      }
      else if (x_posixct) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_x_datetime(
              breaks = x_breaks,
              limits = x_limits,
              expand = x_expand,
              labels = x_labels,
              oob = x_oob,
              sec.axis = x_sec_axis
            )
        })
      }
      else if (x_hms) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_x_continuous(
              breaks = x_breaks,
              limits = x_limits,
              expand = x_expand,
              labels = x_labels,
              oob = x_oob,
              sec.axis = x_sec_axis,
              trans ="hms"
            )
        })
      }
    }

    #Make y scale based on plot_data
    if (!y_continuous) {
      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
      if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
      if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

      suppressMessages({
        plot <- plot +
          ggplot2::scale_y_discrete(
            expand = y_expand,
            labels = y_labels,
            breaks = y_breaks,
            drop = y_drop
          )
      })
    }
    else if (y_numeric | y_date | y_posixct | y_hms | y_null) {

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
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(y_vars_str)), \(x) !is.na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(y_vars_str)))

        if (ncol(y_vctr_temp) != 0) {
          if (flipped & stringr::str_detect(class(stat)[1], "Bin")) {
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
          if (rlang::is_null(y_expand_limits)) {
            if (any(y_transform$name %in% "reverse")) {
              y_vctr <- c(y_vctr, y_expand_limits * -1)
            }
            else {
              y_vctr <- c(y_vctr, y_expand_limits)
            }
          }
        } else {
          y_vctr <- NULL
        }

        if (y_date) {
          y_vctr <- lubridate::as_date(y_vctr, origin = "1970-01-01")
        }
        else if (y_posixct) {
          y_vctr <- lubridate::as_posixct(y_vctr, origin = "1970-01-01")
        }
        else if (y_hms) {
          y_vctr <- hms::as_hms(y_vctr)
        }

        y_range <- range(y_vctr, na.rm = TRUE)
        if (y_hms) y_range <- hms::as_hms(y_range)
        if (!rlang::is_null(y_expand_limits)) y_range <- range(c(y_range, y_expand_limits))
        if (any(y_transform$name %in% "reverse")) y_range <- sort(y_range, decreasing = TRUE)

        if (rlang::is_null(y_breaks)) {
          if (rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
            y_breaks_n <- 7
          }
          else if (facet_layout == "wrap") {
            if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
              if (facet_n * facet2_n <= 3) y_breaks_n <- 6
              else if (facet_n * facet2_n <= 6) y_breaks_n <- 5
              else y_breaks_n <- 4
            }
            else if (!rlang::quo_is_null(facet)) {
              if (facet_n <= 3) y_breaks_n <- 6
              else if (facet_n == 4) y_breaks_n <- 5
              else y_breaks_n <- 4
            }
            else if (!rlang::quo_is_null(facet2)) {
              if (facet2_n <= 3) y_breaks_n <- 6
              else if (facet2_n == 4) y_breaks_n <- 5
              else y_breaks_n <- 4
            }
          }
          else if (facet_layout == "grid") {
            if (rlang::quo_is_null(facet2)) {
              y_breaks_n <- 6
            }
            else if (!rlang::quo_is_null(facet2)) {
              if (facet2_n <= 1) y_breaks_n <- 6
              else if (facet2_n == 2) y_breaks_n <- 5
              else y_breaks_n <- 4
            }
          }
        }

        if (rlang::is_null(y_limits)) {
          if (rlang::is_null(y_breaks)) {
            if (flipped | !rlang::is_null(y_expand)) {
              if (y_hms) y_breaks <- scales::hms_transform()
              else if (any(y_transform == "log10")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)
              else if (any(y_transform == "log2")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 2)
              else if (any(y_transform == "log")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = exp(1))
              else y_breaks <- scales::breaks_pretty(n = y_breaks_n)
            }
            else if (!flipped) {
              if (y_hms) y_breaks <- scales::hms_transform()$breaks(y_range)
              else if (any(y_transform == "log10")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)(y_range)
              else if (any(y_transform == "log2")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 2)(y_range)
              else if (any(y_transform == "log")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = exp(1))(y_range)
              else y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)
            }

            if (flipped) y_limits <- y_range
            else if (any(!y_transform$name %in% c("identity", "reverse"))) y_limits <- y_range
            else if (!rlang::is_null(y_expand)) y_limits <- y_breaks(y_range)[c(1, length(y_breaks(y_range)))]
            else y_limits <- y_breaks[c(1, length(y_breaks))]
          }
          else if (!(rlang::is_null(y_breaks))) {
            if (flipped) y_limits <- y_range
            else {
              if (any(!y_transform$name %in% c("identity", "reverse"))) {
                y_limits <- y_range
              } else {
                if (is.function(y_breaks)) y_breaks <- y_breaks(y_range)
                if (is.vector(y_breaks)) {
                  y_limits <- y_breaks[c(1, length(y_breaks))]
                  if (!rlang::is_null(y_expand_limits)) y_limits <- range(y_limits, y_expand_limits)
                }
              }
            }
          }
          if (any(y_transform$name %in% "reverse")) y_limits <- sort(y_limits, decreasing = TRUE)
        }
        else if (!rlang::is_null(y_limits)) {
          if (is.na(y_limits[1])) y_limits[1] <- y_range[1]
          if (is.na(y_limits[2])) y_limits[2] <- y_range[2]

          if (!rlang::is_null(y_expand_limits)) y_limits <- range(c(y_limits, y_expand_limits))

          if (any(y_transform$name %in% "reverse")) y_limits <- sort(y_limits, decreasing = TRUE)

          if (rlang::is_null(y_breaks)) {
            if (flipped | !rlang::is_null(y_expand)) {
              if (y_hms) y_breaks <- scales::hms_transform()
              else if (any(y_transform == "log10")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)
              else if (any(y_transform == "log2")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 2)
              else if (any(y_transform == "log")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = exp(1))
              else y_breaks <- scales::breaks_pretty(n = y_breaks_n)
            }
            else if (!flipped) {
              if (y_hms) y_breaks <- scales::hms_transform()$breaks(y_limits)
              else if (any(y_transform == "log10")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)(y_limits)
              else if (any(y_transform == "log2")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 2)(y_limits)
              else if (any(y_transform == "log")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = exp(1))(y_limits)
              else y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_limits)
            }
          }
        }
      }
      else if (facet_scales %in% c("free", "free_y")) {
        if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
      }

      if (rlang::is_null(y_expand)) {
        if (any(!y_transform$name %in% c("identity", "reverse"))) {
          y_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else if (!flipped) y_expand <- c(0, 0)
        else if (facet_scales %in% c("fixed", "free_x") &
                 (y_date | y_posixct | y_hms | y_numeric | y_null)) {
          y_expand <- ggplot2::expansion(mult = c(0.05, 0.05))
        }
        else y_expand <- c(0, 0)
      }

      if (rlang::is_null(y_labels)) {
        if (y_numeric | y_null) {
          if (any(y_transform == "log10")) y_labels <- scales::label_log(base = 10)
          else if (any(y_transform == "log2")) y_labels <- scales::label_log(base = 2)
          else if (any(y_transform == "log")) y_labels <- scales::label_math("e"^.x, format = log)
          else y_labels <- scales::label_comma(drop0trailing = TRUE)
        }
        else if (y_date | y_posixct) {
          y_labels <- scales::label_date_short(format = c("%Y", "%b", "%e", "%H:%M"))
        }
        else if (y_hms) {
          y_labels <- scales::label_hms(format = "%H:%M")
        }
      }

      if (y_numeric | y_null) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_y_continuous(
              breaks = y_breaks,
              limits = y_limits,
              expand = y_expand,
              labels = y_labels,
              oob = y_oob,
              sec.axis = y_sec_axis,
              trans = y_transform
            )
        })
      }
      else if (y_date) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_y_date(
              breaks = y_breaks,
              limits = y_limits,
              expand = y_expand,
              labels = y_labels,
              oob = y_oob,
              sec.axis = y_sec_axis
            )
        })
      }
      else if (y_posixct) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_y_posixct(
              breaks = y_breaks,
              limits = y_limits,
              expand = y_expand,
              labels = y_labels,
              oob = y_oob,
              sec.axis = y_sec_axis
            )
        })
      }
      else if (y_hms) {
        suppressMessages({
          plot <- plot +
            ggplot2::scale_y_continuous(
              breaks = y_breaks,
              limits = y_limits,
              expand = y_expand,
              labels = y_labels,
              oob = y_oob,
              sec.axis = y_sec_axis,
              trans ="hms"
            )
        })
      }
    }
  }

  else {
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()

    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = x_limits,
        breaks = x_breaks,
        expand = x_expand,
        labels = x_labels
      ) +
      ggplot2::scale_y_continuous(
        limits = y_limits,
        breaks = y_breaks,
        expand = y_expand,
        labels = y_labels
      )
  }

  #############################################################################
  titles
  #############################################################################
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

  if (rlang::is_null(linetype_title)) {
    if (!rlang::is_null(plot_build$plot$labels$linetype)) {
      linetype_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), titles)
    }
  }
  if (rlang::is_null(shape_title)) {
    if (!rlang::is_null(plot_build$plot$labels$shape)) {
      shape_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), titles)
    }
  }
  if (rlang::is_null(size_title)) {
    if (!rlang::is_null(plot_build$plot$labels$size)) {
      size_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), titles)
    }
  }

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_title,
      y = y_title,
      col = col_title,
      fill = col_title,
      linetype = linetype_title,
      shape = shape_title,
      size = size_title)

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




  #expand limits if necessary
  if (!stringr::str_detect(class(stat)[1], "Sf")) {
    if (!rlang::is_null(x_expand_limits)) {
      plot <- plot +
        ggplot2::expand_limits(x = x_expand_limits)
    }
    if (!rlang::is_null(y_expand_limits)) {
      plot <- plot +
        ggplot2::expand_limits(y = y_expand_limits)
    }
  }

  if (!rlang::is_null(col_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(colour = col_expand_limits, fill = col_expand_limits)
  }

  ##############################################################################
  # gridlines
  ##############################################################################

  if (rlang::is_null(x_gridlines)) {
    if (stringr::str_detect(class(stat)[1], "Sf")) x_gridlines <- FALSE
    else if (flipped) x_gridlines <- TRUE
    else x_gridlines <- FALSE
  }

  if (rlang::is_null(y_gridlines)) {
    if (stringr::str_detect(class(stat)[1], "Sf")) y_gridlines <- FALSE
    else if (flipped) y_gridlines <- FALSE
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

  ##############################################################################
  # plot
  ##############################################################################
  return(plot)
}



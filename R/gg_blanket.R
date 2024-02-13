#' @title Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()` with [geom_blank()][ggplot2::geom_blank()] defaults. This function underlies all other `gg_*` functions. It contains a `geom` argument for maximum flexibility.
#'
#' @param data A data frame or tibble.
#' @param ... Other arguments passed to within a `params` list in `layer()`.
#' @param geom A geometric object to display the data. A snakecase character string of a ggproto Geom subclass object minus the Geom prefix (e.g. `"point"`).
#' @param stat A statistical transformation to use on the data. A snakecase character string of a ggproto Stat subclass object minus the Stat prefix (e.g. `"identity"`).
#' @param position A position adjustment. A snakecase character string of a ggproto Position subclass object minus the Position prefix (e.g. `"identity"`), or a `position_*()` function that outputs a ggproto Position subclass object (e.g. `ggplot2::position_identity()`).
#' @param coord A coordinate system. A `coord_*()` function that outputs a constructed ggproto Coord subclass object (e.g. [ggplot2::coord_cartesian()]).
#' @param mode A `*_mode_*` theme (e.g. [grey_mode_b()], [grey_mode_rt()], or [dark_mode_rt()]). This argument adds the theme, but removes selected gridline/axis-line/ticks. To avoid these side-effects, `+` your theme on as a layer.
#' @param x Unquoted `x` aesthetic variable.
#' @param xmin Unquoted `xmin` aesthetic variable.
#' @param xmax Unquoted `xmax` aesthetic variable.
#' @param xend Unquoted `xend` aesthetic variable.
#' @param y Unquoted `y` aesthetic variable.
#' @param ymin Unquoted `ymin` aesthetic variable.
#' @param ymax Unquoted `ymax` aesthetic variable.
#' @param yend Unquoted `yend` aesthetic variable.
#' @param z Unquoted `z` aesthetic variable.
#' @param col Unquoted `col` aesthetic variable.
#' @param alpha Unquoted `alpha` aesthetic variable.
#' @param facet Unquoted `facet` aesthetic variable.
#' @param facet2 Unquoted `facet2` aesthetic variable.
#' @param group Unquoted `group` aesthetic variable.
#' @param subgroup Unquoted `subgroup` aesthetic variable.
#' @param label Unquoted `label` aesthetic variable.
#' @param text Unquoted `text` aesthetic variable.
#' @param sample Unquoted `sample` aesthetic variable.
#' @param mapping Set of additional aesthetic mappings within [ggplot2::aes()] for non-supported aesthetics (e.g. `shape`, `linetype`, `linewidth`, or `size`) or for delayed evaluation.
#' @param x_breaks,y_breaks A `scales::breaks_*` function (e.g. [scales::breaks_pretty()]), or a vector of breaks.
#' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param x_expand_limits,y_expand_limits For a continuous variable, any values that the limits should encompass (e.g. `0`).
#' @param x_labels,y_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or [scales::label_comma()]), or a vector of labels.
#' @param x_limits,y_limits A vector of length 2 to determine the limits of the axis.
#' @param x_oob,y_oob For a continuous scale variable, a `scales::oob_*` function of how to handle values outside of limits. Defaults to `scales::oob_keep`.
#' @param x_sec_axis,y_sec_axis A secondary axis using [ggplot2::sec_axis()] or [ggplot2::dup_axis()].
#' @param x_title,y_title Axis title string. Use `""` for no title.
#' @param x_transform,y_transform For a numeric scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param col_breaks A `scales::breaks_*` function (e.g. [scales::breaks_pretty()]), or a vector of breaks.
#' @param col_continuous_type For a continuous variable, whether to colour as a `"gradient"` or in `"steps"`. Defaults to `"gradient"`.
#' @param col_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param col_expand_limits For a continuous variable, any values that the limits should encompass (e.g. `0`).
#' @param col_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or [scales::label_comma()]), or a vector of labels.
#' @param col_legend_ncol,col_legend_nrow The number of columns and rows for the legend guide.
#' @param col_legend_rev Reverse the elements of the legend guide. Defaults to `FALSE`.
#' @param col_limits A vector of length 2 to determine the limits of the axis.
#' @param col_oob For a continuous scale variable, a `scales::oob_*` function of how to handle values outside of limits. Defaults to `scales::oob_keep`.
#' @param col_pal Colours to use. A character vector of hex codes (or names).
#' @param col_pal_na Colour to use for `NA` values. A character vector of a hex code (or name).
#' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' @param col_title Axis title string. Use `""` for no title.
#' @param col_transform For a numeric scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param alpha_breaks A `scales::breaks_*` function (e.g. [scales::breaks_pretty()]), or a vector of breaks.
#' @param alpha_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param alpha_expand_limits For a continuous variable, any values that the limits should encompass (e.g. `0`).
#' @param alpha_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or [scales::label_comma()]), or a vector of labels.
#' @param alpha_legend_ncol,alpha_legend_nrow The number of columns and rows for the legend guide.
#' @param alpha_legend_rev Reverse the elements of the legend guide. Defaults to `FALSE`.
#' @param alpha_limits A vector of length 2 to determine the limits of the axis.
#' @param alpha_oob For a continuous scale variable, a `scales::oob_*` function of how to handle values outside of limits. Defaults to `scales::oob_keep`.
#' @param alpha_pal Alpha values to use. For a continuous variable, a vector of length 2 between 0 and 1. For a discrete variable, a vector of integers between 0 and 1.
#' @param alpha_pal_na Alpha value to use for the `NA` value. A integer between 0 and 1.
#' @param alpha_title Axis title string. Use `""` for no title.
#' @param alpha_transform For a numeric scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)`), or a named vector of labels (e.g. c("value1" = "label1", ...)).
#' @param facet_labels_position When the facet layout is `"wrap"`, the position of the facet labels. Either `"top"`, `"right"`, `"bottom"` or `"left"`.
#' @param facet_labels_switch When the facet layout is `"grid"`, whether to switch the facet labels to the opposite side of the plot. Either `"x"`, `"y"` or `"both"`.
#' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of `"wrap"`.
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of `"wrap"`.
#' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param facet_space When the facet layout is `"grid"` and facet scales are not `"fixed"`, whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param caption Caption title string.
#' @param titles A function to format unspecified titles. Defaults to `snakecase::to_sentence_case`.
#' @param flipped `TRUE` or `FALSE` or whether the plot is flipped (i.e. horizontal). This affects the positional scale and gridlines defaults.
#'
#' @return A ggplot object.
#' @export
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
#'   gg_blanket(
#'     geom = "violin",
#'     stat = "ydensity",
#'     position = "dodge",
#'     x = sex,
#'     y = body_mass_g,
#'     col = sex,
#'     facet = species,
#'     mode = grey_mode_b(),
#'   )
#'
gg_blanket <- function(
    data = NULL,
    ...,
    geom = "blank",
    stat = "identity",
    position = "identity",
    coord = NULL,
    mode = NULL,
    x = NULL,
    xmin = NULL,
    xmax = NULL,
    xend = NULL,
    y = NULL,
    ymin = NULL,
    ymax = NULL,
    yend = NULL,
    z = NULL,
    col = NULL,
    alpha = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    subgroup = NULL,
    label = NULL,
    text = NULL,
    sample = NULL,
    mapping = NULL,
    x_breaks = NULL,
    x_expand = NULL,
    x_expand_limits = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_oob = scales::oob_keep,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_transform = NULL,
    y_breaks = NULL,
    y_expand = NULL,
    y_expand_limits = NULL,
    y_labels = NULL,
    y_limits = NULL,
    y_oob = scales::oob_keep,
    y_sec_axis = ggplot2::waiver(),
    y_title = NULL,
    y_transform = NULL,
    col_breaks = NULL,
    col_continuous_type = "gradient",
    col_expand = ggplot2::waiver(),
    col_expand_limits = NULL,
    col_labels = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_limits = NULL,
    col_oob = scales::oob_keep,
    col_pal = NULL,
    col_pal_na = "darkgrey",
    col_rescale = scales::rescale(),
    col_title = NULL,
    col_transform = NULL,
    facet_axes = NULL,
    facet_axis_labels = "margins",
    facet_labels = NULL,
    facet_labels_position = "top",
    facet_labels_switch = NULL,
    facet_layout = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    alpha_breaks = NULL,
    alpha_expand = ggplot2::waiver(),
    alpha_expand_limits = NULL,
    alpha_labels = NULL,
    alpha_legend_ncol = NULL,
    alpha_legend_nrow = NULL,
    alpha_legend_rev = FALSE,
    alpha_limits = NULL,
    alpha_oob = scales::oob_keep,
    alpha_pal = NULL,
    alpha_pal_na = NA,
    alpha_title = NULL,
    alpha_transform = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    titles = snakecase::to_sentence_case,
    flipped = NULL
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
  alpha <- rlang::enquo(alpha)

  ##############################################################################
  #stop, warn and inform
  ##############################################################################

  if (!rlang::is_null(mapping)) {
    if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  ##############################################################################
  #identify if theme set
  ##############################################################################

  if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
    theme_set <- FALSE
  }
  else {
    theme_set <- TRUE
  }

  if (rlang::is_null(mode)) {
    mode <- mode_get()
  }

  ##############################################################################
  #make gg_function work
  ##############################################################################

  if (rlang::is_null(data)) data <- data.frame(x = NA)

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
    what = c("POSIXct")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("POSIXct")
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

  if (rlang::is_null(flipped)) {
    if (x_null & !y_null) flipped <- TRUE
    else if ((x_numeric | x_date | x_posixct | x_hms) &
             !(y_null | y_numeric | y_date | y_posixct | y_hms)) {
      flipped <- TRUE
    }
    else if (x_numeric & (y_date | y_posixct | y_hms)) flipped <- TRUE
    else flipped <- FALSE
  }

  ##############################################################################
  #get geom, stat, transform & position strings
  ##############################################################################

  if (ggplot2::is.ggproto(geom)) {
    geom_name <- stringr::str_to_lower(stringr::str_remove(class(geom)[1], "Geom"))
  }
  else if (is.character(geom)) geom_name <- geom

  if (ggplot2::is.ggproto(stat)) {
    stat_name <- stringr::str_to_lower(stringr::str_remove(class(stat)[1], "Stat"))
  }
  else if (is.character(stat)) stat_name <- stat

  if (ggplot2::is.ggproto(position)) {
    position_name <- stringr::str_to_lower(stringr::str_remove(class(position)[1], "Position"))
  }
  else if (is.character(position)) position_name <- position

  ##############################################################################
  #get positional transform defaults - and strings
  ##############################################################################

  #get x_transform if NULL
  if (rlang::is_null(x_transform)) {
    if (x_hms) x_transform <- scales::transform_hms()
    else if (x_posixct) x_transform <- scales::transform_time()
    else if (x_date) x_transform <- scales::transform_date()
    else x_transform <- scales::transform_identity()
  }

  #make a tidy name to deal with composed transforms
  if (is.character(x_transform)) x_transform_name <- x_transform
  else if (inherits(x_transform, what = "transform")) {
    x_transform_name <- x_transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  #get y_transform if NULL
  if (rlang::is_null(y_transform)) {
    if (y_hms) y_transform <- scales::transform_hms()
    else if (y_posixct) y_transform <- scales::transform_time()
    else if (y_date) y_transform <- scales::transform_date()
    else y_transform <- scales::transform_identity()
  }

  #make a tidy name to deal with composed transforms
  if (is.character(y_transform)) y_transform_name <- y_transform
  else if (inherits(y_transform, what = "transform")) {
    y_transform_name <- y_transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

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
        !!col, !!alpha, !!facet, !!facet2,
        !!group, !!subgroup, !!label, !!sample),
      na_if_inf)) %>%
    #convert logicals to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!alpha, !!facet, !!facet2) &
                                  tidyselect::where(is.logical), function(x)
                                    factor(x, levels = c(TRUE, FALSE)))) %>%
    #convert characters to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!alpha, !!facet, !!facet2) &
                                  tidyselect::where(is.character), function(x)
                                    factor(x))) %>%
    #reverse y*, so that reads top low-levels to bottom high-levels
    dplyr::mutate(dplyr::across(c(!!y, !!ymin, !!ymax, !!yend) &
                                  tidyselect::where(is.factor),
                                function(x) forcats::fct_rev(x)))

  #if flipped, order col correctly
  if ((!identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data))) &
      flipped) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!col & tidyselect::where(is.factor),
                                  function(x) forcats::fct_rev(x)))
  }

  ##############################################################################
  #yet more defaults
  ##############################################################################

  #make drop appropriate to facet scales
  x_drop <- ifelse(facet_scales %in% c("free_x", "free"), TRUE, FALSE)
  y_drop <- ifelse(facet_scales %in% c("free_y", "free"), TRUE, FALSE)

  ##############################################################################
  # add ggplot() with aesthetics
  ##############################################################################

  if (rlang::quo_is_null(alpha)) {
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
            text = !!text,
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
            text = !!text,
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
            text = !!text,
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
            text = !!text,
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
            text = !!text,
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
            text = !!text,
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
            text = !!text,
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
            text = !!text,
            !!!mapping
          ))
      }
    }
  }
  else {
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
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
            text = !!text,
            alpha = !!alpha, !!!mapping
          ))
      }
    }
  }

  ##############################################################################
  # add faceting
  ##############################################################################

  #get layout if NULL
  if (rlang::is_null(facet_layout)) {
    if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) facet_layout <- "wrap"
    else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else facet_layout <- "null"
  }

  if (rlang::is_null(facet_axes)) {
    if (flipped) facet_axes <- "all_y"
    else facet_axes <- "all_x"
  }

  #add facet layer. Reverse facet if y is already reversed to keep order correct
  if (!y_continuous & (identical(rlang::eval_tidy(y, data), rlang::eval_tidy(facet, data)))) {
    if (facet_layout == "wrap") {
      if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(forcats::fct_rev(!!facet)),
            scales = facet_scales,
            drop = FALSE,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels),
            strip.position = facet_labels_position
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet2),
            scales = facet_scales,
            drop = FALSE,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels),
            strip.position = facet_labels_position
          )
      }
      else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet, !!facet2),
            scales = facet_scales,
            drop = FALSE,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels),
            strip.position = facet_labels_position
          )
      }
    }
    else if (facet_layout == "grid") {
      if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(switch = facet_labels_switch,
                              rows = ggplot2::vars(!!facet2),
                              cols = ggplot2::vars(forcats::fct_rev(!!facet)),
                              scales = facet_scales,
                              space = facet_space,
                              drop = FALSE,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(switch = facet_labels_switch,
                              cols = ggplot2::vars(forcats::fct_rev(!!facet)),
                              scales = facet_scales,
                              space = facet_space,
                              drop = FALSE,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(switch = facet_labels_switch,
                              rows = ggplot2::vars(!!facet2),
                              scales = facet_scales,
                              space = facet_space,
                              drop = FALSE,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
  }
  else {
    if (facet_layout == "wrap") {
      if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet),
            scales = facet_scales,
            drop = FALSE,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels),
            strip.position = facet_labels_position
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet2),
            scales = facet_scales,
            drop = FALSE,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels),
            strip.position = facet_labels_position
          )
      }
      else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(!!facet, !!facet2),
            scales = facet_scales,
            drop = FALSE,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels),
            strip.position = facet_labels_position
          )
      }
    }
    else if (facet_layout == "grid") {
      if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(switch = facet_labels_switch,
                              rows = ggplot2::vars(!!facet2),
                              cols = ggplot2::vars(!!facet),
                              scales = facet_scales,
                              space = facet_space,
                              drop = FALSE,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(switch = facet_labels_switch,
                              cols = ggplot2::vars(!!facet),
                              scales = facet_scales,
                              space = facet_space,
                              drop = FALSE,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(switch = facet_labels_switch,
                              rows = ggplot2::vars(!!facet2),
                              scales = facet_scales,
                              space = facet_space,
                              drop = FALSE,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
  }

  ##############################################################################
  # Add positional scales pre getting plot data
  ##############################################################################

  if (!rlang::is_null(x_limits)) {
    if (any(x_transform_name %in% "reverse")) x_limits1 <- rev(x_limits)
    else x_limits1 <- x_limits

    plot <- plot +
      ggplot2::scale_x_continuous(limits = x_limits1)
  }

  if (!rlang::is_null(y_limits)) {
    if (any(y_transform_name %in% "reverse")) y_limits1 <- rev(y_limits)
    else y_limits1 <- y_limits

    plot <- plot +
      ggplot2::scale_y_continuous(limits = y_limits1)
  }

  ##############################################################################
  # Add layer
  ##############################################################################

  if (geom_name == "blank") show_legend <- FALSE
  else show_legend <- TRUE

  if (stat_name %in% c("density_2d", "density_2d_filled")) contour <- TRUE
  else contour <- TRUE

  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")

    plot1 <- plot +
      ggplot2::layer_sf(
        geom = geom,
        stat = stat,
        position = position,
        params = rlang::list2(contour = contour, ...),
        show.legend = show_legend,
      ) +
      coord +
      mode
  }
  else {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_cartesian(clip = "off")

    plot1 <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        params = rlang::list2(contour = contour, ...),
        show.legend = show_legend,
      ) +
      coord +
      mode
  }

  ##############################################################################
  # Get plot build and data
  ##############################################################################

  plot_build <- ggplot2::ggplot_build(plot1)
  plot_data <- plot_build$data[[1]]

  facet_nrows <- length(unique(plot_build$layout$layout$ROW))
  facet_ncols <- length(unique(plot_build$layout$layout$COL))

  ##############################################################################
  # Detect whether the plot has a col or alpha scale
  ##############################################################################

  #sf seems to document scales differently, and this fixes
  if (stringr::str_detect(stat_name, "sf")) {
    if (class(rlang::eval_tidy(col, data)) %in%
        c("numeric", "double", "integer","Date", "POSIXct","hms")) {
      is_col_continuous <- TRUE
    }
    else if (class(rlang::eval_tidy(col, data)) %in%
             c("character", "logical", "factor")) {
      is_col_continuous <- FALSE
    }
    else is_col_continuous <- NA

    if (class(rlang::eval_tidy(alpha, data)) %in%
        c("numeric", "double", "integer","Date", "POSIXct","hms")) {
      is_alpha_continuous <- TRUE
    }
    else if (class(rlang::eval_tidy(alpha, data)) %in%
             c("character", "logical", "factor")) {
      is_alpha_continuous <- FALSE
    }
    else is_alpha_continuous <- NA
  }
  #support where col is null, but there is a colour scale
  else {
    scales <- purrr::map_chr(plot_build$plot$scales$scales, function(x) {
      ifelse(rlang::is_null(rlang::call_name(x[["call"]])), NA,
             rlang::call_name(x[["call"]]))
    })

    if (any(scales %in% continuous_scales_col)) is_col_continuous <- TRUE
    else if (any(scales %in% discrete_scales_col)) is_col_continuous <- FALSE
    else is_col_continuous <- NA

    if (any(scales %in% continuous_scales_alpha)) is_alpha_continuous <- TRUE
    else if (any(scales %in% discrete_scales_alpha)) is_alpha_continuous <- FALSE
    else is_alpha_continuous <- NA
  }

  ##############################################################################
  # Remake the plot where there is either no col or no alpha scale identified
  ##############################################################################

  #get params for when no col or alpha aesthetic
  if (is.na(is_col_continuous)) {
    if (rlang::is_null(col_pal)) col_pal1 <- col_pal_discrete(n = 1)
    else col_pal1 <- col_pal[1]
  }

  if (is.na(is_alpha_continuous)) {
    if (rlang::is_null(alpha_pal)) {
      #points or lines.
      if (geom_name %in% c("contour", "density_2d", "density2d", "errorbar", "line", "linerange", "point", "pointrange", "segment", "step", "text")) alpha_pal1 <- 1
      #Polygons that generally have no gap or overlap
      else if (geom_name %in% c("bin_2d", "bin2d", "contour_filled", "density_2d_filled", "density2d_filled", "hex", "raster")) alpha_pal1 <- 1
      #Polygons that generally have key lines within them also default to 0.5
      else if (geom_name %in% c("boxplot", "crossbar", "density", "ribbon", "smooth")) alpha_pal1 <- 0.5
      #Other polygons
      else if (geom_name %in% c("area", "bar", "col", "histogram", "polygon", "rect", "tile", "violin")) alpha_pal1 <- 0.9
      #labels
      else if (geom_name == "label") alpha_pal1 <- 0.1
      #sf
      else if (geom_name %in% c("sf")) alpha_pal1 <- 0.9
      #Blank
      else if (geom_name %in% c("blank")) alpha_pal1 <- NA
      #Everything else
      else alpha_pal1 <- 0.9
    }
    else alpha_pal1 <- alpha_pal[1]
  }

  if ((is.na(is_col_continuous)) & (is.na(is_alpha_continuous))) {
    params_list <- rlang::list2(contour = contour, colour = col_pal1, fill = col_pal1, alpha = alpha_pal1, ...)
  }
  else if (is.na(is_col_continuous)) {
    params_list <- rlang::list2(contour = contour, colour = col_pal1, fill = col_pal1, ...)
  }
  else if (is.na(is_alpha_continuous)) {
    params_list <- rlang::list2(contour = contour, alpha = alpha_pal1, ...)
  }

  #remake plot where either no col or alpha aesthetic
  if (is.na(is_col_continuous) | is.na(is_alpha_continuous)) {
    if (stringr::str_detect(stat_name, "sf")) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom,
          stat = stat,
          position = position,
          params = params_list,
          show.legend = show_legend,
        ) +
        coord +
        mode
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = params_list,
          show.legend = show_legend,
        ) +
        coord +
        mode
    }
  }
  #revert back to the original plot where there is a col or alpha aesthetic
  else {
    plot <- plot1
  }

  ##############################################################################
  # Make colour scale where there is a colour scale identified
  ##############################################################################

  if (!is.na(is_col_continuous)) {
    if (is_col_continuous) {
      if (rlang::is_null(col_pal)) {
        col_pal <- col_pal_continuous()
      }

      #get col_transform if NULL
      if (rlang::is_null(col_transform)) {
        if (inherits(rlang::eval_tidy(col, data), what = "hms")) col_transform <- scales::transform_hms()
        else if (inherits(rlang::eval_tidy(col, data), what = "POSIXct")) col_transform <- scales::transform_time()
        else if (inherits(rlang::eval_tidy(col, data), what = "Date")) col_transform <- scales::transform_date()
        else col_transform <- scales::transform_identity()
      }

      #make a tidy name to deal with composed transforms
      if (is.character(col_transform)) col_transform_name <- col_transform
      else if (inherits(col_transform, what = "transform")) {
        col_transform_name <- col_transform$name %>%
          stringr::str_remove("composition") %>%
          stringr::str_remove("\\(") %>%
          stringr::str_remove("\\)") %>%
          stringr::str_split(",") %>%
          unlist()
      }

      if (rlang::is_null(col_breaks)) {
        if (any(stringr::str_detect(col_transform_name, "log-")) |
            any(col_transform_name %in% c("log", "log2", "log10"))
        ) {
          col_breaks <- scales::breaks_log(n = 5)
        }
        else if (!any(col_transform_name %in% c("identity", "reverse"))) col_breaks <- ggplot2::waiver()
        else col_breaks <- scales::breaks_pretty(n = 5)
      }

      if (rlang::is_null(col_labels)) {
        if (any(col_transform_name %in% c("hms"))) col_labels <- scales::label_time()
        else if (any(col_transform_name %in% c("date", "time"))) col_labels <- scales::label_date_short()
        else if (any(stringr::str_detect(col_transform_name, "log-")) |
                 any(col_transform_name %in% c("log", "log2", "log10"))) {
          col_labels <- scales::label_log()
        }
        else if (!any(col_transform_name %in% c("identity", "reverse"))) col_labels <- ggplot2::waiver()
        else col_labels <- scales::label_comma(drop0trailing = TRUE)
      }

      if (col_continuous_type == "gradient") {
        plot <- plot +
          ggplot2::scale_fill_gradientn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
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
            transform = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
            fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
          )

      }
      else if (col_continuous_type == "steps") {
        plot <- plot +
          ggplot2::scale_fill_stepsn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
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
            transform = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_coloursteps(reverse = col_legend_rev),
            fill = ggplot2::guide_coloursteps(reverse = col_legend_rev)
          ) +
          ggplot2::theme(legend.ticks = ggplot2::element_blank())
      }
    }
    else if (!is_col_continuous) {
      if (!rlang::quo_is_null(col)) {
        col_n <- data %>%
          dplyr::pull(!!col) %>%
          levels() %>%
          length()

        if (rlang::is_null(col_pal)) {
          col_pal <- col_pal_discrete(n = col_n)
        }
        else col_pal <- col_pal[1:col_n]
      }
      else { #guess anything that's ordered represents col,
        #as there is a discrete col scale and no col variable supplied
        plot_data_ordered <- plot_data %>%
          dplyr::summarise(dplyr::across(tidyselect::where(is.ordered), function(x) length(levels(x))))

        if (ncol(plot_data_ordered) == 0) {
          if (rlang::is_null(col_pal)) col_pal <- col_pal_discrete()
        }
        else {
          col_n <- plot_data_ordered %>%
            tidyr::pivot_longer(cols = tidyselect::everything()) %>%
            dplyr::summarise(max(.data$value)) %>%
            dplyr::pull()

          if (rlang::is_null(col_pal)) {
            col_pal <- col_pal_continuous(n = col_n)
          }
          else col_pal <- col_pal[1:col_n]

          col_legend_rev <- !col_legend_rev
        }
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
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          ),
          fill = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          )
        )
    }

    #expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        colour = col_expand_limits,
        fill = col_expand_limits
      )
  }

  ##############################################################################
  # Make alpha scale where there is a alpha scale identified
  ##############################################################################

  if (!is.na(is_alpha_continuous)) {
    if (is_alpha_continuous) {
      if (rlang::is_null(alpha_pal)) {
        alpha_pal <- c(0.1, 1)
      }

      #get alpha_transform if NULL
      if (rlang::is_null(alpha_transform)) {
        if (inherits(rlang::eval_tidy(alpha, data), what = "hms")) alpha_transform <- scales::transform_hms()
        else if (inherits(rlang::eval_tidy(alpha, data), what = "POSIXct")) alpha_transform <- scales::transform_time()
        else if (inherits(rlang::eval_tidy(alpha, data), what = "Date")) alpha_transform <- scales::transform_date()
        else alpha_transform <- scales::transform_identity()
      }

      #make a tidy name to deal with composed transforms
      if (is.character(alpha_transform)) alpha_transform_name <- alpha_transform
      else if (inherits(alpha_transform, what = "transform")) {
        alpha_transform_name <- alpha_transform$name %>%
          stringr::str_remove("composition") %>%
          stringr::str_remove("\\(") %>%
          stringr::str_remove("\\)") %>%
          stringr::str_split(",") %>%
          unlist()
      }

      if (rlang::is_null(alpha_breaks)) {
        if (any(stringr::str_detect(alpha_transform_name, "log-")) |
            any(alpha_transform_name %in% c("log", "log2", "log10"))
        ) {
          alpha_breaks <- scales::breaks_log(n = 5)
        }
        else if (!any(alpha_transform_name %in% c("identity", "reverse"))) alpha_breaks <- ggplot2::waiver()
        else alpha_breaks <- scales::breaks_pretty(n = 5)
      }

      if (rlang::is_null(alpha_labels)) {
        if (any(alpha_transform_name %in% c("hms"))) alpha_labels <- scales::label_time()
        else if (any(alpha_transform_name %in% c("date", "time"))) alpha_labels <- scales::label_date_short()
        else if (any(stringr::str_detect(alpha_transform_name, "log-")) |
                 any(alpha_transform_name %in% c("log", "log2", "log10"))) {
          alpha_labels <- scales::label_log()
        }
        else if (!any(alpha_transform_name %in% c("identity", "reverse"))) alpha_labels <- ggplot2::waiver()
        else alpha_labels <- scales::label_comma(drop0trailing = TRUE)
      }

      plot <- plot +
        ggplot2::scale_alpha_continuous(
          range = alpha_pal,
          limits = alpha_limits,
          expand = alpha_expand,
          breaks = alpha_breaks,
          labels = alpha_labels,
          transform = alpha_transform,
          oob = alpha_oob,
          na.value = alpha_pal_na,
        ) +
        ggplot2::guides(
          alpha = ggplot2::guide_legend(
            reverse = TRUE
          )
        )
    }
    else if (!is_alpha_continuous) {
      if (!rlang::quo_is_null(alpha)) {
        alpha_n <- data %>%
          dplyr::pull(!!alpha) %>%
          levels() %>%
          length()

        if (rlang::is_null(alpha_pal)) {
          alpha_pal <- seq(from = 0.1, to = 1, by = (1 - 0.1) / (alpha_n - 1))
        }
      }
      else { #guess anything that's ordered represents alpha,
        #as there is a discrete alpha scale and no alpha variable supplied
        plot_data_ordered <- plot_data %>%
          dplyr::summarise(dplyr::across(tidyselect::where(is.ordered), function(x) length(levels(x))))

        if (ncol(plot_data_ordered) == 0) {
          if (rlang::is_null(alpha_pal)) alpha_pal <- rep(1, times = 10)
        }
        else {
          alpha_n <- plot_data_ordered %>%
            tidyr::pivot_longer(cols = tidyselect::everything()) %>%
            dplyr::summarise(max(.data$value)) %>%
            dplyr::pull()

          if (rlang::is_null(alpha_pal)) {
            alpha_pal <- seq(from = 0.1, to = 1, by = (1 - 0.1) / (alpha_n - 1))
          }
          # alpha_legend_rev <- !alpha_legend_rev
        }
      }

      if ((identical(rlang::eval_tidy(col, data), rlang::eval_tidy(alpha, data))) &
          flipped) {
        alpha_legend_rev <- !(alpha_legend_rev)
        alpha_pal <- rev(alpha_pal)
      }

      if (rlang::is_null(alpha_labels)) alpha_labels <- ggplot2::waiver()

      if (rlang::is_null(alpha_breaks)) alpha_breaks <- ggplot2::waiver()

      plot <- plot +
        ggplot2::scale_alpha_manual(
          values = alpha_pal,
          limits = alpha_limits,
          expand = alpha_expand,
          breaks = alpha_breaks,
          labels = alpha_labels,
          na.value = alpha_pal_na,
          drop = FALSE, #consider add argument
        ) +
        ggplot2::guides(
          alpha = ggplot2::guide_legend(
            reverse = alpha_legend_rev,
            ncol = alpha_legend_ncol,
            nrow = alpha_legend_nrow
          )
        )
    }

    #expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        alpha = alpha_expand_limits
      )
  }

  ##############################################################################
  # Positional scales
  ##############################################################################

  #Make x scale based on plot_data
  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
    if (rlang::is_null(x_transform)) x_transform <- scales::transform_identity()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_x_continuous(
          limits = x_limits,
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels,
          oob = x_oob,
          sec.axis = x_sec_axis,
          transform = x_transform
        )
    })
  }
  else if (!x_continuous) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_x_discrete(
          limits = x_limits,
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels,
          drop = x_drop
        )
    })
  }
  else {
    #get x_labels if NULL
    if (rlang::is_null(x_labels)) {
      if (any(x_transform_name %in% c("hms"))) x_labels <- scales::label_time()
      else if (any(x_transform_name %in% c("date", "time"))) x_labels <- scales::label_date_short()
      else if (any(stringr::str_detect(x_transform_name, "log-")) |
               any(x_transform_name %in% c("log", "log2", "log10"))) {
        x_labels <- scales::label_log()
      }
      else if (!any(x_transform_name %in% c("identity", "reverse"))) x_labels <- ggplot2::waiver()
      else x_labels <- scales::label_comma(drop0trailing = TRUE)
    }

    #get x_breaks_n if x_breaks is NULL
    if (rlang::is_null(x_breaks)) {
      if (facet_ncols == 1) x_breaks_n <- 6
      else if (facet_ncols == 2) x_breaks_n <- 4
      else if (facet_ncols == 3) x_breaks_n <- 3
      else x_breaks_n <- 2
    }

    #get x_expand and x_breaks for non-pretty scales situation
    if (!flipped |
        facet_scales %in% c("free", "free_x") |
        length(x_transform_name) > 1 |
        !any(x_transform_name %in% c("identity", "reverse", "date", "time", "hms")) |
        !rlang::is_null(x_expand)) {

      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()

      if (rlang::is_null(x_breaks)) {
        if (any(stringr::str_detect(x_transform_name, "log-")) |
            any(x_transform_name %in% c("log", "log2", "log10"))
        ) {
          x_breaks <- scales::breaks_log(n = x_breaks_n)
        } else {
          x_breaks <- scales::breaks_pretty(n = x_breaks_n)
        }
      }
    }
    #make binned scales correct when a non-identity transform selected
    else if (flipped &
             stringr::str_detect(stat_name, "bin") &
             !identical(y_transform_name, "identity")) {

      if (rlang::is_null(x_expand)) x_expand <- ggplot2::expansion(mult = c(0, 0.05))

      if (rlang::is_null(x_breaks)) {
        x_breaks <- scales::breaks_pretty(n = x_breaks_n)
      }

      if (rlang::is_null(x_limits)) x_limits <- c(0, NA)
    }
    #get x_limits and x_breaks for complex situation
    else {
      if (!rlang::is_null(x_limits)) {
        if (rlang::is_null(x_breaks)) x_breaks <- scales::breaks_pretty(n = x_breaks_n)
      }
      else {
        x_vars_str <- "^x$|^xmin$|^xmax$|^xend$|^xmin_final$|^xmax_final$"

        x_vctr <- plot_data %>%
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(x_vars_str)), function(x) !is.na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(x_vars_str))) %>%
          tidyr::pivot_longer(cols = tidyselect::everything()) %>%
          dplyr::pull(.data$value)

        if (!rlang::is_null(x_expand_limits)) {
          if (any(x_transform_name %in% "reverse")) {
            x_vctr <- c(x_vctr, x_expand_limits * -1)
          }
          else x_vctr <- c(x_vctr, x_expand_limits)
        }

        if (x_hms) x_vctr <- hms::as_hms(x_vctr)
        else if (x_posixct) x_vctr <- lubridate::as_datetime(x_vctr, origin = "1970-01-01")
        else if (x_date) x_vctr <- lubridate::as_date(x_vctr, origin = "1970-01-01")

        #get the range from the vctr
        x_range <- range(x_vctr, na.rm = TRUE)
        if (x_hms) x_range <- hms::as_hms(x_range)

        if (any(x_transform_name %in% "reverse")) {
          x_range <- sort(x_range, decreasing = TRUE)
        }

        if (position_name == "fill") {
          if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
        }
        else if (rlang::is_null(x_breaks)) {
          x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)
          if (rlang::is_null(x_limits)) x_limits <- sort(range(x_breaks))
        }
        else {
          if (rlang::is_null(x_limits)) {
            if (is.function(x_breaks)) {
              x_breaks <- x_breaks(x_range)
              x_limits <- range(x_breaks)
            }
            else if (is.vector(x_breaks)) x_limits <- sort(range(x_breaks))
          }
        }

        if (any(x_transform_name %in% "reverse")) {
          if (!rlang::is_null(x_limits)) {
            x_limits <- sort(x_limits, decreasing = TRUE)
          }
        }
      }

      #and x_expand
      if (rlang::is_null(x_expand)) {
        if (!rlang::is_null(x_limits)) {
          if (identical(x_limits, c(NA, NA))) {
            x_expand <- ggplot2::waiver()
          }
          else if (identical(x_limits, c(0, NA))) {
            x_expand <- ggplot2::expansion(mult = c(0, 0.05))
          }
          else if (identical(x_limits, c(NA, 0))) {
            x_expand <- ggplot2::expansion(mult = c(0.05, 0))
          }
          else if (identical(x_limits, c(lubridate::NA_Date_, lubridate::NA_Date_))) {
            x_expand <- ggplot2::waiver()
          }
          else x_expand <- c(0, 0)
        }
        else x_expand <- c(0, 0)
      }
    }

    #make the x_scale
    suppressMessages({
      plot <- plot +
        ggplot2::scale_x_continuous(
          limits = x_limits,
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels,
          oob = x_oob,
          sec.axis = x_sec_axis,
          transform = x_transform
        )
    })
  }

  #Make y scale based on plot_data
  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
    if (rlang::is_null(y_transform)) y_transform <- scales::transform_identity()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_continuous(
          limits = y_limits,
          expand = y_expand,
          labels = y_labels,
          breaks = y_breaks,
          oob = y_oob,
          sec.axis = y_sec_axis,
          transform = y_transform
        )
    })
  }
  else if (!y_continuous) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_discrete(
          limits = y_limits,
          expand = y_expand,
          breaks = y_breaks,
          labels = y_labels,
          drop = y_drop
        )
    })
  }
  else {
    #get y_labels if NULL
    if (rlang::is_null(y_labels)) {
      if (any(y_transform_name %in% c("hms"))) y_labels <- scales::label_time()
      else if (any(y_transform_name %in% c("date", "time"))) y_labels <- scales::label_date_short()
      else if (any(stringr::str_detect(y_transform_name, "log-")) |
               any(y_transform_name %in% c("log", "log2", "log10"))) {
        y_labels <- scales::label_log()
      }
      else if (!any(y_transform_name %in% c("identity", "reverse"))) y_labels <- ggplot2::waiver()
      else y_labels <- scales::label_comma(drop0trailing = TRUE)
    }

    #get y_breaks_n if y_breaks is NULL
    if (rlang::is_null(y_breaks)) {
      if (facet_nrows == 1) y_breaks_n <- 6
      else if (facet_nrows == 2) y_breaks_n <- 5
      else if (facet_nrows == 3) y_breaks_n <- 4
      else y_breaks_n <- 3
    }

    #get y_expand and y_breaks for non-pretty scales situation
    if (flipped |
        facet_scales %in% c("free", "free_y") |
        length(y_transform_name) > 1 |
        !any(y_transform_name %in% c("identity", "reverse", "date", "time", "hms")) |
        !rlang::is_null(y_expand)) {

      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()

      if (rlang::is_null(y_breaks)) {
        if (any(stringr::str_detect(y_transform_name, "log-")) |
            any(y_transform_name %in% c("log", "log2", "log10"))
        ) {
          y_breaks <- scales::breaks_log(n = y_breaks_n)
        } else {
          y_breaks <- scales::breaks_pretty(n = y_breaks_n)
        }
      }
    }
    #make binned scales correct when a non-identity transform selected
    else if (!flipped &
             stringr::str_detect(stat_name, "bin") &
             !identical(x_transform_name, "identity")) {

      if (rlang::is_null(y_expand)) y_expand <- ggplot2::expansion(mult = c(0, 0.05))

      if (rlang::is_null(y_breaks)) {
        y_breaks <- scales::breaks_pretty(n = y_breaks_n)
      }

      if (rlang::is_null(y_limits)) y_limits <- c(0, NA)
    }
    #get y_limits and y_breaks for complex situation
    else {
      if (!rlang::is_null(y_limits)) {
        if (rlang::is_null(y_breaks)) y_breaks <- scales::breaks_pretty(n = y_breaks_n)
      }
      else {
        y_vars_str <- "^y$|^ymin$|^ymax$|^yend$|^ymin_final$|^ymax_final$"

        y_vctr <- plot_data %>%
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(y_vars_str)), function(x) !is.na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(y_vars_str))) %>%
          tidyr::pivot_longer(cols = tidyselect::everything()) %>%
          dplyr::pull(.data$value)

        if (!rlang::is_null(y_expand_limits)) {
          if (any(y_transform_name %in% "reverse")) {
            y_vctr <- c(y_vctr, y_expand_limits * -1)
          }
          else y_vctr <- c(y_vctr, y_expand_limits)
        }

        if (y_hms) y_vctr <- hms::as_hms(y_vctr)
        else if (y_posixct) y_vctr <- lubridate::as_datetime(y_vctr, origin = "1970-01-01")
        else if (y_date) y_vctr <- lubridate::as_date(y_vctr, origin = "1970-01-01")

        #get the range from the vctr
        y_range <- range(y_vctr, na.rm = TRUE)
        if (y_hms) y_range <- hms::as_hms(y_range)

        if (any(y_transform_name %in% "reverse")) {
          y_range <- sort(y_range, decreasing = TRUE)
        }

        if (position_name == "fill") {
          if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
        }
        else if (rlang::is_null(y_breaks)) {
          y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)
          if (rlang::is_null(y_limits)) y_limits <- sort(range(y_breaks))
        }
        else {
          if (rlang::is_null(y_limits)) {
            if (is.function(y_breaks)) {
              y_breaks <- y_breaks(y_range)
              y_limits <- range(y_breaks)
            }
            else if (is.vector(y_breaks)) y_limits <- sort(range(y_breaks))
          }
        }

        if (any(y_transform_name %in% "reverse")) {
          if (!rlang::is_null(y_limits)) {
            y_limits <- sort(y_limits, decreasing = TRUE)
          }
        }
      }

      #and y_expand
      if (rlang::is_null(y_expand)) {
        if (!rlang::is_null(y_limits)) {
          if (identical(y_limits, c(NA, NA))) {
            y_expand <- ggplot2::waiver()
          }
          else if (identical(y_limits, c(0, NA))) {
            y_expand <- ggplot2::expansion(mult = c(0, 0.05))
          }
          else if (identical(y_limits, c(NA, 0))) {
            y_expand <- ggplot2::expansion(mult = c(0.05, 0))
          }
          else if (identical(y_limits, c(lubridate::NA_Date_, lubridate::NA_Date_))) {
            y_expand <- ggplot2::waiver()
          }
          else y_expand <- c(0, 0)
        }
        else y_expand <- c(0, 0)
      }
    }

    #make the y_scale
    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_continuous(
          limits = y_limits,
          expand = y_expand,
          breaks = y_breaks,
          labels = y_labels,
          oob = y_oob,
          sec.axis = y_sec_axis,
          transform = y_transform
        )
    })
  }

  #expand limits if necessary
  if (!rlang::is_null(x_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(x = x_expand_limits)
  }
  if (!rlang::is_null(y_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(y = y_expand_limits)
  }

  #############################################################################
  # titles
  #############################################################################
  if (rlang::is_null(x_title)) {
    if (stringr::str_detect(stat_name, "sf")) {
      x_title <- ""
    }
    else if (!rlang::is_null(plot_build$plot$labels$x)) {
      x_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), titles)
    }
  }
  if (rlang::is_null(y_title)) {
    if (stringr::str_detect(stat_name, "sf")) {
      y_title <- ""
    }
    else if (!rlang::is_null(plot_build$plot$labels$y)) {
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
  if (rlang::is_null(alpha_title)) {
    if (!rlang::is_null(plot_build$plot$labels$alpha)) {
      alpha_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), titles)
    }
  }

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_title,
      y = y_title,
      alpha = alpha_title
    )

  if (!rlang::is_null(col_title)) {
    plot <- plot +
      ggplot2::labs(col = col_title, fill = col_title)
  }
  if (!rlang::is_null(alpha_title)) {
    plot <- plot +
      ggplot2::labs(alpha = alpha_title)
  }

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
  if (!rlang::is_null(alpha_title)) {
    if (alpha_title == "") {
      plot <- plot +
        ggplot2::labs(alpha = NULL)
    }
  }

  ##############################################################################
  # mode auto gridlines, axis-line/ticks removal
  ##############################################################################

  if (stringr::str_detect(stat_name, "sf")) {
    plot <- plot +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) +
      ggplot2::theme(axis.line = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  }
  else if (flipped) {
    plot <- plot +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  }
  else {
    plot <- plot +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  }

  ##############################################################################
  # plot
  ##############################################################################

  if (theme_set) {
    plot <- plot +
      ggplot2::theme_get()
  }

  return(plot)
}

#' Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around [ggplot2::ggplot()] + `layer()` with [geom_blank()][ggplot2::geom_blank()] defaults. This function underlies all other `gg_*` functions. It contains a `geom` argument for maximum flexibility.
#'
#' @param data A data frame or tibble.
#' @param ... Other arguments passed to within a `params` list in `layer()`.
#' @param geom A geometric object to display the data. A snakecase character string of a ggproto Geom subclass object minus the Geom prefix (e.g. `"point"`).
#' @param stat A statistical transformation to use on the data. A snakecase character string of a ggproto Stat subclass object minus the Stat prefix (e.g. `"identity"`).
#' @param position A position adjustment. A snakecase character string of a ggproto Position subclass object minus the Position prefix (e.g. `"identity"`), or a `position_*()` function that outputs a ggproto Position subclass object (e.g. `ggplot2::position_identity()`).
#' @param coord A coordinate system. A `coord_*()` function that outputs a constructed ggproto Coord subclass object (e.g. [ggplot2::coord_cartesian()]).
#' @param mode A `*_mode_*` theme (e.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()]). This argument adds the theme with side-effects, as the `gg_*` function will removes selected gridlines/axis-line/ticks. To avoid these side-effects, `+` the theme on to the output of `gg_*`.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,facet,facet2,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' @param mapping A set of additional aesthetic mappings in [ggplot2::aes()]. Intended primarily for non-supported aesthetics (e.g. `shape`, `linetype`, `linewidth`, or `size`), but can also be used for delayed evaluation etc.
#' @param x_breaks,y_breaks,col_breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param x_expand,y_expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param x_expand_limits,y_expand_limits,col_expand_limits For a continuous variable, any values that the limits should encompass (e.g. `0`). For a discrete scale, manipulate the data instead with `forcats::fct_expand`.
#' @param x_label,y_label,col_label Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)` for no title.
#' @param x_labels,y_labels,col_labels,facet_labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels. (Note this must be named for `facet_labels`).
#' @param x_limits,y_limits,col_limits For a continuous scale, a vector of length 2 to determine the limits of the scale. For a discrete scale, manipulate the data instead with `factor`, `forcats::fct_expand` or `forcats::fct_drop`.
#' @param x_oob,y_oob,col_oob For a continuous scale, a `scales::oob_*` function of how to handle values outside of limits. Defaults to `scales::oob_keep`.
#' @param x_position,y_position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).If using `y_position = "top"` with a `*_mode_*` theme, add `caption = ""` or `caption = "\n"`.
#' @param x_transform,y_transform,col_transform For a continuous scale, a transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#' @param col_drop,facet_drop For a discrete variable, FALSE or TRUE of whether to drop unused levels.
#' @param col_legend_ncol,col_legend_nrow The number of columns and rows in a legend guide.
#' @param col_legend_rev `TRUE` or `FALSE` of whether to reverse the elements of a legend guide. Defaults to `FALSE`.
#' @param col_palette A character vector of hex codes (or names) or a `scales::pal_*()` function.
#' @param col_palette_na A hex code (or name) for the colour of `NA` values.
#' @param col_rescale For a continuous variable, a `scales::rescale()` function.
#' @param col_steps For a continuous variable, `TRUE` or `FALSE` of whether to colour in steps. Defaults to `FALSE`.
#' @param facet_axes Whether to add interior axes and ticks with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`. Sometimes `+ *_mode_*()` may be needed.
#' @param facet_axis_labels Whether to add interior axis labels with `"margins"`, `"all"`, `"all_x"`, or `"all_y"`.
#' @param facet_layout Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a single `facet` (or `facet2`) argument is provided, then defaults to `"wrap"`. If `NULL` and both facet and facet2 arguments are provided, defaults to `"grid"`.
#' @param facet_ncol,facet_nrow The number of columns and rows of facet panels. Only applies to a facet layout of `"wrap"`.
#' @param facet_scales Whether facet scales should be `"fixed"` across facets, `"free"` in both directions, or free in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param facet_space When the facet layout is `"grid"` and facet scales are not `"fixed"`, whether facet space should be `"fixed"` across facets, `"free"` to be proportional in both directions, or free to be proportional in just one direction (i.e. `"free_x"` or `"free_y"`). Defaults to `"fixed"`.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param caption Caption title string.
#' @param label_to_case A function to format the default `x_label`, `y_label` and `col_label` of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins %>%
#'   tidyr::drop_na(sex) %>%
#'   mutate(across(sex, \(x) stringr::str_to_sentence(x))) %>%
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
gg_blanket <- function(data = NULL,
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
                       x_position = "bottom",
                       x_label = NULL,
                       x_transform = NULL,
                       y_breaks = NULL,
                       y_expand = NULL,
                       y_expand_limits = NULL,
                       y_labels = NULL,
                       y_limits = NULL,
                       y_oob = scales::oob_keep,
                       y_position = "left",
                       y_label = NULL,
                       y_transform = NULL,
                       col_breaks = NULL,
                       col_drop = FALSE,
                       col_expand_limits = NULL,
                       col_labels = NULL,
                       col_legend_ncol = NULL,
                       col_legend_nrow = NULL,
                       col_legend_rev = FALSE,
                       col_limits = NULL,
                       col_oob = scales::oob_keep,
                       col_palette = NULL,
                       col_palette_na = NULL,
                       col_rescale = scales::rescale(),
                       col_steps = FALSE,
                       col_label = NULL,
                       col_transform = NULL,
                       facet_axes = NULL,
                       facet_axis_labels = "margins",
                       facet_drop = FALSE,
                       facet_labels = NULL,
                       facet_layout = NULL,
                       facet_ncol = NULL,
                       facet_nrow = NULL,
                       facet_scales = "fixed",
                       facet_space = "fixed",
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       label_to_case = snakecase::to_sentence_case) {

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
    if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
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

  if (x_null & !y_null) flipped <- TRUE
  else if ((x_numeric | x_date | x_posixct | x_hms) &
           !(y_null | y_numeric | y_date | y_posixct | y_hms)) {
    flipped <- TRUE
  }
  else if (x_numeric & (y_date | y_posixct | y_hms)) flipped <- TRUE
  else flipped <- FALSE

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
    #convert to factors class that can handle labels
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!facet, !!facet2) &
                                  (tidyselect::where(is.character) | tidyselect::where(is.factor) | tidyselect::where(is.logical)),
                                function(x) labelled::to_factor(x))) %>%
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
  #get more defaults
  ##############################################################################

  #make drop appropriate to facet scales
  x_drop <- ifelse(facet_scales %in% c("free_x", "free"), TRUE, FALSE)
  y_drop <- ifelse(facet_scales %in% c("free_y", "free"), TRUE, FALSE)

  #get mode
  if (rlang::is_null(mode)) {
    mode <- get_mode()
  }

  ##############################################################################
  # add ggplot() with aesthetics
  ##############################################################################

  # if (rlang::quo_is_null(col)) {
  #   if (!x_null & !y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         x = !!x,
  #         y = !!y,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  #   else if (!x_null & y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         x = !!x,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  #   else if (x_null & !y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         y = !!y,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  #   else if (x_null & y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  # }
  # else {
  #   if (!x_null & !y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         x = !!x,
  #         y = !!y,
  #         col = !!col,
  #         fill = !!col,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  #   else if (!x_null & y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         x = !!x,
  #         col = !!col,
  #         fill = !!col,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  #   else if (x_null & !y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         y = !!y,
  #         col = !!col,
  #         fill = !!col,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  #   else if (x_null & y_null) {
  #     plot <- data %>%
  #       ggplot2::ggplot(mapping = ggplot2::aes(
  #         col = !!col,
  #         fill = !!col,
  #         xmin = !!xmin,
  #         xmax = !!xmax,
  #         xend = !!xend,
  #         ymin = !!ymin,
  #         ymax = !!ymax,
  #         yend = !!yend,
  #         z = !!z,
  #         group = !!group,
  #         subgroup = !!subgroup,
  #         sample = !!sample,
  #         label = !!label,
  #         text = !!text,
  #         # !!!mapping
  #       )) +
  #       mode
  #   }
  # }

  plot <- get_base(
    data = data,
    x = !!x,
    y = !!y,
    col = !!col,
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
  ) +
  mode

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
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
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
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
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
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
    else if (facet_layout == "grid") {
      if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              cols = ggplot2::vars(forcats::fct_rev(!!facet)),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(cols = ggplot2::vars(forcats::fct_rev(!!facet)),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
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
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
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
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
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
            drop = facet_drop,
            axes = facet_axes,
            axis.labels = facet_axis_labels,
            nrow = facet_nrow,
            ncol = facet_ncol,
            labeller = ggplot2::as_labeller(facet_labels)
          )
      }
    }
    else if (facet_layout == "grid") {
      if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              cols = ggplot2::vars(!!facet),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(cols = ggplot2::vars(!!facet),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
                              axes = facet_axes,
                              axis.labels = facet_axis_labels,
                              labeller = ggplot2::as_labeller(facet_labels)
          )
      }
      else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
        plot <- plot +
          ggplot2::facet_grid(rows = ggplot2::vars(!!facet2),
                              scales = facet_scales,
                              space = facet_space,
                              drop = facet_drop,
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

  suppressMessages({
      suppressWarnings({

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
    })
  })

  ##############################################################################
  # Add layer
  ##############################################################################

  if (geom_name == "blank") show_legend <- FALSE
  else show_legend <- TRUE

  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")

    plot <- plot +
      ggplot2::layer_sf(
        geom = geom,
        stat = stat,
        position = position,
        mapping = ggplot2::aes(!!!mapping),
        params = rlang::list2(...),
        show.legend = show_legend,
      ) +
      coord
  }
  else {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_cartesian(clip = "off")

    plot <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        mapping = ggplot2::aes(!!!mapping),
        params = rlang::list2(...),
        show.legend = show_legend,
      ) +
      coord
  }

  if (!rlang::is_null(x_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(x = x_expand_limits)
  }

  if (!rlang::is_null(y_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(y = y_expand_limits)
  }

  ##############################################################################
  # Get plot build and data
  ##############################################################################

  suppressMessages({
    suppressWarnings({
      plot_build <- ggplot2::ggplot_build(plot)
      plot_data <- plot_build$data[[1]]

      facet_nrows <- length(unique(plot_build$layout$layout$ROW))
      facet_ncols <- length(unique(plot_build$layout$layout$COL))
    })
  })

  ##############################################################################
  # Detect scale types
  ##############################################################################

  plot_scales <- purrr::map_chr(plot_build$plot$scales$scales, function(x) {
    ifelse(rlang::is_null(rlang::call_name(x[["call"]])), NA,
           rlang::call_name(x[["call"]]))
  })

  if (any(plot_scales %in% c("scale_colour_discrete", "scale_fill_discrete"))) col_scale_type <- "discrete"
  else if (any(plot_scales %in% c("scale_colour_ordinal", "scale_fill_ordinal"))) col_scale_type <- "ordinal"
  else if (any(plot_scales %in% c("scale_colour_date", "scale_fill_date"))) col_scale_type <- "date"
  else if (any(plot_scales %in% c("scale_colour_datetime", "scale_fill_datetime"))) col_scale_type <- "datetime"
  else if (any(plot_scales %in% c("scale_colour_time", "scale_fill_time"))) col_scale_type <- "time"
  else if (any(plot_scales %in% c("scale_colour_continuous", "scale_fill_continuous"))) col_scale_type <- "numeric"
  else col_scale_type <- "numeric"

  if (any(plot_scales %in% "scale_x_discrete")) x_scale_type <- "discrete"
  else if (any(plot_scales %in% "scale_x_date")) x_scale_type <- "date"
  else if (any(plot_scales %in% "scale_x_datetime")) x_scale_type <- "datetime"
  else if (any(plot_scales %in% "scale_x_time")) x_scale_type <- "time"
  else if (any(plot_scales %in% "scale_x_continuous")) x_scale_type <- "numeric"
  else x_scale_type <- "numeric"

  if (any(plot_scales %in% "scale_y_discrete")) y_scale_type <- "discrete"
  else if (any(plot_scales %in% "scale_y_date")) y_scale_type <- "date"
  else if (any(plot_scales %in% "scale_y_datetime")) y_scale_type <- "datetime"
  else if (any(plot_scales %in% "scale_y_time")) y_scale_type <- "time"
  else if (any(plot_scales %in% "scale_y_continuous")) y_scale_type <- "numeric"
  else y_scale_type <- "numeric"

  ##############################################################################
  # Make colour scale where there is a colour scale identified
  ##############################################################################

  if (!is.na(col_scale_type)) {
    if (col_scale_type %in% c("date", "datetime", "time", "numeric")) {

      if (rlang::is_null(col_palette)) {
        col_palette <- get_col_palette_c()
        if (rlang::is_null(col_palette)) {
          col_palette <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")(seq(0, 1, length.out = 20))
        }
      } else if (rlang::is_function(col_palette)) col_palette <- col_palette(20)

      if (rlang::is_null(col_palette_na)) {
        col_palette_na <- get_col_palette_na_c()
        if (rlang::is_null(col_palette_na)) col_palette_na <- "grey50"
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
        else col_breaks <- scales::breaks_extended(n = 5, only.loose = TRUE)
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

      if (isFALSE(col_steps)) {
        plot <- plot +
          ggplot2::scale_colour_gradientn(
            colours = col_palette,
            values = col_rescale,
            limits = col_limits,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = col_oob,
            na.value = col_palette_na,
            aesthetics = c("colour", "fill")
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(reverse = col_legend_rev),
            fill = ggplot2::guide_colourbar(reverse = col_legend_rev)
          )
      }
      else if (isTRUE(col_steps)) {
        plot <- plot +
          ggplot2::scale_colour_stepsn(
            colours = col_palette,
            values = col_rescale,
            limits = col_limits,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = col_oob,
            na.value = col_palette_na,
            aesthetics = c("colour", "fill")
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_coloursteps(
              reverse = col_legend_rev,
              theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())),
            fill = ggplot2::guide_coloursteps(
              reverse = col_legend_rev,
              theme = ggplot2::theme(legend.ticks = ggplot2::element_blank()))
          )
      }
    }
    else if (col_scale_type %in% c("discrete", "ordinal")) {
      if (col_scale_type == "discrete") {
        if (!rlang::quo_is_null(col)) {
          col_n <- data %>%
            dplyr::pull(!!col) %>%
            levels() %>%
            length()
        } else col_n <- NULL
      } else col_n <- NULL

      if (rlang::is_null(col_palette)) {
        if (col_scale_type == "discrete") col_palette <- get_col_palette_d()
        else if (col_scale_type == "ordinal") col_palette <- get_col_palette_o()
      }

      if (!rlang::is_null(col_n))  {
        if (rlang::is_function(col_palette)) col_palette <- col_palette(col_n)
        else if (!rlang::is_named(col_palette)) col_palette <- col_palette[1:col_n]
      }

      if (flipped) {
        col_legend_rev <- !col_legend_rev
        col_palette <- rev(col_palette)
      }

      if (col_scale_type == "ordinal") col_legend_rev <- !col_legend_rev

      if (rlang::is_null(col_palette_na)) {
        if (col_scale_type == "discrete") col_palette_na <- get_col_palette_na_d()
        else if (col_scale_type == "ordinal") col_palette_na <- get_col_palette_na_o()
      }

      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()

      if (!rlang::is_null(col_palette)) {
        if (rlang::is_vector(col_palette)) {

          plot <- plot +
            ggplot2::scale_colour_manual(
              values = col_palette,
              limits = col_limits,
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
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
        else if (rlang::is_function(col_palette)) {
          plot <- plot +
            ggplot2::discrete_scale(
              palette = col_palette,
              limits = col_limits,
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
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
      }
      else {
        if (rlang::is_null(col_palette_na)) col_palette_na <- "grey50"

        if (col_scale_type == "discrete") {
          plot <- plot +
            ggplot2::scale_colour_hue(
              limits = col_limits,
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
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
        else if (col_scale_type == "ordinal") {
          plot <- plot +
            ggplot2::scale_colour_viridis_d(
              limits = col_limits,
              breaks = col_breaks,
              labels = col_labels,
              na.value = col_palette_na,
              drop = col_drop,
              aesthetics = c("colour", "fill")
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
      }

      if (!rlang::is_null(plot_build$plot$labels$alpha)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
            plot <- plot +
              ggplot2::guides(
                alpha = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
            plot <- plot +
              ggplot2::guides(
                alpha = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$shape)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
            plot <- plot +
              ggplot2::guides(
                shape = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
            plot <- plot +
              ggplot2::guides(
                shape = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$size)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
            plot <- plot +
              ggplot2::guides(
                size = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
            plot <- plot +
              ggplot2::guides(
                size = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$linewidth)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
            plot <- plot +
              ggplot2::guides(
                linewidth = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
            plot <- plot +
              ggplot2::guides(
                linewidth = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$linetype)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
            plot <- plot +
              ggplot2::guides(
                linetype = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
            plot <- plot +
              ggplot2::guides(
                linetype = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

      if (!rlang::is_null(plot_build$plot$labels$pattern)) {
        if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
          if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
            plot <- plot +
              ggplot2::guides(
                pattern = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
        else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
          if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
            plot <- plot +
              ggplot2::guides(
                pattern = ggplot2::guide_legend(
                  reverse = col_legend_rev,
                  ncol = col_legend_ncol,
                  nrow = col_legend_nrow
                )
              )
          }
        }
      }

    }

    #expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        colour = col_expand_limits,
        fill = col_expand_limits
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

    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = x_limits,
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        oob = x_oob,
        position = x_position,
        transform = x_transform
      )
  }
  else if (x_scale_type  == "discrete") {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_x_discrete(
        limits = x_limits,
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        drop = x_drop,
        position = x_position
      )
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

    #get x_breaks_n
    if (facet_ncols == 1) x_breaks_n <- 6
    else if (facet_ncols == 2) x_breaks_n <- 5
    else if (facet_ncols == 3) x_breaks_n <- 4
    else x_breaks_n <- 3

    #get x_expand and x_breaks for non-'symmetric' scales situation
    if (!flipped |
        stringr::str_detect(stat_name, "sf") |
        facet_scales %in% c("free", "free_x") |
        length(x_transform_name) > 1 |
        !any(x_transform_name %in% c("identity", "reverse", "date", "time", "hms")) |
        !rlang::is_null(x_expand)) {

      # x_breaks_n <- x_breaks_n + 1

      if (rlang::is_null(x_expand)) {
        if (any(colnames(plot_data) %in% "xmin")) {
          if (all(plot_data["xmin"] == 0)) x_expand <- ggplot2::expansion(c(0, 0.05))
          else if (all(plot_data["xmax"] == 0)) x_expand <- ggplot2::expansion(c(0.05, 0))
          else x_expand <- ggplot2::waiver()
        } else x_expand <- ggplot2::waiver()
      }

      if (rlang::is_null(x_breaks)) {
        if (any(stringr::str_detect(x_transform_name, "log-")) |
            any(x_transform_name %in% c("log", "log2", "log10"))) {
          x_breaks <- scales::breaks_log(n = x_breaks_n)
        }
        else if (any(x_transform_name %in% c("date", "time", "hms"))) {
          x_breaks <- scales::breaks_pretty(n = x_breaks_n)
        }
        else {
          x_breaks <- scales::breaks_extended(n = x_breaks_n, only.loose = FALSE)
          }
      }
    }
    #get x_limits and x_breaks for 'symmetric'
    else {
      if (!rlang::is_null(x_limits)) {
        if (any(x_transform_name %in% c("date", "time", "hms"))) {
          x_breaks <- scales::breaks_pretty(n = x_breaks_n)
        }
        else {
          x_breaks <- scales::breaks_extended(n = x_breaks_n, only.loose = TRUE)
        }
      }
      else {
        x_vars_str <- "^(?!xid|xbin)x.*" #starts with x & not xid & not xbin

        x_vctr <- plot_data %>%
          dplyr::select(tidyselect::matches(stringr::regex(x_vars_str)))

        if (ncol(x_vctr) != 0) {
          x_vctr <- x_vctr %>%
            tidyr::pivot_longer(cols = tidyselect::everything()) %>%
            dplyr::filter(!is.na(.data$value)) %>%
            dplyr::pull(.data$value)
        } else {
          x_vctr <- c(-Inf, Inf)
        }

        if (!rlang::is_null(x_expand_limits)) {
          x_vctr <- c(x_vctr, x_expand_limits)
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
          if (any(x_transform_name %in% c("date", "time", "hms"))) {
            x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)
          }
          else {
            x_breaks <- scales::breaks_extended(n = x_breaks_n, only.loose = TRUE)(x_range)
          }

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
    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = x_limits,
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels,
        oob = x_oob,
        position = x_position,
        transform = x_transform
      )
  }

  #Make y scale based on plot_data
  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
    if (rlang::is_null(y_transform)) y_transform <- scales::transform_identity()

    plot <- plot +
      ggplot2::scale_y_continuous(
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        breaks = y_breaks,
        oob = y_oob,
        position = y_position,
        transform = y_transform
      )
  }
  else if (y_scale_type  == "discrete") {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

    plot <- plot +
      ggplot2::scale_y_discrete(
        limits = y_limits,
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels,
        drop = y_drop,
        position = y_position
      )
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

    #get y_breaks_n
    if (facet_nrows == 1) y_breaks_n <- 6
    else if (facet_nrows == 2) y_breaks_n <- 5
    else if (facet_nrows == 3) y_breaks_n <- 4
    else y_breaks_n <- 3

    #get y_expand and y_breaks for non-'symmetric' scales situation
    if (flipped |
        stringr::str_detect(stat_name, "sf") |
        facet_scales %in% c("free", "free_y") |
        length(y_transform_name) > 1 |
        !any(y_transform_name %in% c("identity", "reverse", "date", "time", "hms")) |
        !rlang::is_null(y_expand)) {

      # y_breaks_n <- y_breaks_n + 1

      if (rlang::is_null(y_expand)) {
        if (any(colnames(plot_data) %in% "ymin")) {
          if (all(plot_data["ymin"] == 0)) y_expand <- ggplot2::expansion(c(0, 0.05))
          else if (all(plot_data["ymax"] == 0)) y_expand <- ggplot2::expansion(c(0.05, 0))
          else y_expand <- ggplot2::waiver()
        } else y_expand <- ggplot2::waiver()
      }

      if (rlang::is_null(y_breaks)) {
        if (any(stringr::str_detect(y_transform_name, "log-")) |
            any(y_transform_name %in% c("log", "log2", "log10"))) {
          y_breaks <- scales::breaks_log(n = y_breaks_n)
        }
        else if (any(y_transform_name %in% c("date", "time", "hms"))) {
          y_breaks <- scales::breaks_pretty(n = y_breaks_n)
        }
        else {
          y_breaks <- scales::breaks_extended(n = y_breaks_n, only.loose = FALSE)
        }
      }
    }
    #get y_limits and y_breaks for 'symmetric'
    else {
      if (!rlang::is_null(y_limits)) {
        if (any(y_transform_name %in% c("date", "time", "hms"))) {
          y_breaks <- scales::breaks_pretty(n = y_breaks_n)
        }
        else {
          y_breaks <- scales::breaks_extended(n = y_breaks_n, only.loose = TRUE)
        }
      }
      else {
        y_vars_str <- "^(?!yid|ybin)y.*" #starts with y & not yid & not ybin

        y_vctr <- plot_data %>%
          dplyr::select(tidyselect::matches(stringr::regex(y_vars_str)))

        if (ncol(y_vctr) != 0) {
          y_vctr <- y_vctr %>%
            tidyr::pivot_longer(cols = tidyselect::everything()) %>%
            dplyr::filter(!is.na(.data$value)) %>%
            dplyr::pull(.data$value)
        } else {
          y_vctr <- c(-Inf, Inf)
        }

        if (!rlang::is_null(y_expand_limits)) {
          y_vctr <- c(y_vctr, y_expand_limits)
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
          if (any(y_transform_name %in% c("date", "time", "hms"))) {
            y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)
          }
          else {
            y_breaks <- scales::breaks_extended(n = y_breaks_n, only.loose = TRUE)(y_range)
          }

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
    plot <- plot +
      ggplot2::scale_y_continuous(
        limits = y_limits,
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels,
        oob = y_oob,
        position = y_position,
        transform = y_transform
      )
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
  # label_to_case
  #############################################################################

  if (rlang::is_null(x_label)) {
    if ((!rlang::quo_is_null(x))) {
      if (!rlang::is_null(attr(dplyr::pull(data, !!x), "label"))) {
        x_label <- attr(dplyr::pull(data, !!x), "label")
      }
      else {
        if (stringr::str_detect(stat_name, "sf")) {
          x_label <- ""
        }
        else if (!rlang::is_null(plot_build$plot$labels$x)) {
          x_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), label_to_case)
        }
      }
    }
    else {
      if (stringr::str_detect(stat_name, "sf")) {
        x_label <- ""
      }
      else if (!rlang::is_null(plot_build$plot$labels$x)) {
        x_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), label_to_case)
      }
      else x_label <- purrr::map_chr("x", label_to_case)
    }
  }

  if (rlang::is_null(y_label)) {
    if ((!rlang::quo_is_null(y))) {
      if (!rlang::is_null(attr(dplyr::pull(data, !!y), "label"))) {
        y_label <- attr(dplyr::pull(data, !!y), "label")
      }
      else {
        if (stringr::str_detect(stat_name, "sf")) {
          y_label <- ""
        }
        else if (!rlang::is_null(plot_build$plot$labels$y)) {
          y_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$y[1]), label_to_case)
        }
      }
    }
    else {
      if (stringr::str_detect(stat_name, "sf")) {
        y_label <- ""
      }
      else if (!rlang::is_null(plot_build$plot$labels$y)) {
        y_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$y[1]), label_to_case)
      }
      else y_label <- purrr::map_chr("y", label_to_case)
    }
  }

  if (rlang::is_null(col_label)) {
    if ((!rlang::quo_is_null(col))) {
      if (!rlang::is_null(attr(dplyr::pull(data, !!col), "label"))) {
        col_label <- attr(dplyr::pull(data, !!col), "label")
      }
      else {
        if (!rlang::is_null(plot_build$plot$labels$fill)) {
          col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$fill[1]), label_to_case)
        }
        else if (!rlang::is_null(plot_build$plot$labels$colour)) {
          col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$colour[1]), label_to_case)
        }
      }
    }
    else {
      if (!rlang::is_null(plot_build$plot$labels$fill)) {
        col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$fill[1]), label_to_case)
      }
      else if (!rlang::is_null(plot_build$plot$labels$colour)) {
        col_label <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$colour[1]), label_to_case)
      }
    }
  }

  if (!rlang::is_null(plot_build$plot$labels$alpha)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
        alpha_title <- col_label
      }
      else alpha_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$alpha[1])) {
        alpha_title <- col_label
      }
      else alpha_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), label_to_case)
    }
    else {
      alpha_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), label_to_case)
    }
  } else alpha_title <- NULL

  if (!rlang::is_null(plot_build$plot$labels$shape)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
        shape_title <- col_label
      }
      else shape_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$shape[1])) {
        shape_title <- col_label
      }
      else shape_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), label_to_case)
    }
    else {
      shape_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), label_to_case)
    }
  } else shape_title <- NULL

  if (!rlang::is_null(plot_build$plot$labels$size)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
        size_title <- col_label
      }
      else size_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$size[1])) {
        size_title <- col_label
      }
      else size_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), label_to_case)
    }
    else {
      size_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), label_to_case)
    }
  } else size_title <- NULL

  if (!rlang::is_null(plot_build$plot$labels$linewidth)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
        linewidth_title <- col_label
      }
      else linewidth_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linewidth[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linewidth[1])) {
        linewidth_title <- col_label
      }
      else linewidth_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linewidth[1]), label_to_case)
    }
    else {
      linewidth_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linewidth[1]), label_to_case)
    }
  } else linewidth_title <- NULL

  if (!rlang::is_null(plot_build$plot$labels$linetype)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
        linetype_title <- col_label
      }
      else linetype_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$linetype[1])) {
        linetype_title <- col_label
      }
      else linetype_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), label_to_case)
    }
    else {
      linetype_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), label_to_case)
    }
  } else linetype_title <- NULL

  if (!rlang::is_null(plot_build$plot$labels$pattern)) {
    if (!rlang::is_null(plot_build$plot$labels$colour[1])) {
      if (rlang::as_name(plot_build$plot$labels$colour[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
        pattern_title <- col_label
      }
      else pattern_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$pattern[1]), label_to_case)
    }
    else if (!rlang::is_null(plot_build$plot$labels$fill[1])) {
      if (rlang::as_name(plot_build$plot$labels$fill[1]) == rlang::as_name(plot_build$plot$labels$pattern[1])) {
        pattern_title <- col_label
      }
      else pattern_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$pattern[1]), label_to_case)
    }
    else {
      pattern_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$pattern[1]), label_to_case)
    }
  } else pattern_title <- NULL

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_label,
      y = y_label,
      colour = col_label,
      fill = col_label,
      alpha = alpha_title,
      shape = shape_title,
      size = size_title,
      linewidth = linewidth_title,
      linetype = linetype_title,

      pattern = pattern_title
    )

  ##############################################################################
  # auto panel.grid, line & ticks removal
  ##############################################################################

  if (stringr::str_detect(stat_name, "sf")) {
    plot <- plot +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        axis.line.x.top = ggplot2::element_blank(),
        axis.line.x.bottom = ggplot2::element_blank(),
        axis.ticks.x.top = ggplot2::element_blank(),
        axis.ticks.x.bottom = ggplot2::element_blank(),
        axis.minor.ticks.x.top = ggplot2::element_blank(),
        axis.minor.ticks.x.bottom = ggplot2::element_blank(),
        axis.text.x.top = ggplot2::element_blank(),
        axis.text.x.bottom = ggplot2::element_blank(),

        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_blank(),
        axis.line.y.right = ggplot2::element_blank(),
        axis.ticks.y.left = ggplot2::element_blank(),
        axis.ticks.y.right = ggplot2::element_blank(),
        axis.minor.ticks.y.left = ggplot2::element_blank(),
        axis.minor.ticks.y.right = ggplot2::element_blank(),
        axis.text.y.left = ggplot2::element_blank(),
        axis.text.y.right = ggplot2::element_blank()
      )
  }
  else if (flipped) {
    plot <- plot +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.line.x.top = ggplot2::element_blank(),
        axis.line.x.bottom = ggplot2::element_blank(),
        axis.ticks.x.top = ggplot2::element_blank(),
        axis.ticks.x.bottom = ggplot2::element_blank(),
        axis.minor.ticks.x.top = ggplot2::element_blank(),
        axis.minor.ticks.x.bottom = ggplot2::element_blank()
      )
  }
  else {
    plot <- plot +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_blank(),
        axis.line.y.right = ggplot2::element_blank(),
        axis.ticks.y.left = ggplot2::element_blank(),
        axis.ticks.y.right = ggplot2::element_blank(),
        axis.minor.ticks.y.left = ggplot2::element_blank(),
        axis.minor.ticks.y.right = ggplot2::element_blank()
      )
  }

  ##############################################################################
  #add the theme if globally set
  ##############################################################################

  if (rlang::is_null(mode) & rlang::is_null(get_mode())) {
    if (!rlang::is_null(get_theme())) {
      plot <- plot +
        get_theme()
    } else {
      plot <- plot +
        ggplot2::theme_grey()
    }
  }

  return(plot)
}

#' Default colour blind safe categorical palette with 5 colours
#'
#' @description The default colour blind safe 5 colour palette used to colour a categorical col variable.
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_discrete)
#'
#' ggplot2::diamonds |>
#'   gg_bar(x = color,
#'          col = color,
#'          pal = pal_discrete,
#'          width = 0.75)
#'
#' ggplot2::diamonds |>
#'   gg_blank(x = color,
#'            col = color,
#'            stat = "count",
#'            pal = pal_discrete) +
#'   ggplot2::geom_point(stat = "count")
#'
#' ggplot2::diamonds |>
#'   gg_blank(x = color,
#'            col = color,
#'            stat = "count",
#'            pal = pal_discrete,
#'            theme = dark_mode()) +
#'   ggplot2::geom_point(stat = "count")

#'
#' @references Derived from a Datawrapper blog dated 30/03/2022 by Lisa Charlotte Muth.
pal_discrete <- c("#69c2c0", "#941111", "#b2c615", "#2b6999", "#d57236")

#' A colour blind safe categorical palette with 6 colours
#'
#' @description A colour blind safe 6 colour palette used to colour a categorical col variable that works only on a light background.
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_discrete2)
#'
#' ggplot2::diamonds |>
#'   gg_bar(x = color,
#'          col = color,
#'          pal = pal_discrete2,
#'          width = 0.75)
#'
#' ggplot2::diamonds |>
#'   gg_blank(x = color,
#'            col = color,
#'            stat = "count",
#'            pal = pal_discrete2) +
#'   ggplot2::geom_point(stat = "count")
#'
#' @references Derived from a Datawrapper blog dated 30/03/2022 by Lisa Charlotte Muth.
pal_discrete2 <- c("#69c2c0", "#941111", "#b2c615", "#2b6999", "#d57236", "#0c2052")

#' Default blue colour
#'
#' @description A default blue colour used to colour when there is no col variable.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_blue)
pal_blue <- "#2b6999"

#' Default grey colour
#'
#' @description The default grey colour used to colour NA values.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_grey)
pal_grey <- "#7f7f7f"

#' Default colours used in the light_mode theme.
#'
#' @description Default colours used in the light_mode theme for the (1) base text and axis, (2) plot background, (3) panel background and (4) gridlines.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_light_mode)
pal_light_mode <- c("#121b24", "#e6ecf2", "#fcfdfe", "#dbe1e7")

#' Default colours used in the dark_mode theme.
#'
#' @description Default colours used in the dark_mode theme for the (1) base text and axis, (2) plot background, (3) panel background and (4) gridlines.
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_dark_mode)
pal_dark_mode <- c("#bbccdd", "#121b24", "#1f2f3e", "#2c3a48")

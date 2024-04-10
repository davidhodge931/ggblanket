#' Set the default style
#'
#' @description Set the default style by setting the default mode and updating a series of geom and annotate defaults.
#'
#' @param mode A `*_mode_*` set globally for when mode = NULL. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param colour A default geom colour (and fill) for geoms other than those used by `colour_annotate`. Fill inherits from this colour. Defaults to `blue`.
#' @param colour_annotate A default annotate colour used for `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`. Fill inherits from this colour. Defaults to `lightness[1]`.
#' @param ... Provided to support trailing commas only.
#'
#' @return A globally set mode and updated geom defaults.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#' library(palmerpenguins)
#'
#' set_blanket(dark_mode_r(), orange, darkness[1])
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     x_breaks = scales::breaks_pretty(3),
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.25), y = I(0.75), label = "Here")
#'
#' penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     x_breaks = scales::breaks_pretty(3),
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.75), y = I(0.75), label = "Here")
#'
set_blanket <- function(
    mode = light_mode_r(),
    colour = "#357ba2",
    colour_annotate = "#121b24",
    ...
) {
  weave_mode(mode)
  weave_colour( {{ colour }})
  weave_colour_annotate( {{ colour_annotate }})
}


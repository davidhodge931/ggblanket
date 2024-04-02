#' Set the default style
#'
#' @description Set the default style by setting the default mode and updating a series of geom and annotate defaults.
#'
#' @param mode A `*_mode_*` set globally for when mode = NULL. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param geom_colour A default geom colour. The default geom fill inherits from this.
#' @param annotate_colour A default annotate colour used for `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`. The default annotate fill inherits from this.
#' @param ... Provided only to support trailing commas.
#'
#' @return A globally set mode and updated geom defaults.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#' library(palmerpenguins)
#'
#' set_blanket(dark_mode_r(), orange, darkness[2])
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
    geom_colour = blue,
    annotate_colour = lightness[2],
    ...
) {
  weave_mode(mode)
  weave_geom_defaults(geom_colour)
  weave_annotate_defaults(annotate_colour)
}


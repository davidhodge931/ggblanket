#' Set a default style
#'
#' @description Set a default style by setting a default mode, a series of geom and annotate aesthetic defaults, and a default col_palette for discrete and continuous scales.
#'
#' @param mode A default `*_mode_*`. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param geom_colour A default hex colour (and fill) for geoms. Fill inherits from this colour. Defaults to `blue`.
#' @param geom_linewidth A default linewidth for geoms. Fill inherits from this colour. Defaults to 0.66.
#' @param geom_size A default point size for `*_point`. `*_pointrange` multiplies this by 0.25. Defaults to 1.5. .
#' @param annotate_colour A default hex colour (and fill) for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param annotate_linewidth A default linewidth for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to 0.33 (i.e. `linewidthness[1]`).
#' @param annotate_size A default size for `*_text` and `*_label`. Defaults to 3.88.
#' @param annotate_family A default family for `*_text` and `*_label`. Defaults to ""
#' @param col_palette_discrete A default col_palette to use in the discrete scale. A character vector of hex codes (or names).
#' @param col_palette_continuous A default col_palette to use in the continuous scale. A character vector of hex codes (or names).
#' @param theme A default ggplot2 theme to be `+`-ed on unmodified to `gg_*` functions. Note, `mode` takes precedence, unless `mode = NULL`.
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
#' set_blanket(
#'   mode = dark_mode_r(),
#'   geom_colour = orange,
#'   annotate_colour = darkness[1],
#' )
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
    geom_colour = "#357ba2",
    geom_linewidth = 0.66,
    geom_size = 1.5,
    annotate_colour = "#121b24",
    annotate_linewidth = 0.33,
    annotate_size = 3.88,
    annotate_family = "",
    col_palette_discrete = c(teal, orange, navy, pink),
    col_palette_continuous = viridisLite::mako(n = 20, direction = -1),
    theme = light_mode_r(orientation = "x"),
    ...
) {
  weave_mode(mode = mode)

  weave_geom_aes(
    colour = geom_colour,
    linewidth = geom_linewidth,
    size = geom_size
  )

  weave_annotate_aes(
    colour = annotate_colour,
    linewidth = annotate_linewidth,
    size = annotate_size,
    family = annotate_family
  )

  weave_col_palette(
    discrete = col_palette_discrete,
    continuous = col_palette_continuous
  )

  weave_theme(theme = theme)
}


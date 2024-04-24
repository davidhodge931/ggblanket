#' Set a default style
#'
#' @description Set a default style by setting a default mode, a series of geom and annotate aesthetic defaults, and a default col_palette for discrete and continuous scales.
#'
#' @param mode A default `*_mode_*`. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param geom_colour A default hex colour (and fill) for geoms. Fill inherits from this colour. Defaults to `blue`.
#' @param geom_linewidth A default linewidth for geoms. Fill inherits from this colour. Defaults to 0.66.
#' @param geom_size A default point size for `*_point`. `*_pointrange` multiplies this by 0.25. Defaults to 1.5. .
#' @param annotate_colour A default hex colour (and fill) for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to "#121b24" (i.e. `"#121b24"`).
#' @param annotate_linewidth A default linewidth for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to 0.33 (i.e. `0.33`).
#' @param annotate_size A default size for `*_text` and `*_label`. Defaults to 3.88.
#' @param annotate_family A default family for `*_text` and `*_label`. Defaults to ""
#' @param col_palette_d A default col_palette to use in the discrete scale. A character vector of hex codes (or names).
#' @param col_palette_na_d A default colour for NA on a discrete scale. A hex code or name.
#' @param col_palette_c A default col_palette to use in the continuous scale. A character vector of hex codes (or names).
#' @param col_palette_na_c A default colour for NA on a continuous scale. A hex code or name.
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
#'   annotate_colour = "#c8d7df",
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
    col_palette_d = jumble,
    col_palette_na_d = "seashell3",
    col_palette_c = blues9,
    col_palette_na_c = "seashell3",
    theme = light_mode_r(orientation = "x"),
    ...
) {
  weave_mode(new = mode)

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

  weave_col_palette_d(
    new = col_palette_d,
    na = col_palette_na_d
  )

  weave_col_palette_c(
    new = col_palette_c,
    na = col_palette_na_c
  )

  weave_theme(new = theme)
}


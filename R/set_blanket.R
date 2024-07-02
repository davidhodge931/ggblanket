#' Set a style
#'
#' @description
#' Set a style by setting a mode, a series of geom and annotate aesthetic defaults, and a col_palette for discrete and continuous scales.
#'
#' @param ... Provided to force user argument naming etc.
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#' @param geom_colour A hex colour (and fill) for most geoms. Fill inherits from this colour. Defaults to `blue` (i.e. `#357BA2FF`).
#' @param annotate_colour A hex colour (and fill) for other geoms commonly used for annotation (i.e. `*_hline`/`*_vline`/`*_abline` and `*_curve`). Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param annotate_linewidth A linewidth for `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to 0.33.
#' @param annotate_family A family for `*_text` and `*_label`. Defaults to "".
#' @param annotate_size A size for `*_text` and `*_label`. Defaults to 3.88.
#' @param col_palette_d For a discrete scale, a character vector of hex codes (or names) for the `col_palette`.
#' @param col_palette_c For a continuous scale, a character vector of hex codes (or names) for the `col_palette`.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function for the `col_palette`.
#' @param col_palette_na_d For a discrete scale, a hex code or name for the `col_palette_na`.
#' @param col_palette_na_c For a continuous scale, a hex code or name for the `col_palette_na`.
#' @param col_palette_na_o For an ordinal scale, a hex code or name for the `col_palette_na`.
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects. Note, `mode` takes precedence, unless `mode = NULL`.
#'
#' @return A globally set style.
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
    ...,
    mode = light_mode_r(),
    geom_colour = "#357BA2FF",
    annotate_colour = "#121B24FF",
    annotate_linewidth = 0.33,
    annotate_family = "",
    annotate_size = 3.88,
    col_palette_d = jumble,
    col_palette_na_d = "#CDC5BFFF",
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    col_palette_na_c = "#988F88FF",
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_o = "#988F88FF",
    theme = ggplot2::theme_grey()) {

  weave_mode(new = mode)

  weave_geom_aes(colour = geom_colour)

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

  weave_col_palette_o(
    new = col_palette_o,
    na = col_palette_na_o
  )

  weave_col_palette_c(
    new = col_palette_c,
    na = col_palette_na_c
  )

  weave_theme(new = theme)
}


#' Set the style
#'
#' @description
#' Set the style by setting:
#'
#' 1. the geom defaults, including the colour (and fill) of geoms
#' 2. the colour (and fill) palettes (i.e. discrete, continuous and ordinal)
#' 3. the theme, and how/what side-effects are to be applied
#' 4. the function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc.
#'
#' `set_geom_font()`, `set_geom_font()` and `set_geom_reference_line()` can be used to customise "text", "label",  "abline, "vline" and "hline" geom defaults.
#'
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour For most geoms, a default hex code for the colour of geoms (i.e. geoms other than "text", "label", "hline", "vline" and "abline"). Note, the "fill" inherits from this argument.
#' @param col_palette_d For a discrete scale, a character vector of hex codes.
#' @param col_palette_c For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param theme A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]).
#' @param theme_orientation The orientation of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param theme_axis_line_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `theme_orientation` of the plot.
#' @param theme_axis_ticks_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `theme_orientation` of the plot.
#' @param theme_panel_grid_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `theme_orientation` of the plot.
#' @param label_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
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
#'   theme = dark_mode_r(),
#'   colour = "#E7298AFF",
#'   col_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
#'                     "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF"),
#' )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'   )
#'
#' penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = species,
#'   )
#'
set_blanket <- function(
    ...,
    colour = "#357BA2FF",
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_d = "#CDC5BFFF",
    col_palette_na_c = "#988F88FF",
    col_palette_na_o = "#988F88FF",
    theme = light_mode_r(),
    theme_orientation = NULL,
    theme_axis_line_rm = TRUE,
    theme_axis_ticks_rm = TRUE,
    theme_panel_grid_rm = TRUE,
    label_case = snakecase::to_sentence_case) {

  weave_geom_defaults(colour = colour)

  weave_col_palette(
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
    col_palette_o = col_palette_o,
    col_palette_na_d = col_palette_na_d,
    col_palette_na_c = col_palette_na_c,
    col_palette_na_o = col_palette_na_o
  )

  weave_theme(theme = theme,
              theme_orientation = theme_orientation,
              theme_axis_line_rm = theme_axis_line_rm,
              theme_axis_ticks_rm = theme_axis_ticks_rm,
              theme_panel_grid_rm = theme_panel_grid_rm)

  weave_label_case(label_case = label_case)
}

#' Set the text and label geom defaults
#'
#' @description Update the "text" and "label" geom defaults. Note all other text is controlled by the theme.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour A hex code.
#' @param fill A hex code.
#' @param size A size.
#' @param family A family.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket(theme = dark_mode_r())
#' set_geom_font(colour = darkness[1])
#' set_geom_reference_line(colour = darkness[1])
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     x_breaks_n = 4,
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.25), y = I(0.75), label = "Here")
#'
set_geom_font <- function(
    ...,
    colour = "#121B24FF",
    fill = "#FFFFFFFF",
    size = 11 / 2.835052,
    family = "") {

  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!colour, size = !!size, family = !!family))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!colour, fill = !!fill, size = !!size, family = !!family))
}

#' Set the geom reference line defaults
#'
#' @description Update the "hline", "vline", "abline", and "curve" geom defaults.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour A hex code.
#' @param linewidth A linewidth.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket(theme = dark_mode_r())
#' set_geom_font(colour = darkness[1])
#' set_geom_reference_line(colour = darkness[1])
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     x_breaks_n = 4,
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.25), y = I(0.75), label = "Here")
#'
set_geom_reference_line <- function(
    ...,
    colour = "#121B24FF",
    linewidth = 0.25) {
  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
}


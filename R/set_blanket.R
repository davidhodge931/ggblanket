#' Set the style
#'
#' @description
#' This function sets the default:
#'
#' 1. theme (`ggplot2::set_theme`)
#'
#' 2. geom colour/fill (`update_geom_col`)
#' 3. geom linetype (`update_geom_linetype`).
#' 4. geom linewidth (`update_geom_linewidth`)
#' 5. geom size (`update_geom_size`)
#'
#' 6. geom font (`update_geom_font`)
#' 7. geom reference line (`update_geom_reference_line`)
#'
#' 8. geom colour/fill palettes (`update_geom_palettes`)
#'
#' 9. the function to apply to unspecified/unlabelled titles in `gg_*` functions (`weave_titles_case`)
#'
#' 10. the perspective behaviour in `gg_*` functions in `gg_*` functions (`weave_perspective`)
#'
#' For simplicity, this function does not provide all arguments.
#'
#' For further control, use `update_*` and `weave_*` functions.
#'
#' Note the theme should be set before any `update_*` functions.
#'
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param col A default hex code for the colour and fill of most geoms. Defaults to "#357BA2FF" (i.e. `blue`).
#' @param size A default size for the point geom. Defaults to 1.5. The pointrange size defaults to dividing by 6 (i.e. 0.25).
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param col_palette For a discrete colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param col_palette_d For a discrete colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param col_palette_c For a continuous (or ordinal) colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param titles_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `perspective` of the plot.
#'
#' @return A globally set style.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#'
#' set_blanket(
#'   theme = theme_darker(),
#'   col = "#E7298AFF",
#'   col_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
#'                     "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF"),
#' )
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'   )
#'
#' palmerpenguins::penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = species,
#'   )
#'
set_blanket <- function(
    theme = theme_lighter(),
    ...,
    col = "#357BA2FF",
    linewidth = 0.66,
    size = 1.5,
    col_palette = NULL,
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    titles_case = snakecase::to_sentence_case,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE
) {

  ggplot2::set_theme(theme)

  update_geom_col(col)

  update_geom_linetype()

  update_geom_linewidth(linewidth)

  update_geom_size(size)

  update_geom_font()

  update_geom_reference_line()

  update_geom_palettes(
    col_palette = col_palette,
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
  )

  weave_titles_case(titles_case = titles_case)

  weave_perspective(
    axis_line_transparent = axis_line_transparent,
    axis_ticks_transparent = axis_ticks_transparent,
    panel_grid_transparent = panel_grid_transparent,
  )
}

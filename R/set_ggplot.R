#' @description Set a default style for ggplot2.
#'
#' @param theme A default `theme`. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param geom_colour A default hex colour (and fill) for geoms. Fill inherits from this colour. Defaults to `blue`.
#' @param geom_linewidth A default linewidth for geoms. Fill inherits from this colour. Defaults to 0.66.
#' @param geom_size A default point size for `*_point`. `*_pointrange` multiplies this by 0.25. Defaults to 1.5. .
#' @param annotate_colour A default hex colour (and fill) for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param annotate_linewidth A default linewidth for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to 0.33 (i.e. `linewidthness[1]`).
#' @param annotate_size A default size for `*_text` and `*_label`. Defaults to 3.88.
#' @param annotate_family A default family for `*_text` and `*_label`. Defaults to ""
#' @param col_palette_discrete A default col_palette to use in the discrete scale. A character vector of hex codes (or names).
#' @param col_palette_continuous A default col_palette to use in the continuous scale. A character vector of hex codes (or names).
#' @param ... Provided to support trailing commas only.
#'
#' @noRd
set_ggplot <- function(
    theme = NULL,
    geom_colour = "#357ba2",
    geom_linewidth = 0.66,
    geom_size = 1.5,
    annotate_colour = "#121b24",
    annotate_linewidth = 0.33,
    annotate_size = 3.88,
    annotate_family = "",
    col_palette_discrete = c(teal, orange, navy, pink),
    col_palette_continuous = viridisLite::mako(n = 20, direction = -1),
    ...
) {

  if (rlang::is_null(theme)) theme <- light_mode_r() +
      ggplot2::theme(
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()
      )

  ggplot2::theme_set(new = theme)

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

  options(
    ggplot2.discrete.colour = function()
      scale_colour_manual(
        values = col_palette_discrete,
        na.value = "darkgrey"
      ),
    ggplot2.discrete.fill = function()
      scale_fill_manual(
        values = col_palette_discrete,
        na.value = "darkgrey"
      ),
    ggplot2.continuous.colour = function()
      scale_color_gradientn(
        colours = col_palette_continuous,
        na.value = "darkgrey"
      ),
    ggplot2.continuous.fill = function()
      scale_fill_gradientn(
        colours = col_palette_continuous,
        na.value = "darkgrey"
      )
  )
}

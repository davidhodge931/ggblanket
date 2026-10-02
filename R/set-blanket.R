#' Set ggblanket defaults
#'
#' Use `set_blanket` to:
#' * Set a global theme via [ggplot2::set_theme()]
#' * Set a global option for how themes are to be refined based on plot scale
#'   types via `refine`
#' * Update the global theme `fill`, `colour`, `linewidth`, `shape`,
#'   `linetype`, `size`, `stroke`
#' * Update the global theme `fill_palette`, `colour_palette`, `shape_palette`
#'   and `linetype_palette`
#' * Set a global option for `colour_blend`, which is a function to transform
#'   the `colour` and `colour_palette` with input of the `fill` and
#'   `fill_palette` respectively
#' * Set a global option for `fill_blend`, which is a function to transform
#'   the `fill` and `fill_palette` with input of the `colour` and
#'   `colour_palette` respectively.
#' * Set a global option `coord_clip`.
#'
#' @param ... Not used. Forces named arguments.
#' @param theme A ggplot2 theme.
#' @param refine A function with arguments discrete and orientation to refine the theme based on these. Defaults to that globally set.
#' @param fill Default fill colour. Defaults to `"#357BA2FF"`.
#' @param fill_palette Palette for fill scales. A single discrete palette or
#'   `list(discrete, continuous)`. Defaults to
#'   `list(jumble::jumble, viridis::turbo(n = 256))`.
#' @param fill_blend When `polygon = TRUE`, a function applied to `fill` and
#'   `fill_palette` to derive the fill. Defaults to `\(x) x`.
#' @param colour Default colour. Defaults to `fill`.
#' @param colour_palette Palette for colour scales. Same format as
#'   `fill_palette`. Defaults to `fill_palette`.
#' @param colour_blend When `polygon = TRUE`, a function applied to `fill` and
#'   `fill_palette` to derive the colour. If `fill_blend` is `NULL`, defaults
#'   to [blends::multiply()] for light panels and [blends::screen()] for dark
#'   panels. Otherwise defaults to `\(x) x`.
#' @param linewidth Default linewidth. Defaults to `0.66`.
#' @param borderwidth When `polygon = TRUE`, the default linewidth.
#'   Defaults to `0.33`.
#' @param shape Default point shape. Defaults to `21`.
#' @param shape_palette Palette for shape scales. Defaults to
#'   `scales::pal_manual(c(21, 24, 22, 23, 25))`.
#' @param linetype Default linetype. Defaults to `1`.
#' @param linetype_palette Palette for linetype scales. Defaults to
#'   `scales::pal_manual(1:6)`.
#' @param size Default point size. Defaults to `1.5`.
#' @param stroke Default stroke for point geoms. Defaults to `0.33`.
#' @param coord_clip Whether drawing is clipped to the panel. Either `"on"` or `"off"`.
#'
#' @return Called for side effects.
#' @export
#'
#' @examples
#' set_blanket(
#'   fill_palette = scales::pal_hue(),
#' )
#'
#' palmerpenguins::penguins |>
#'   gg_density(
#'     x = flipper_length_mm,
#'     fill = species,
#'   )
#'
#' set_blanket(
#'   fill_palette = scales::pal_hue(),
#'   fill_blend = \(x) scales::alpha(x, 0.75),
#' )
#'
#' palmerpenguins::penguins |>
#'   gg_density(
#'     x = flipper_length_mm,
#'     fill = species,
#'   )
#'
#' @seealso [scales::number_options()]
#'
set_blanket <- function(
  ...,
  theme = theme_lights(),
  refine = \(discrete, orientation) refine_axis_grid(discrete, orientation),
  colour_blend = NULL,
  fill_blend = NULL,
  borderwidth = 0.33,

  fill = "steelblue",
  fill_palette = list(jumble::jumble, viridis::turbo(n = 256)),
  colour = fill,
  colour_palette = fill_palette,
  linewidth = 0.66,
  shape = 21,
  shape_palette = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype = 1,
  linetype_palette = scales::pal_manual(1:6),
  size = 1.5,
  stroke = 0.33,
  coord_clip = "on"
) {
  rlang::check_dots_empty()

  if (!is.null(fill_blend) && !is.null(colour_blend)) {
    rlang::abort(
      "Only one of `fill_blend` or `colour_blend` can be set - not both."
    )
  }

  options("ggblanket.initialised" = TRUE)

  # Set base theme
  ggplot2::set_theme(new = theme)

  # Resolve fill_palette into discrete and continuous components
  resolve_palette <- function(palette) {
    if (is.list(palette) && length(palette) == 2) {
      list(discrete = palette[[1]], continuous = palette[[2]])
    } else {
      list(discrete = palette, continuous = NULL)
    }
  }

  fill_palettes <- resolve_palette(fill_palette)
  colour_palettes <- resolve_palette(colour_palette)

  # Update geom and palette theme elements
  ggplot2::update_theme(
    geom = ggplot2::element_geom(
      fill = fill,
      colour = colour,
      pointshape = shape,
      linewidth = linewidth,
      borderwidth = borderwidth,
      linetype = linetype,
      polygontype = linetype,
      pointsize = size
    ),
    # Border geoms — have both fill and colour
    geom.area = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.col = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.density = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.dotplot = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.map = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.point = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.pointrange = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.smooth = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.sf = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.tile = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    # Line geoms — colour only
    geom.abline = ggplot2::element_geom(linewidth = linewidth),
    geom.contour = ggplot2::element_geom(linewidth = linewidth),
    geom.density_2d = ggplot2::element_geom(linewidth = linewidth),
    geom.errorbar = ggplot2::element_geom(linewidth = linewidth),
    geom.hline = ggplot2::element_geom(linewidth = linewidth),
    geom.line = ggplot2::element_geom(linewidth = linewidth),
    geom.linerange = ggplot2::element_geom(linewidth = linewidth),
    geom.path = ggplot2::element_geom(linewidth = linewidth),
    geom.quantile = ggplot2::element_geom(linewidth = linewidth),
    geom.rug = ggplot2::element_geom(linewidth = linewidth),
    geom.segment = ggplot2::element_geom(linewidth = linewidth),
    geom.spoke = ggplot2::element_geom(linewidth = linewidth),
    geom.step = ggplot2::element_geom(linewidth = linewidth),
    geom.vline = ggplot2::element_geom(linewidth = linewidth),
    geom.curve = ggplot2::element_geom(linewidth = linewidth),
    palette.fill.discrete = fill_palettes$discrete,
    palette.fill.continuous = fill_palettes$continuous,
    palette.colour.discrete = colour_palettes$discrete,
    palette.colour.continuous = colour_palettes$continuous,
    palette.shape.discrete = shape_palette,
    palette.linetype.discrete = linetype_palette
  )

  # Set refine function
  set_refine(refine = refine)

  # Set polygon functions as options — only one of fill_blend or colour_blend
  if (is.null(fill_blend) && is.null(colour_blend)) {
    colour_blend <- \(x) {
      if (is_panel_dark()) blends::screen(x) else blends::multiply(x)
    }
    fill_blend <- \(x) x
  } else if (is.null(fill_blend)) {
    fill_blend <- \(x) x
  } else if (is.null(colour_blend)) {
    colour_blend <- \(x) x
  }

  set_colour_blend(colour_blend = colour_blend)
  set_fill_blend(fill_blend = fill_blend)
  set_borderwidth(borderwidth = borderwidth)
  set_stroke(stroke = stroke)

  set_coord_clip(coord_clip = coord_clip)
}

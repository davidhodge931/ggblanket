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
#' * Set a global option for `colour_border`, which is a function to transform
#'   the `colour` and `colour_palette` with input of the `fill` and
#'   `fill_palette` respectively
#' * Set a global option for `fill_border`, which is a function to transform
#'   the `fill` and `fill_palette` with input of the `colour` and
#'   `colour_palette` respectively.
#' * Set a global option `coord_clip`.
#'
#' @param ... Not used. Forces named arguments.
#' @param theme A ggplot2 theme. Defaults to [ggrefine::theme_grey()].
#' @param refine A refine function. Defaults to [ggrefine::modern].
#' @param fill Default fill colour. Defaults to `"#357BA2FF"`.
#' @param fill_palette Palette for fill scales. A single discrete palette or
#'   `list(discrete, continuous)`. Defaults to
#'   `list(jumble::jumble, viridis::turbo(n = 256))`.
#' @param fill_border When `border = TRUE`, a function applied to `fill` and
#'   `fill_palette` to derive the fill. Defaults to `\(x) x`.
#' @param colour Default colour. Defaults to `fill`.
#' @param colour_palette Palette for colour scales. Same format as
#'   `fill_palette`. Defaults to `fill_palette`.
#' @param colour_border When `border = TRUE`, a function applied to `fill` and
#'   `fill_palette` to derive the colour. If `fill_border` is `NULL`, defaults
#'   to [blends::multiply()] for light panels and [blends::screen()] for dark
#'   panels. Otherwise defaults to `\(x) x`.
#' @param linewidth Default linewidth. Defaults to `0.66`.
#' @param linewidth_border When `border = TRUE`, the default linewidth.
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
#'   fill_border = \(x) scales::alpha(x, 0.75),
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
    theme            = ggrefine::theme_grey(),
    refine           = ggrefine::modern,
    fill             = "#357BA2FF",
    fill_palette     = list(jumble::jumble, viridis::turbo(n = 256)),
    fill_border      = NULL,
    colour           = fill,
    colour_palette   = fill_palette,
    colour_border    = NULL,
    linewidth        = 0.66,
    linewidth_border = 0.33,
    shape            = 21,
    shape_palette    = scales::pal_manual(c(21, 24, 22, 23, 25)),
    linetype         = 1,
    linetype_palette = scales::pal_manual(1:6),
    size             = 1.5,
    stroke           = 0.33,
    coord_clip       = "on"
) {
  rlang::check_dots_empty()

  if (!is.null(fill_border) && !is.null(colour_border)) {
    rlang::abort("Only one of `fill_border` or `colour_border` can be set - not both.")
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

  fill_palettes   <- resolve_palette(fill_palette)
  colour_palettes <- resolve_palette(colour_palette)

  # Update geom and palette theme elements
  ggplot2::update_theme(
    geom = ggplot2::element_geom(
      fill        = fill,
      colour      = colour,
      pointshape  = shape,
      linewidth   = linewidth,
      borderwidth = linewidth_border,
      linetype    = linetype,
      bordertype  = linetype,
      pointsize   = size
    ),
    # Border geoms — have both fill and colour
    geom.area       = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.bar        = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.boxplot    = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.col        = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.crossbar   = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.density    = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.dotplot    = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.hex        = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.map        = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.point      = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.pointrange = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.polygon    = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.rect       = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.ribbon     = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.smooth     = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.sf         = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.tile       = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    geom.violin     = ggplot2::element_geom(linewidth = linewidth_border, borderwidth = linewidth_border),
    # Line geoms — colour only
    geom.abline     = ggplot2::element_geom(linewidth = linewidth),
    geom.contour    = ggplot2::element_geom(linewidth = linewidth),
    geom.density_2d = ggplot2::element_geom(linewidth = linewidth),
    geom.errorbar   = ggplot2::element_geom(linewidth = linewidth),
    geom.hline      = ggplot2::element_geom(linewidth = linewidth),
    geom.line       = ggplot2::element_geom(linewidth = linewidth),
    geom.linerange  = ggplot2::element_geom(linewidth = linewidth),
    geom.path       = ggplot2::element_geom(linewidth = linewidth),
    geom.quantile   = ggplot2::element_geom(linewidth = linewidth),
    geom.rug        = ggplot2::element_geom(linewidth = linewidth),
    geom.segment    = ggplot2::element_geom(linewidth = linewidth),
    geom.spoke      = ggplot2::element_geom(linewidth = linewidth),
    geom.step       = ggplot2::element_geom(linewidth = linewidth),
    geom.vline      = ggplot2::element_geom(linewidth = linewidth),
    geom.curve      = ggplot2::element_geom(linewidth = linewidth),
    palette.fill.discrete     = fill_palettes$discrete,
    palette.fill.continuous   = fill_palettes$continuous,
    palette.colour.discrete   = colour_palettes$discrete,
    palette.colour.continuous = colour_palettes$continuous,
    palette.shape.discrete    = shape_palette,
    palette.linetype.discrete = linetype_palette
  )

  # Set refine function
  set_refine(refine = refine)

  # Set border functions as options — only one of fill_border or colour_border
  if (is.null(fill_border) && is.null(colour_border)) {
    colour_border <- \(x) {
      if (is_panel_dark()) blends::screen(x) else blends::multiply(x)
    }
    fill_border <- \(x) x
  }
  else if (is.null(fill_border)) {
    fill_border <- \(x) x
  }
  else if (is.null(colour_border)) {
    colour_border <- \(x) x
  }

  set_colour_border(colour_border = colour_border)
  set_fill_border(fill_border = fill_border)
  set_linewidth_border(linewidth_border = linewidth_border)
  set_stroke(stroke = stroke)

  set_coord_clip(coord_clip = coord_clip)
}

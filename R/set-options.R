#' Set the ggblanket refine function
#'
#' `set_refine()` sets a global function that refines the theme depending on
#' the plot's scale types. All other defaults (fill, colour, linewidth,
#' palettes, etc.) are controlled through the theme, e.g. with
#' [ggplot2::set_theme()], [ggplot2::theme()] and [ggplot2::element_geom()].
#'
#' @param new A function with arguments `discrete` and `orientation` that
#'   returns a theme refinement. `NULL` (default) resets to
#'   `\(discrete, orientation) refine_axis_grid(discrete, orientation)`.
#'
#' @return Invisibly, the previous value of the option.
#' @export
#'
#' @examples
#' set_refine(\(discrete, orientation) refine_axis_grid(discrete, orientation))
set_refine <- function(new = NULL) {
  if (!is.null(new) && !is.function(new)) {
    rlang::abort("`new` must be a function or `NULL`.")
  }
  old <- options("ggblanket.refine" = new)
  invisible(old[[1]])
}

#' Set the ggblanket colour blend function
#'
#' When `polygon = TRUE`, `colour_blend` is a function applied to the `fill`
#' and `fill_palette` to derive the colour. Only one of the colour blend and
#' fill blend can be active: setting a non-`NULL` colour blend clears any fill
#' blend (last call wins), and the fill blend then defaults to identity.
#'
#' @param new A function of one argument (a colour vector), e.g.
#'   [blends::multiply()]. `NULL` (default) resets to the default: if no fill
#'   blend is set, [blends::multiply()] for light panels and
#'   [blends::screen()] for dark panels; otherwise identity.
#'
#' @return Invisibly, the previous value of the option.
#' @export
#'
#' @examples
#' set_colour_blend(\(x) blends::multiply(x))
set_colour_blend <- function(new = NULL) {
  if (!is.null(new) && !is.function(new)) {
    rlang::abort("`new` must be a function or `NULL`.")
  }
  if (!is.null(new)) {
    options("ggblanket.fill_blend" = NULL)
  }
  old <- options("ggblanket.colour_blend" = new)
  invisible(old[[1]])
}

#' Set the ggblanket fill blend function
#'
#' When `polygon = TRUE`, `fill_blend` is a function applied to the `colour`
#' and `colour_palette` to derive the fill. Only one of the colour blend and
#' fill blend can be active: setting a non-`NULL` fill blend clears any colour
#' blend (last call wins), and the colour blend then defaults to identity.
#'
#' @param new A function of one argument (a colour vector), e.g.
#'   `\(x) scales::alpha(x, 0.75)`. `NULL` (default) resets to the default of
#'   identity.
#'
#' @return Invisibly, the previous value of the option.
#' @export
#'
#' @examples
#' set_fill_blend(\(x) scales::alpha(x, 0.75))
set_fill_blend <- function(new = NULL) {
  if (!is.null(new) && !is.function(new)) {
    rlang::abort("`new` must be a function or `NULL`.")
  }
  if (!is.null(new)) {
    options("ggblanket.colour_blend" = NULL)
  }
  old <- options("ggblanket.fill_blend" = new)
  invisible(old[[1]])
}

# Getters (internal) -------------------------------------------------------
# Defaults are resolved here, so no initialisation step is needed.

get_refine <- function() {
  getOption("ggblanket.refine") %||%
    \(discrete, orientation) {
      refine_axis_grid(
        discrete = discrete,
        orientation = orientation,
        axis_mode = "modern",
        grid_mode = "flow"
      )
    }
}

get_colour_blend <- function() {
  colour_blend <- getOption("ggblanket.colour_blend")
  if (!is.null(colour_blend)) {
    return(colour_blend)
  }

  # A fill blend has been set, so the colour is left unblended
  if (!is.null(getOption("ggblanket.fill_blend"))) {
    return(\(x) x)
  }

  \(x) {
    if (is.null(x)) {
      return(x)
    }
    if (is_panel_dark()) blends::screen(x) else blends::multiply(x)
  }
}

get_fill_blend <- function() {
  getOption("ggblanket.fill_blend") %||% \(x) x
}

# Theme-derived values (internal) -----------------------------------------

get_borderwidth <- function(theme = ggplot2::get_theme()) {
  ggplot2::calc_element("geom", theme)@borderwidth %||% 0.33
}

# Fallback for the coord clip when the user hasn't supplied one
get_clip <- function(coord_clip = NULL) {
  coord_clip %||% "on"
}

get_theme <- function(theme = NULL) {
  if (!is.null(theme)) return(theme)

  current <- ggplot2::get_theme()

  if (identical(current, ggplot2::theme_grey())) theme_lights() else current
}

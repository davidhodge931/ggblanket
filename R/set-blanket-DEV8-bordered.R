set_blanket <- function(
  ...,
  theme = ggrefine::theme_white(),
  refine = refine_modern,
  bordered_colour = NULL
) {

  rlang::check_dots_empty()

  #theme
  ggplot2::set_theme(new = theme)

  #refine
  set_refine(refine = refine)

  #bordered_colour
  if (rlang::is_null(bordered_colour)) {
    bordered_colour <- \(x) if (is_panel_dark()) paletteblend::screen(x) else paletteblend::multiply(x)
  }

  set_bordered_colour(bordered_colour = bordered_colour)
}

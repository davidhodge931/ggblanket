#' Setup the ggblanket style
#'
#' @description A set-up function, which sets the mode and updates ggplot2 geom defaults.
#'
#' @param mode A `*_mode_*` theme set globally for when mode = NULL. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param geom_defaults A series of ggplot2::update_geom_defaults calls. Defaults to `weave_geom_defaults()`.
#' @param ... Provided only to support trailing commas.
#'
#' @return A globally set mode and updated geom defaults.
#' @export
set_blanket <- function(
    mode = light_mode_r(),
    geom_defaults = weave_geom_defaults(),
    ...
) {

  set_mode(mode)

  geom_defaults
}


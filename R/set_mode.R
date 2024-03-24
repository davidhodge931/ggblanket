#' Get the mode
#'
#' @description Get the current globally set theme that is added to the `mode` argument where NULL in the the `gg_*` functions.
#' Note [ggplot2::theme_get()] sets globally a new theme that is `+`-ed on as a layer to the `gg_*` functions.
#'
#' @noRd
get_mode <- function() {
  if (!identical(theme_env$theme_current, ggplot2::theme_get())) {
    theme_env$mode_current <- ggplot2::theme_get()
    theme_env$theme_current <- ggplot2::theme_get()
    theme <- theme_env$theme_current
  } else {
    theme <- theme_env$mode_current
  }
  theme
}

#' Set the mode
#'
#' @description Set a theme for when the `mode` argument in a `gg_*` function in NULL.
#' Use `set_mode(light_mode_r())` to unset a set mode.
#' Note [ggplot2::theme_set()] sets globally a new theme that is added to the `gg_*` function output with no side-effects.
#' Use `ggplot2::theme_set(theme_grey())` to unset the set theme.
#'
#' @export
#'
#' @param new A new theme to add to the mode argument where NULL (e.g. [dark_mode_r()].
#'
#' @noRd
set_mode <- function(new = light_mode_r()) {
  mode_old <- theme_env$mode_current
  theme_env$mode_current <- new
  theme_env$theme_current <- ggplot2::theme_get()
  invisible(mode_old)
}

# internal ----------------------------------------------------------------

theme_env <- new.env(parent = emptyenv())
theme_env$mode_current <- light_mode_r()
theme_env$theme_current <- ggplot2::theme_grey()

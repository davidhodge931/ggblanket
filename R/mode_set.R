#' Get the mode
#'
#' @description Get the current globally set mode that is added to the `mode` argument where NULL in the the `gg_*` functions.
#' Note [ggplot2::theme_get()] sets globally a new theme that is `+`-ed on as a layer to the `gg_*` functions.
#'
#' @return The current set mode
#' @export
#'
mode_get <- function() {
  if (!identical(.mode_env$gg_current, ggplot2::theme_get())) {
    .mode_env$current <- ggplot2::theme_get()
    .mode_env$gg_current <- ggplot2::theme_get()
    thm <- .mode_env$gg_current
  } else {
    thm <- .mode_env$current
  }
  thm
}

#' Set the mode
#'
#' @description Set a new theme globally to be added to the `mode` argument where NULL in the `gg_*` functions. Note [ggplot2::theme_set()] sets globally a new theme that is `+`-ed on as a layer to the `gg_*` functions.
#'
#' @export
#'
#' @param new The new `*_mode_*` theme to add to the mode argument where NULL.
#'
#' @return The current set mode
#' @export

#' @rdname mode_get
#' @export
mode_set <- function(new = grey_mode_rt()) {
  old <- .mode_env$current
  .mode_env$current <- new
  .mode_env$gg_current <- ggplot2::theme_get()
  invisible(old)
}

# internal ----------------------------------------------------------------

.mode_env <- new.env(parent = emptyenv())
.mode_env$current <- grey_mode_rt()
.mode_env$gg_current <- ggplot2::theme_grey()

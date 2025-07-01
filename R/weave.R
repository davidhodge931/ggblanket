# global defaults
ggblanket_global <- new.env(parent = emptyenv())

ggblanket_global$theme <- NULL
ggblanket_global$perspective <- NULL
ggblanket_global$axis_line_transparent <- NULL
ggblanket_global$axis_ticks_transparent <- NULL
ggblanket_global$axis_line_transparent <- NULL

ggblanket_global$titles_case <- NULL

ggblanket_global$col_palette_d <- NULL
ggblanket_global$col_palette_c <- NULL
ggblanket_global$col_palette_o <- NULL
ggblanket_global$col_palette_na_d <- NULL
ggblanket_global$col_palette_na_c <- NULL
ggblanket_global$col_palette_na_o <- NULL

#' Set a theme
#'
#' @description Set a theme for the theme argument in `gg_*` functions.
#'
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param perspective The perspective of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `perspective` of the plot.
#'
#' @noRd
weave_theme <- function(
    theme = theme_lighter(),
    ...,
    perspective = NULL,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE
) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- theme
  invisible(old)

  old <- ggblanket_global$perspective
  ggblanket_global$perspective <- perspective
  invisible(old)

  old <- ggblanket_global$axis_line_transparent
  ggblanket_global$axis_line_transparent <- axis_line_transparent
  invisible(old)

  old <- ggblanket_global$axis_ticks_transparent
  ggblanket_global$axis_ticks_transparent <- axis_ticks_transparent
  invisible(old)

  old <- ggblanket_global$panel_grid_transparent
  ggblanket_global$panel_grid_transparent <- panel_grid_transparent
  invisible(old)

  if (ggplot2::is_theme(theme)) {
    ggplot2::set_theme(new = theme)
  } else {
    ggplot2::set_theme(new = theme[[1]]) #if list, assume first element is theme
  }
}

#' Set a label case function
#'
#' @description Set a function to format the label of unlabelled variables.
#'
#' @param titles_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_titles_case <- function(titles_case = snakecase::to_sentence_case, ...) {
  old <- ggblanket_global$titles_case
  ggblanket_global$titles_case <- titles_case
  invisible(old)
}

#' Set geom palettes
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param col_palette_d For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default. Use NULL to leave as is.
#' @param col_palette_c For a continuous scale, a character vector of hex codes. Use NULL for ggplot2 default. Use NULL to leave as is.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default. Use NULL to leave as is.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#'
#' @noRd
weave_geom_palettes <- function(
  ...,
  col_palette_d = jumble,
  col_palette_c = viridisLite::mako(n = 9, direction = -1),
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_d = "#CDC5BFFF",
  col_palette_na_c = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
  col_palette_na_o = "#988F88FF"
) {
  if (!rlang::is_null(col_palette_d)) {
    weave_col_palette_d(
      col_palette_d = col_palette_d,
      col_palette_na_d = col_palette_na_d
    )
  }
  if (!rlang::is_null(col_palette_c)) {
    weave_col_palette_c(
      col_palette_c = col_palette_c,
      col_palette_na_c = col_palette_na_c
    )
  }
  if (!rlang::is_null(col_palette_o)) {
    weave_col_palette_o(
      col_palette_o = col_palette_o,
      col_palette_na_o = col_palette_na_o
    )
  }
}

#' Set a discrete geom_colour and geom_fill palette
#'
#' @param col_palette_d For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_col_palette_d <- function(
  col_palette_d = jumble,
  col_palette_na_d = "#CDC5BFFF",
  ...
) {
  if (rlang::is_null(col_palette_na_d)) {
    col_palette_na_d <- "grey50"
  }

  if (!rlang::is_null(col_palette_d)) {
    if (!rlang::is_function(col_palette_d)) {
      col_palette_d <- c(col_palette_d, rep(col_palette_na_d, times = 100))
    }
    old <- ggblanket_global$col_palette_d
    ggblanket_global$col_palette_d <- col_palette_d
    invisible(old)

    old <- ggblanket_global$col_palette_na_d
    ggblanket_global$col_palette_na_d <- col_palette_na_d
    invisible(old)

    options(
      ggplot2.discrete.colour = function() {
        ggplot2::scale_colour_manual(
          values = col_palette_d,
          na.value = col_palette_na_d
        )
      },
      ggplot2.discrete.fill = function() {
        ggplot2::scale_fill_manual(
          values = col_palette_d,
          na.value = col_palette_na_d
        )
      }
    )
  }
}

#' Set a continuous geom_colour and geom_fill palette
#'
#' @param col_palette_c For a continuous scale, A character vector of hex codes (or names) or a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_col_palette_c <- function(
  col_palette_c = viridisLite::mako(n = 9, direction = -1),
  col_palette_na_c = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
  ...
) {
  if (rlang::is_null(col_palette_c)) {
    col_palette_c <- scales::pal_seq_gradient(
      low = "#132B43",
      high = "#56B1F7"
    )(seq(0, 1, length.out = 20))
  }

  if (rlang::is_null(col_palette_na_c)) {
    col_palette_na_c <- "grey50"
  }

  col_palette_c <- if (is.function(col_palette_c)) {
    col_palette_c(256)
  } else {
    col_palette_c
  }

  old <- ggblanket_global$col_palette_c
  ggblanket_global$col_palette_c <- col_palette_c
  invisible(old)

  old <- ggblanket_global$col_palette_na_c
  ggblanket_global$col_palette_na_c <- col_palette_na_c
  invisible(old)

  options(
    ggplot2.continuous.colour = function() {
      ggplot2::scale_colour_gradientn(
        colours = col_palette_c,
        na.value = col_palette_na_c
      )
    },
    ggplot2.continuous.fill = function() {
      ggplot2::scale_fill_gradientn(
        colours = col_palette_c,
        na.value = col_palette_na_c
      )
    }
  )
}

#' Set an ordinal geom_colour and geom_fill palette
#'
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @noRd
weave_col_palette_o <- function(
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_o = "#988F88FF",
  ...
) {
  if (rlang::is_null(col_palette_o)) {
    col_palette_o <- scales::pal_viridis()
  }
  if (rlang::is_null(col_palette_na_o)) {
    col_palette_na_o <- "grey50"
  }

  old <- ggblanket_global$col_palette_o
  ggblanket_global$col_palette_o <- col_palette_o
  invisible(old)

  old <- ggblanket_global$col_palette_na_o
  ggblanket_global$col_palette_na_o <- col_palette_na_o
  invisible(old)
}

#' #' Annotated panel grid lines
#' #'
#' #' @description Replace panel grid lines with annotated segments.
#' #' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#' #'
#' #' @param ... Provided to require argument naming, support trailing commas etc.
#' #' @param x_breaks A vector of x-axis breaks for vertical grid lines. If NULL, no vertical grid lines are drawn.
#' #' @param y_breaks A vector of y-axis breaks for horizontal grid lines. If NULL, no horizontal grid lines are drawn.
#' #' @param colour The colour of grid lines. Inherits from current theme panel.grid.major etc.
#' #' @param linewidth The linewidth of grid lines. Inherits from current theme panel.grid.major etc.
#' #' @param linetype The linetype of grid lines. Inherits from current theme panel.grid.major etc.
#' #' @param theme_elements What to do with theme panel grid elements. Either "transparent", "keep" or "blank". Defaults "transparent".
#' #'
#' #' @return A list of annotate layers and theme elements.
#' #' @export
#' #'
#' #' @examples
#' #' library(ggplot2)
#' #' library(dplyr)
#' #' library(stringr)
#' #' library(palmerpenguins)
#' #'
#' #' set_blanket(
#' #'   theme = theme_lighter(
#' #'     panel_heights = rep(unit(50, "mm"), 100),
#' #'     panel_widths = rep(unit(75, "mm"), 100),
#' #'   ),
#' #' )
#' #'
#' #' penguins |>
#' #'   tidyr::drop_na(sex) |>
#' #'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#' #'   gg_blanket(
#' #'     x = flipper_length_mm,
#' #'     y = body_mass_g,
#' #'     col = sex,
#' #'   ) +
#' #'   annotate_panel_grid(
#' #'     x_breaks = seq(190, 230, 15),
#' #'     y_breaks = seq(3000, 6500, 250),
#' #'   ) +
#' #'   geom_point()
#' #'
#' annotate_panel_grid <- function(
#'     ...,
#'     x_breaks = NULL,
#'     y_breaks = NULL,
#'     colour = NULL,
#'     linewidth = NULL,
#'     linetype = NULL,
#'     theme_elements = "transparent"
#' ) {
#'   rlang::inform(
#'     "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
#'   )
#'
#'   # Get current theme
#'   current_theme <- ggplot2::get_theme()
#'
#'   # Extract theme properties for grid lines
#'   if (rlang::is_null(colour)) {
#'     colour <- current_theme$panel.grid.major$colour %||%
#'       current_theme$panel.grid$colour %||%
#'       "#E6E9EFFF"
#'   }
#'
#'   if (rlang::is_null(linewidth)) {
#'     linewidth <- current_theme$panel.grid.major$linewidth %||%
#'       current_theme$panel.grid$linewidth %||%
#'       0.5
#'   }
#'
#'   if (rlang::is_null(linetype)) {
#'     linetype <- current_theme$panel.grid.major$linetype %||%
#'       current_theme$panel.grid$linetype %||%
#'       "solid"
#'   }
#'
#'   # Create list to store output
#'   stamp <- list()
#'
#'   # Add theme modifications if requested
#'   if (theme_elements == "transparent") {
#'     stamp <- c(
#'       stamp,
#'       list(
#'         ggplot2::theme(
#'           panel.grid.major = ggplot2::element_line(colour = "transparent"),
#'           panel.grid.minor = ggplot2::element_line(colour = "transparent")
#'         )
#'       )
#'     )
#'   } else if (theme_elements == "blank") {
#'     stamp <- c(
#'       stamp,
#'       list(
#'         ggplot2::theme(
#'           panel.grid.major = ggplot2::element_blank(),
#'           panel.grid.minor = ggplot2::element_blank()
#'         )
#'       )
#'     )
#'   }
#'
#'   # Add vertical grid lines (x_breaks)
#'   if (!is.null(x_breaks)) {
#'     stamp <- c(
#'       stamp,
#'       list(
#'         ggplot2::annotate(
#'           "segment",
#'           x = x_breaks,
#'           xend = x_breaks,
#'           y = I(-Inf),
#'           yend = I(Inf),
#'           colour = colour,
#'           linewidth = linewidth,
#'           linetype = linetype
#'         )
#'       )
#'     )
#'   }
#'
#'   # Add horizontal grid lines (y_breaks)
#'   if (!is.null(y_breaks)) {
#'     stamp <- c(
#'       stamp,
#'       list(
#'         ggplot2::annotate(
#'           "segment",
#'           x = I(-Inf),
#'           xend = I(Inf),
#'           y = y_breaks,
#'           yend = y_breaks,
#'           colour = colour,
#'           linewidth = linewidth,
#'           linetype = linetype
#'         )
#'       )
#'     )
#'   }
#'
#'   return(stamp)
#' }

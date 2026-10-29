#' Move Legend
#'
#' Helper shortcut to set legend position, location, and margins.
#'
#' @param position Character. Where to place the legend ("right", "top", or "bottom").
#' @param ... For trailing comma support.
#'
#' @return A ggplot2 theme object.
#' @noRd
refine_legend <- function(position = "right", ...) {

  if (position == "right") {
    return(
      ggplot2::theme(
        legend.position = "right",
        legend.location = "panel",
        legend.margin   = ggplot2::margin(l = 5.5, b = 11),
        legend.justification = "top",
        legend.title.position = "top",
        legend.byrow = FALSE
      )
    )
  }

  if (position == "top") {
    return(
      ggplot2::theme(
        legend.position = "top",
        legend.location = "plot",
        legend.margin   = ggplot2::margin(r = 5.5, b = 11),
        legend.justification = "left",
        legend.title.position = "top",
        legend.byrow = TRUE
      )
    )
  }

  if (position == "bottom") {
    return(
      ggplot2::theme(
        legend.position = "bottom",
        legend.location = "plot",
        legend.margin   = ggplot2::margin(r = 5.5, b = 11),
        legend.justification = "left",
        legend.title.position = "top",
        legend.byrow = TRUE
      )
    )
  }
}

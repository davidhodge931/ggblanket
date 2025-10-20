aspect_modern <- function(
    aspect = c("x", "y"),
    x_type = c("continuous", "binned", "discrete"),
    y_type = c("continuous", "binned", "discrete")
    ) {

  theme <- ggplot2::theme()

  if (aspect == "x") {
      theme <- theme +
        ggplot2::theme(
          axis.line.y.left = ggplot2::element_line(linetype = 0),
          axis.line.y.right = ggplot2::element_line(linetype = 0),
          axis.ticks.y.left = ggplot2::element_line(linetype = 0),
          axis.ticks.y.right = ggplot2::element_line(linetype = 0),
          axis.minor.ticks.y.left = ggplot2::element_line(linetype = 0),
          axis.minor.ticks.y.right = ggplot2::element_line(linetype = 0),
          panel.grid.major.x = ggplot2::element_line(linetype = 0),
          panel.grid.minor.x = ggplot2::element_line(linetype = 0)
        )
  }

  if (aspect == "y") {
    theme <- theme +
      ggplot2::theme(
        axis.line.x.bottom = ggplot2::element_line(linetype = 0),
        axis.line.x.top = ggplot2::element_line(linetype = 0),
        axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        axis.ticks.x.top = ggplot2::element_line(linetype = 0),
        axis.minor.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        axis.minor.ticks.x.top = ggplot2::element_line(linetype = 0),
        panel.grid.major.y = ggplot2::element_line(linetype = 0),
        panel.grid.minor.y = ggplot2::element_line(linetype = 0)
      )
  }

  if (x_type == "discrete") {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        axis.ticks.x.top = ggplot2::element_line(linetype = 0),
      )
  }
  if (y_type == "discrete") {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.y.left = ggplot2::element_line(linetype = 0),
        axis.ticks.y.right = ggplot2::element_line(linetype = 0),
      )
  }

  return(theme)
}

aspect_classic <- function(
    aspect = c("x", "y"),
    x_type = c("continuous", "binned", "discrete"),
    y_type = c("continuous", "binned", "discrete")
) {

  theme <- ggplot2::theme()

  if (aspect == "x") {
    theme <- theme +
      ggplot2::theme(
        # axis.line.y.left = ggplot2::element_line(linetype = 0),
        # axis.line.y.right = ggplot2::element_line(linetype = 0),
        # axis.ticks.y.left = ggplot2::element_line(linetype = 0),
        # axis.ticks.y.right = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.y.left = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.y.right = ggplot2::element_line(linetype = 0),
        panel.grid.major.x = ggplot2::element_line(linetype = 0),
        panel.grid.minor.x = ggplot2::element_line(linetype = 0)
      )
  }

  if (aspect == "y") {
    theme <- theme +
      ggplot2::theme(
        # axis.line.x.bottom = ggplot2::element_line(linetype = 0),
        # axis.line.x.top = ggplot2::element_line(linetype = 0),
        # axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        # axis.ticks.x.top = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.x.top = ggplot2::element_line(linetype = 0),
        panel.grid.major.y = ggplot2::element_line(linetype = 0),
        panel.grid.minor.y = ggplot2::element_line(linetype = 0)
      )
  }

  if (x_type == "discrete") {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        axis.ticks.x.top = ggplot2::element_line(linetype = 0),
      )
  }
  if (y_type == "discrete") {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.y.left = ggplot2::element_line(linetype = 0),
        axis.ticks.y.right = ggplot2::element_line(linetype = 0),
      )
  }

  return(theme)
}

aspect_traditional <- function(
    aspect = c("x", "y"),
    x_type = c("continuous", "binned", "discrete"),
    y_type = c("continuous", "binned", "discrete")
) {

  theme <- ggplot2::theme()

  if (aspect == "x") {
    theme <- theme +
      ggplot2::theme(
        # axis.line.y.left = ggplot2::element_line(linetype = 0),
        # axis.line.y.right = ggplot2::element_line(linetype = 0),
        # axis.ticks.y.left = ggplot2::element_line(linetype = 0),
        # axis.ticks.y.right = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.y.left = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.y.right = ggplot2::element_line(linetype = 0),
        # panel.grid.major.x = ggplot2::element_line(linetype = 0),
        # panel.grid.minor.x = ggplot2::element_line(linetype = 0)
      )
  }

  if (aspect == "y") {
    theme <- theme +
      ggplot2::theme(
        # axis.line.x.bottom = ggplot2::element_line(linetype = 0),
        # axis.line.x.top = ggplot2::element_line(linetype = 0),
        # axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        # axis.ticks.x.top = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        # axis.minor.ticks.x.top = ggplot2::element_line(linetype = 0),
        # panel.grid.major.y = ggplot2::element_line(linetype = 0),
        # panel.grid.minor.y = ggplot2::element_line(linetype = 0)
      )
  }

  if (x_type == "discrete") {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
        axis.ticks.x.top = ggplot2::element_line(linetype = 0),
      )
  }
  if (y_type == "discrete") {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.y.left = ggplot2::element_line(linetype = 0),
        axis.ticks.y.right = ggplot2::element_line(linetype = 0),
      )
  }

  return(theme)
}


#' Get the base of the plot
#'
#' @param data A data frame or tibble.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,group,subgroup,label,text,sample An unquoted aesthetic variable.
#'
#' @noRd
plot_base_internal <- function(
  data,
  x = NULL,
  xmin = NULL,
  xmax = NULL,
  xend = NULL,
  y = NULL,
  ymin = NULL,
  ymax = NULL,
  yend = NULL,
  z = NULL,
  col = NULL,
  facet = NULL,
  facet2 = NULL,
  group = NULL,
  subgroup = NULL,
  sample = NULL,
  label = NULL,
  text = NULL
) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  xmin <- rlang::enquo(xmin)
  xmax <- rlang::enquo(xmax)
  xend <- rlang::enquo(xend)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  yend <- rlang::enquo(yend)
  z <- rlang::enquo(z)
  group <- rlang::enquo(group)
  subgroup <- rlang::enquo(subgroup)
  sample <- rlang::enquo(sample)

  label <- rlang::enquo(label)
  text <- rlang::enquo(text)

  if (rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            y = !!y,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    } else if (!rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            y = !!y,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    }
  } else if (!rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = !!x,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    } else if (!rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = !!x,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    }
  } else if (!rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = !!x,
            y = !!y,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    } else if (!rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = !!x,
            y = !!y,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    }
  } else if (rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    } else if (!rlang::quo_is_null(col)) {
      plot <- data |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            text = !!text
          )
        )
    }
  }

  return(plot)
}

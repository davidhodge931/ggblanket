#' #' Title
#' #'
#' #' @param data
#' #' @param x
#' #' @param xmin
#' #' @param xmax
#' #' @param xend
#' #' @param y
#' #' @param ymin
#' #' @param ymax
#' #' @param yend
#' #' @param z
#' #' @param col
#' #' @param facet
#' #' @param facet2
#' #' @param group
#' #' @param subgroup
#' #' @param label
#' #' @param text
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' get_base < function(
#'     data,
#'     x = NULL,
#'     xmin = NULL,
#'     xmax = NULL,
#'     xend = NULL,
#'     y = NULL,
#'     ymin = NULL,
#'     ymax = NULL,
#'     yend = NULL,
#'     z = NULL,
#'     col = NULL,
#'     facet = NULL,
#'     facet2 = NULL,
#'     group = NULL,
#'     subgroup = NULL,
#'     label = NULL,
#'     text = NULL) {
#'
#'   x <- rlang::enquo(x)
#'   y <- rlang::enquo(y)
#'   col <- rlang::enquo(col)
#'   facet <- rlang::enquo(facet)
#'   facet2 <- rlang::enquo(facet2)
#'   xmin <- rlang::enquo(xmin)
#'   xmax <- rlang::enquo(xmax)
#'   xend <- rlang::enquo(xend)
#'   ymin <- rlang::enquo(ymin)
#'   ymax <- rlang::enquo(ymax)
#'   yend <- rlang::enquo(yend)
#'   z <- rlang::enquo(z)
#'   group <- rlang::enquo(group)
#'   subgroup <- rlang::enquo(subgroup)
#'   sample <- rlang::enquo(sample)
#'
#'   label <- rlang::enquo(label)
#'   text <- rlang::enquo(text)
#'
#'   if (rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
#'     plot <- data %>%
#'       ggplot2::ggplot(mapping = ggplot2::aes(
#'         # x = !!x,
#'         y = !!y,
#'         col = !!col,
#'         fill = !!col,
#'         xmin = !!xmin,
#'         xmax = !!xmax,
#'         xend = !!xend,
#'         ymin = !!ymin,
#'         ymax = !!ymax,
#'         yend = !!yend,
#'         z = !!z,
#'         group = !!group,
#'         subgroup = !!subgroup,
#'         sample = !!sample,
#'         label = !!label,
#'         text = !!text,
#'         # !!!mapping
#'       )) #+
#'       # mode
#'   }
#'   else if (rlang::quo_is_null(y) & !rlang::quo_is_null(x)) {
#'     plot <- data %>%
#'       ggplot2::ggplot(mapping = ggplot2::aes(
#'         x = !!x,
#'         # y = !!y,
#'         col = !!col,
#'         fill = !!col,
#'         xmin = !!xmin,
#'         xmax = !!xmax,
#'         xend = !!xend,
#'         ymin = !!ymin,
#'         ymax = !!ymax,
#'         yend = !!yend,
#'         z = !!z,
#'         group = !!group,
#'         subgroup = !!subgroup,
#'         sample = !!sample,
#'         label = !!label,
#'         text = !!text,
#'         # !!!mapping
#'       )) #+
#'   }
#'   else {
#'     plot <- data %>%
#'       ggplot2::ggplot(mapping = ggplot2::aes(
#'         x = !!x,
#'         y = !!y,
#'         col = !!col,
#'         fill = !!col,
#'         xmin = !!xmin,
#'         xmax = !!xmax,
#'         xend = !!xend,
#'         ymin = !!ymin,
#'         ymax = !!ymax,
#'         yend = !!yend,
#'         z = !!z,
#'         group = !!group,
#'         subgroup = !!subgroup,
#'         sample = !!sample,
#'         label = !!label,
#'         text = !!text,
#'         # !!!mapping
#'       )) #+
#'   }
#' }

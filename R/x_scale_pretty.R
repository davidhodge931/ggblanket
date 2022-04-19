#' #' Title
#' #'
#' #' @param data
#' #' @param x
#' #' @param y
#' #' @param col
#' #' @param facet
#' #' @param x_layer_data
#' #' @param x_balance
#' #' @param x_breaks
#' #' @param x_breaks_n
#' #' @param x_expand
#' #' @param x_labels
#' #' @param x_limits
#' #' @param x_oob
#' #' @param x_rev
#' #' @param x_zero
#' #' @param x_layer_data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' x_scale_pretty <-
#'   function(data,
#'            x,
#'            y = NULL,
#'            facet = NULL,
#'            facet_scales = "fixed",
#'            x_balance = FALSE,
#'            x_breaks = NULL,
#'            x_breaks_n = NULL,
#'            x_expand = NULL,
#'            x_labels = NULL,
#'            x_limits = NULL,
#'            x_oob = scales::oob_keep,
#'            x_rev = FALSE,
#'            x_zero = NULL,
#'            x_layer_data = NULL) {
#'
#'   x <- rlang::enquo(x)
#'   y <- rlang::enquo(y)
#'   facet <- rlang::enquo(facet)
#'
#'   if (is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data))) {
#'     if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
#'     if (rlang::is_null(x_labels)) x_labels <- snakecase::to_sentence_case
#'
#'     x_scale <- ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
#'   }
#'   else if (is.numeric(rlang::eval_tidy(x, data)) |
#'            lubridate::is.Date(rlang::eval_tidy(x, data)) |
#'            rlang::quo_is_null(x)) {
#'
#'     if (facet_scales %in% c("fixed", "free_y")) {
#'       if (is.null(x_zero)) {
#'         if ((is.numeric(rlang::eval_tidy(x, data)) |
#'              lubridate::is.Date(rlang::eval_tidy(x, data))) &
#'             !(is.numeric(rlang::eval_tidy(y, data)) |
#'               lubridate::is.Date(rlang::eval_tidy(y, data))))
#'           x_zero <- FALSE
#'         else
#'           x_zero <- TRUE
#'       }
#'
#'       if (is.null(x_layer_data)) {
#'         x_vctr <- dplyr::pull(data, !!x)
#'       }
#'       else if (!is.null(x_layer_data)){
#'         temp <- x_layer_data %>%
#'           dplyr::select(tidyselect::matches(stringr::regex("^x$|^xmin$|^xmax$|^xend$|^outliers$")))
#'
#'         if (any(stringr::str_detect(names(temp), stringr::regex("^outliers$")))) {
#'           temp <- temp %>%
#'             tidyr::unnest_longer(col = .data$outliers)
#'         }
#'
#'         x_vctr <- temp %>%
#'           tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "x") %>%
#'           dplyr::pull(.data$x)
#'
#'         if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
#'           x_vctr <- as.Date(x_vctr, origin = "1970-01-01")
#'         }
#'       }
#'
#'       x_min <- min(x_vctr, na.rm = TRUE)
#'       x_max <- max(x_vctr, na.rm = TRUE)
#'
#'       if ((x_min < 0 & x_max > 0)) x_zero <- FALSE
#'
#'       if (rlang::is_null(x_breaks)) {
#'         x_min_max <- c(x_min, x_max)
#'         if (x_zero) x_min_max <- c(0, x_min_max)
#'         if (x_balance) x_min_max <- c(-x_min_max, x_min_max)
#'         if (rlang::is_null(x_breaks_n)) x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 2)
#'         x_breaks <- pretty(x_min_max, n = x_breaks_n)
#'       }
#'
#'       if (rlang::is_null(x_limits)) x_limits <- c(min(x_breaks), max(x_breaks))
#'       if (rlang::is_null(x_expand)) x_expand <- c(0, 0)
#'     }
#'     else if (facet_scales %in% c("free", "free_x")) {
#'       if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
#'       x_limits <- NULL
#'       if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
#'     }
#'
#'     if (rlang::is_null(x_labels)) {
#'       if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) x_labels <- scales::label_comma()
#'       else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date()
#'       else x_labels <- ggplot2::waiver()
#'     }
#'
#'     if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) {
#'       x_scale <- ggplot2::scale_x_continuous(
#'         breaks = x_breaks,
#'         limits = x_limits,
#'         expand = x_expand,
#'         labels = x_labels,
#'         oob = x_oob
#'       )
#'     }
#'     else if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
#'       x_scale <- ggplot2::scale_x_date(
#'         breaks = x_breaks,
#'         limits = x_limits,
#'         expand = x_expand,
#'         labels = x_labels,
#'         oob = x_oob
#'       )
#'     }
#'   }
#' }

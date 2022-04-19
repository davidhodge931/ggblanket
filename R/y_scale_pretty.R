#' #' Title
#' #'
#' #' @param data
#' #' @param x
#' #' @param y
#' #' @param col
#' #' @param facet
#' #' @param y_layer_data
#' #' @param y_balance
#' #' @param y_breaks
#' #' @param y_breaks_n
#' #' @param y_expand
#' #' @param y_labels
#' #' @param y_limits
#' #' @param y_oob
#' #' @param y_rev
#' #' @param y_zero
#' #' @param y_layer_data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' y_scale_pretty <-
#'   function(data,
#'            x,
#'            y = NULL,
#'            facet = NULL,
#'            facet_scales = "fixed",
#'            y_balance = FALSE,
#'            y_breaks = NULL,
#'            y_breaks_n = NULL,
#'            y_expand = NULL,
#'            y_labels = NULL,
#'            y_limits = NULL,
#'            y_oob = scales::oob_keep,
#'            y_rev = FALSE,
#'            y_zero = NULL,
#'            y_layer_data = NULL) {
#'
#'     x <- rlang::enquo(x)
#'     y <- rlang::enquo(y)
#'     facet <- rlang::enquo(facet)
#'
#'     if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
#'       if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
#'       if (rlang::is_null(y_labels)) y_labels <- snakecase::to_sentence_case
#'
#'       y_scale <- ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
#'     }
#'     if (is.numeric(rlang::eval_tidy(y, data)) |
#'         lubridate::is.Date(rlang::eval_tidy(y, data)) |
#'         rlang::quo_is_null(y)) {
#'
#'         if (facet_scales %in% c("fixed", "free_x")) {
#'
#'           if (is.null(y_layer_data)) {
#'             x_vctr <- dplyr::pull(data, !!x)
#'           }
#'           else if (!is.null(y_layer_data)){
#'
#'             temp <- y_layer_data %>%
#'               dplyr::select(tidyselect::matches(stringr::regex("^y$|^ymin$|^ymax$|^yend$|^outliers$")))
#'
#'             if (any(stringr::str_detect(names(temp), stringr::regex("^outliers$")))) {
#'               temp <- temp %>%
#'                 tidyr::unnest_longer(col = .data$outliers)
#'             }
#'
#'             y_vctr <- temp %>%
#'               tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "y") %>%
#'               dplyr::pull(.data$y)
#'
#'             if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
#'               y_vctr <- as.Date(y_vctr, origin = "1970-01-01")
#'             }
#'           }
#'
#'           y_min <- min(y_vctr, na.rm = TRUE)
#'           y_max <- max(y_vctr, na.rm = TRUE)
#'
#'           if ((y_min < 0 & y_max > 0)) y_zero <- FALSE
#'
#'           if (rlang::is_null(y_breaks)) {
#'             y_min_max <- c(y_min, y_max)
#'             if (y_zero) y_min_max <- c(0, y_min_max)
#'             if (y_balance) y_min_max <- c(-y_min_max, y_min_max)
#'             if (rlang::is_null(y_breaks_n)) y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
#'             y_breaks <- pretty(y_min_max, n = y_breaks_n)
#'           }
#'
#'           if (rlang::is_null(y_limits)) y_limits <- c(min(y_breaks), max(y_breaks))
#'           if (rlang::is_null(y_expand)) y_expand <- c(0, 0)
#'         }
#'         else if (facet_scales %in% c("free", "free_y")) {
#'           if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
#'           y_limits <- NULL
#'           if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
#'         }
#'
#'         if (rlang::is_null(y_labels)) {
#'           if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) y_labels <- scales::label_comma()
#'           else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date()
#'           else y_labels <- ggplot2::waiver()
#'         }
#'
#'         if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) {
#'           y_scale <- ggplot2::scale_y_continuous(
#'             breaks = y_breaks,
#'             limits = y_limits,
#'             expand = y_expand,
#'             labels = y_labels,
#'             oob = y_oob
#'           )
#'         }
#'         else if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
#'           y_scale <- ggplot2::scale_y_date(
#'             breaks = y_breaks,
#'             limits = y_limits,
#'             expand = y_expand,
#'             labels = y_labels,
#'             oob = y_oob
#'           )
#'         }
#'     }
#'   }

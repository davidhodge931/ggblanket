#' #' Create base ggplot from aesthetic list
#' #'
#' #' @param data A data frame or tibble.
#' #' @param aes_list A list of aesthetic quosures already processed
#' #' @param mapping Additional aesthetic mappings
#' #'
#' #' @noRd
#' initialise_ggplot_from_list <- function(
#'     data,
#'     aes_list,
#'     mapping = NULL
#' ) {
#'   # Build base aesthetics from the provided aes_list
#'   base_aes <- ggplot2::aes()
#'
#'   # Add each non-NULL aesthetic
#'   if (!rlang::quo_is_null(aes_list$x)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(x = !!aes_list$x))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$y)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(y = !!aes_list$y))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$xmin)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(xmin = !!aes_list$xmin))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$xmax)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(xmax = !!aes_list$xmax))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$xend)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(xend = !!aes_list$xend))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$ymin)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(ymin = !!aes_list$ymin))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$ymax)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(ymax = !!aes_list$ymax))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$yend)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(yend = !!aes_list$yend))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$z)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(z = !!aes_list$z))
#'   }
#'
#'   # IMPORTANT: Add colour and fill from aes_list (which already has col inheritance)
#'   if (!rlang::quo_is_null(aes_list$colour)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(colour = !!aes_list$colour))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$fill)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(fill = !!aes_list$fill))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$group)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(group = !!aes_list$group))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$subgroup)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(subgroup = !!aes_list$subgroup))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$sample)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(sample = !!aes_list$sample))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$label)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(label = !!aes_list$label))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$text)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(text = !!aes_list$text))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$shape)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(shape = !!aes_list$shape))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$linetype)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(linetype = !!aes_list$linetype))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$linewidth)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(linewidth = !!aes_list$linewidth))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$size)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(size = !!aes_list$size))
#'   }
#'
#'   if (!rlang::quo_is_null(aes_list$alpha)) {
#'     base_aes <- utils::modifyList(base_aes, ggplot2::aes(alpha = !!aes_list$alpha))
#'   }
#'
#'   # Merge with additional mapping
#'   final_aes <- if (!is.null(mapping)) {
#'     utils::modifyList(base_aes, mapping)
#'   } else {
#'     base_aes
#'   }
#'
#'   # Create plot
#'   data |>
#'     ggplot2::ggplot(mapping = final_aes)
#' }

#' #' Title
#' #'
#' #' @param n
#' #' @param begin
#' #' @param end
#' #' @param option
#' #' @param rev
#' #'
#' #' @returns
#' #' @export
#' #'
#' #' @examples
#' mako_direction <- function(
#'     n = 20,
#'     begin = 0.05,
#'     end = 0.95,
#'     option = "G",
#'     rev = FALSE) {
#'
#'   if (!rev) {
#'     viridisLite::viridis(
#'       n = n,
#'       begin = begin,
#'       end = end,
#'       option = option,
#'       direction = ifelse(is_panel_background_dark(), 1, -1),
#'     )
#'   }
#'   else {
#'     viridisLite::viridis(
#'       n = n,
#'       begin = begin,
#'       end = end,
#'       option = option,
#'       direction = ifelse(is_panel_background_dark(), -1, 1),
#'     )
#'   }
#' }
#'
#' #' Title
#' #'
#' #' @param begin
#' #' @param end
#' #' @param option
#' #' @param rev
#' #'
#' #' @returns
#' #' @export
#' #'
#' #' @examples
#' pal_mako_direction <- function(
#'   begin = 0.05,
#'   end = 0.95,
#'   option = "G",
#'   rev = FALSE) {
#'
#'     if (!rev) {
#'       scales::pal_viridis(
#'         begin = begin,
#'         end = end,
#'         option = option,
#'         direction = ifelse(is_panel_background_dark(), 1, -1),
#'       )
#'     }
#'     else {
#'       scales::pal_viridis(
#'         begin = begin,
#'         end = end,
#'         option = option,
#'         direction = ifelse(is_panel_background_dark(), -1, 1),
#'       )
#'     }
#'   }

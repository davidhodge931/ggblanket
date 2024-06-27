#' Get a symmetric x continuous scale
#'
#' @param data A data frame or tibble.
#' @param x An unquoted variable.
#' @param ... Provided to force user argument naming etc.
#' @param breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param n_breaks If `breaks = NULL`, the desired number of breaks.
#' @param expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param expand_limits Any values that the limits should encompass (e.g. `0`).
#' @param labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels.
#' @param position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).
#' @param sec_axis A secondary axis created with [ggplot2::sec_axis()] or [ggplot2::dup_axis()].
#' @param transform A transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#'
#' @return A ggplot2 continuous x scale.
#' @export
get_scale_x_symmetric <- function(data = NULL,
                                  x = NULL,
                                  ...,
                                  breaks = NULL,
                                  n_breaks = 6,
                                  expand = NULL,
                                  expand_limits = NULL,
                                  labels = NULL,
                                  position = "bottom",
                                  sec_axis = ggplot2::waiver(),
                                  transform = NULL) {

  x <- rlang::enquo(x)

  vctr <- data %>%
    dplyr::pull(!!x)

  if (rlang::is_null(transform)) {
    if (inherits(vctr, what = c("hms"))) transform <- "hms"
    else if (inherits(vctr, what = c("POSIXt"))) transform <- "time"
    else if (inherits(vctr, what = c("Date"))) transform <- "date"
    else transform <- "identity"
  }

  if (is.character(transform)) transform_name <- transform
  else if (inherits(transform, what = "transform")) {
    transform_name <- transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  if (rlang::is_null(labels)) {
    if (any(transform_name == "hms")) labels <- scales::label_time()
    else if (any(transform_name %in% c("time", "datetime", "date"))) labels <- scales::label_date_short()
    else labels <- scales::label_comma(drop0trailing = TRUE)
  }

  if (!rlang::is_null(expand_limits)) {
    vctr <- c(vctr, expand_limits)
  }

  if (any(transform_name == "hms")) vctr <- hms::as_hms(vctr)
  else if (any(transform_name %in% c("time", "datetime"))) vctr <- lubridate::as_datetime(vctr)
  else if (any(transform_name == "date")) vctr <- lubridate::as_date(vctr)

  range <- range(vctr, na.rm = TRUE)
  if (any(transform_name == "hms")) range <- hms::as_hms(range)

  if (rlang::is_null(breaks)) {
    if (any(transform_name %in% c("hms", "time", "datetime", "date"))) {
      breaks <- scales::breaks_pretty(n = n_breaks)(range)
    }
    else {
      breaks <- scales::breaks_extended(n = n_breaks, only.loose = TRUE)(range)
    }
  }
  else if (is.function(breaks)) breaks <- breaks(range)

  limits <- range(breaks)

  if (any(transform_name %in% "reverse")) limits <- rev(limits)

  if (rlang::is_null(expand)) expand <- c(0, 0)

  ggplot2::scale_x_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    oob = scales::oob_keep,
    position = position,
    sec.axis = sec_axis,
    transform = transform
  )
}

#' Get a symmetric y continuous scale
#'
#' @param data A data frame or tibble.
#' @param y An unquoted variable.
#' @param ... Provided to force user argument naming etc.
#' @param breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param n_breaks If `breaks = NULL`, the desired number of breaks.
#' @param expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param expand_limits Any values that the limits should encompass (e.g. `0`).
#' @param labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels.
#' @param position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).
#' @param sec_axis A secondary axis created with [ggplot2::sec_axis()] or [ggplot2::dup_axis()].
#' @param transform A transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#'
#' @return A ggplot2 continuous y scale.
#' @export
get_scale_y_symmetric <- function(data = NULL,
                                  y = NULL,
                                  ...,
                                  breaks = NULL,
                                  n_breaks = 6,
                                  expand = NULL,
                                  expand_limits = NULL,
                                  labels = NULL,
                                  position = "left",
                                  sec_axis = ggplot2::waiver(),
                                  transform = NULL) {

  y <- rlang::enquo(y)

  vctr <- data %>%
    dplyr::pull(!!y)

  if (rlang::is_null(transform)) {
    if (inherits(vctr, what = c("hms"))) transform <- "hms"
    else if (inherits(vctr, what = c("POSIXt"))) transform <- "time"
    else if (inherits(vctr, what = c("Date"))) transform <- "date"
    else transform <- "identity"
  }

  if (is.character(transform)) transform_name <- transform
  else if (inherits(transform, what = "transform")) {
    transform_name <- transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  if (rlang::is_null(labels)) {
    if (any(transform_name == "hms")) labels <- scales::label_time()
    else if (any(transform_name %in% c("time", "datetime", "date"))) labels <- scales::label_date_short()
    else labels <- scales::label_comma(drop0trailing = TRUE)
  }

  if (!rlang::is_null(expand_limits)) {
    vctr <- c(vctr, expand_limits)
  }

  if (any(transform_name == "hms")) vctr <- hms::as_hms(vctr)
  else if (any(transform_name %in% c("time", "datetime"))) vctr <- lubridate::as_datetime(vctr)
  else if (any(transform_name == "date")) vctr <- lubridate::as_date(vctr)

  range <- range(vctr, na.rm = TRUE)
  if (any(transform_name == "hms")) range <- hms::as_hms(range)

  if (rlang::is_null(breaks)) {
    if (any(transform_name %in% c("hms", "time", "datetime", "date"))) {
      breaks <- scales::breaks_pretty(n = n_breaks)(range)
    }
    else {
      breaks <- scales::breaks_extended(n = n_breaks, only.loose = TRUE)(range)
    }
  }
  else if (is.function(breaks)) breaks <- breaks(range)

  limits <- range(breaks)

  if (any(transform_name %in% "reverse")) limits <- rev(limits)

  if (rlang::is_null(expand)) expand <- c(0, 0)

  ggplot2::scale_y_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    oob = scales::oob_keep,
    position = position,
    sec.axis = sec_axis,
    transform = transform
  )
}

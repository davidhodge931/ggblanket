#' Symmetric x continuous scale
#'
#' @param ... Provided to force user argument naming etc.
#' @param data A data frame or tibble.
#' @param x An unquoted variable.
#' @param symmetric `TRUE` or `FALSE` of whether a symmetric scale.
#' @param breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param breaks_n If `breaks = NULL`, the desired number of breaks.
#' @param expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param expand_limits Any values that the limits should encompass (e.g. `0`).
#' @param labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels.
#' @param position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).
#' @param sec_axis A secondary axis created with [ggplot2::sec_axis()] or [ggplot2::dup_axis()].
#' @param transform A transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#'
#' @return A ggplot2 continuous x scale.
#' @keywords internal
#'
scale_x_symmetric <- function(...,
                              data = NULL,
                              x = NULL,
                              symmetric = TRUE,
                              breaks = NULL,
                              breaks_n = 6,
                              expand = NULL,
                              expand_limits = NULL,
                              labels = NULL,
                              position = "bottom",
                              sec_axis = ggplot2::waiver(),
                              transform = "identity") {

  if (is.character(transform)) transform_name <- transform
  else if (inherits(transform, what = "transform")) {
    transform_name <- transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  if (any(stringr::str_detect(transform_name, "log-")) |
      any(transform_name %in% c("log", "log2", "log10"))) {

    symmetric <- FALSE

    rlang::inform("ggblanket does not currenly support log symmetric axes")
  } #remove and update below once there is a mechanism to ensure breaks surround data


  if (symmetric) {
    x <- rlang::enquo(x)

    vctr <- data %>%
      dplyr::pull(!!x)

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
        breaks <- scales::breaks_pretty(n = breaks_n)(range)
      }
      else if (any(stringr::str_detect(transform_name, "log-")) |
               any(transform_name %in% c("log", "log2", "log10"))) {
        breaks <- scales::breaks_log(n = breaks_n)(range) # update here
      }
      else {
        breaks <- scales::breaks_extended(n = breaks_n, only.loose = TRUE)(range)
      }
    }
    else if (is.function(breaks)) breaks <- breaks(range)

    limits <- range(breaks)

    if (any(transform_name %in% "reverse")) limits <- rev(limits)

    if (rlang::is_null(expand)) expand <- ggplot2::expansion(mult = c(0, 0))

    if (rlang::is_null(labels)) {
      if (any(transform_name == "hms")) labels <- scales::label_time()
      else if (any(transform_name %in% c("time", "datetime", "date"))) labels <- scales::label_date_short()
      else labels <- scales::label_comma(drop0trailing = TRUE)
    }

    scale <- ggplot2::scale_x_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits,
      expand = expand,
      oob = scales::oob_keep,
      transform = transform,
      position = position,
      sec.axis = sec_axis
    )
  }
  else {
    if (rlang::is_null(breaks)) {
      if (any(transform_name %in% c("hms", "time", "datetime", "date"))) {
        breaks <- scales::breaks_pretty(n = breaks_n)
      }
      else if (any(stringr::str_detect(transform_name, "log-")) |
               any(transform_name %in% c("log", "log2", "log10"))) {
        breaks <- scales::breaks_log(n = breaks_n)
      }
      else {
        breaks <- scales::breaks_extended(n = breaks_n, only.loose = FALSE)
      }
    }

    if (rlang::is_null(expand)) expand <- ggplot2::expansion(mult = c(0.05, 0.05))

    if (rlang::is_null(labels)) {
      if (any(transform_name == "hms")) labels <- scales::label_time()
      else if (any(transform_name %in% c("time", "datetime", "date"))) labels <- scales::label_date_short()
      else labels <- scales::label_comma(drop0trailing = TRUE)
    }

    scale <- list(
      ggplot2::scale_x_continuous(
        breaks = breaks,
        labels = labels,
        expand = expand,
        oob = scales::oob_keep,
        transform = transform,
        position = position,
        sec.axis = sec_axis
      ),
      ggplot2::expand_limits(x = expand_limits)
    )
  }

  return(scale)
}

#' Symmetric y continuous scale
#'
#' @param ... Provided to force user argument naming etc.
#' @param data A data frame or tibble.
#' @param y An unquoted variable.
#' @param symmetric `TRUE` or `FALSE` of whether a symmetric scale.
#' @param breaks A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector of breaks.
#' @param breaks_n If `breaks = NULL`, the desired number of breaks.
#' @param expand Padding to the limits with the [ggplot2::expansion()] function, or a vector of length 2 (e.g. `c(0, 0)`).
#' @param expand_limits Any values that the limits should encompass (e.g. `0`).
#' @param labels A function that takes the breaks as inputs (e.g. `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a vector of labels.
#' @param position The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or `"top"`).
#' @param sec_axis A secondary axis created with [ggplot2::sec_axis()] or [ggplot2::dup_axis()].
#' @param transform A transformation object (e.g. [scales::transform_log10()]) or character string of this minus the `transform_` prefix (e.g. `"log10"`).
#'
#' @return A ggplot2 continuous y scale.
#' @keywords internal
#'
scale_y_symmetric <- function(...,
                              data = NULL,
                              y = NULL,
                              symmetric = TRUE,
                              breaks = NULL,
                              breaks_n = 6,
                              expand = NULL,
                              expand_limits = NULL,
                              labels = NULL,
                              position = "left",
                              sec_axis = ggplot2::waiver(),
                              transform = "identity") {

  if (is.character(transform)) transform_name <- transform
  else if (inherits(transform, what = "transform")) {
    transform_name <- transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  if (any(stringr::str_detect(transform_name, "log-")) |
      any(transform_name %in% c("log", "log2", "log10"))) {

    symmetric <- FALSE

    rlang::inform("ggblanket does not currenly support log symmetric axes")
  } #remove and update below once there is a mechanism to ensure breaks surround data

  if (symmetric) {
    y <- rlang::enquo(y)

    vctr <- data %>%
      dplyr::pull(!!y)

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
        breaks <- scales::breaks_pretty(n = breaks_n)(range)
      }
      else if (any(stringr::str_detect(transform_name, "log-")) |
               any(transform_name %in% c("log", "log2", "log10"))) {
        breaks <- scales::breaks_log(n = breaks_n)(range) # update here
      }
      else {
        breaks <- scales::breaks_extended(n = breaks_n, only.loose = TRUE)(range)
      }
    }
    else if (is.function(breaks)) breaks <- breaks(range)

    limits <- range(breaks)

    if (any(transform_name %in% "reverse")) limits <- rev(limits)

    if (rlang::is_null(expand)) expand <- ggplot2::expansion(mult = c(0, 0))

    if (rlang::is_null(labels)) {
      if (any(transform_name == "hms")) labels <- scales::label_time()
      else if (any(transform_name %in% c("time", "datetime", "date"))) labels <- scales::label_date_short()
      else labels <- scales::label_comma(drop0trailing = TRUE)
    }

    scale <- ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits,
      expand = expand,
      oob = scales::oob_keep,
      transform = transform,
      position = position,
      sec.axis = sec_axis
    )
  }
  else {
    if (rlang::is_null(breaks)) {
      if (any(transform_name %in% c("hms", "time", "datetime", "date"))) {
        breaks <- scales::breaks_pretty(n = breaks_n)
      }
      else if (any(stringr::str_detect(transform_name, "log-")) |
               any(transform_name %in% c("log", "log2", "log10"))) {
        breaks <- scales::breaks_log(n = breaks_n)
      }
      else {
        breaks <- scales::breaks_extended(n = breaks_n, only.loose = FALSE)
      }
    }

    if (rlang::is_null(expand)) expand <- ggplot2::expansion(mult = c(0.05, 0.05))

    if (rlang::is_null(labels)) {
      if (any(transform_name == "hms")) labels <- scales::label_time()
      else if (any(transform_name %in% c("time", "datetime", "date"))) labels <- scales::label_date_short()
      else labels <- scales::label_comma(drop0trailing = TRUE)
    }

    scale <- list(
      ggplot2::scale_y_continuous(
        breaks = breaks,
        labels = labels,
        expand = expand,
        oob = scales::oob_keep,
        transform = transform,
        position = position,
        sec.axis = sec_axis
      ),
      ggplot2::expand_limits(y = expand_limits)
    )
  }

  return(scale)
}

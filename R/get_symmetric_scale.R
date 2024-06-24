get_scale_x_symmetric <- function(data = NULL,
                                  x = NULL,
                                  ...,
                                  x_breaks = NULL,
                                  x_breaks_n = 6,
                                  x_expand = NULL,
                                  x_expand_limits = NULL,
                                  x_labels = NULL,
                                  x_position = "bottom",
                                  x_sec_axis = ggplot2::waiver(),
                                  x_transform = NULL) {

  x <- rlang::enquo(x)

  x_vctr <- data %>%
    dplyr::pull(!!x)

  if (rlang::is_null(x_transform)) {
    if (inherits(x_vctr, what = c("hms"))) x_transform <- "hms"
    else if (inherits(x_vctr, what = c("POSIXt"))) x_transform <- "time"
    else if (inherits(x_vctr, what = c("Date"))) x_transform <- "date"
    else x_transform <- "identity"
  }

  if (is.character(x_transform)) x_transform_name <- x_transform
  else if (inherits(x_transform, what = "transform")) {
    x_transform_name <- x_transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  if (rlang::is_null(x_labels)) {
    if (any(x_transform_name == "hms")) x_labels <- scales::label_time()
    else if (any(x_transform_name %in% c("time", "datetime", "date"))) x_labels <- scales::label_date_short()
    else x_labels <- scales::label_comma(drop0trailing = TRUE)
  }

  if (!rlang::is_null(x_expand_limits)) {
    x_vctr <- c(x_vctr, x_expand_limits)
  }

  if (any(x_transform_name == "hms")) x_vctr <- hms::as_hms(x_vctr)
  else if (any(x_transform_name %in% c("time", "datetime"))) x_vctr <- lubridate::as_datetime(x_vctr)
  else if (any(x_transform_name == "date")) x_vctr <- lubridate::as_date(x_vctr)

  x_range <- range(x_vctr, na.rm = TRUE)
  if (any(x_transform_name == "hms")) x_range <- hms::as_hms(x_range)

  if (rlang::is_null(x_breaks)) {
    if (any(x_transform_name %in% c("hms", "time", "datetime", "date"))) {
      x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)
    }
    else {
      x_breaks <- scales::breaks_extended(n = x_breaks_n, only.loose = TRUE)(x_range)
    }
  }
  else if (is.function(x_breaks)) x_breaks <- x_breaks(x_range)

  x_limits <- range(x_breaks)

  if (any(x_transform_name %in% "reverse")) x_limits <- rev(x_limits)

  if (rlang::is_null(x_expand)) x_expand <- c(0, 0)

  x_scale <- ggplot2::scale_x_continuous(
    limits = x_limits,
    expand = x_expand,
    breaks = x_breaks,
    labels = x_labels,
    oob = scales::oob_keep,
    position = x_position,
    sec.axis = x_sec_axis,
    transform = x_transform
  )

  return(x_scale)
}


get_scale_y_symmetric <- function(data = NULL,
                                  y = NULL,
                                  ...,
                                  y_breaks = NULL,
                                  y_breaks_n = 6,
                                  y_expand = NULL,
                                  y_expand_limits = NULL,
                                  y_labels = NULL,
                                  y_position = "left",
                                  y_sec_axis = ggplot2::waiver(),
                                  y_transform = NULL) {

  y <- rlang::enquo(y)

  y_vctr <- data %>%
    dplyr::pull(!!y)

  if (rlang::is_null(y_transform)) {
    if (inherits(y_vctr, what = c("hms"))) y_transform <- "hms"
    else if (inherits(y_vctr, what = c("POSIXt"))) y_transform <- "time"
    else if (inherits(y_vctr, what = c("Date"))) y_transform <- "date"
    else y_transform <- "identity"
  }

  if (is.character(y_transform)) y_transform_name <- y_transform
  else if (inherits(y_transform, what = "transform")) {
    y_transform_name <- y_transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  if (rlang::is_null(y_labels)) {
    if (any(y_transform_name == "hms")) y_labels <- scales::label_time()
    else if (any(y_transform_name %in% c("time", "datetime", "date"))) y_labels <- scales::label_date_short()
    else y_labels <- scales::label_comma(drop0trailing = TRUE)
  }

  if (!rlang::is_null(y_expand_limits)) {
    y_vctr <- c(y_vctr, y_expand_limits)
  }

  if (any(y_transform_name == "hms")) y_vctr <- hms::as_hms(y_vctr)
  else if (any(y_transform_name %in% c("time", "datetime"))) y_vctr <- lubridate::as_datetime(y_vctr)
  else if (any(y_transform_name == "date")) y_vctr <- lubridate::as_date(y_vctr)

  y_range <- range(y_vctr, na.rm = TRUE)
  if (any(y_transform_name == "hms")) y_range <- hms::as_hms(y_range)

  if (rlang::is_null(y_breaks)) {
    if (any(y_transform_name %in% c("hms", "time", "datetime", "date"))) {
      y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)
    }
    else {
      y_breaks <- scales::breaks_extended(n = y_breaks_n, only.loose = TRUE)(y_range)
    }
  }
  else if (is.function(y_breaks)) y_breaks <- y_breaks(y_range)

  y_limits <- range(y_breaks)

  if (any(y_transform_name %in% "reverse")) y_limits <- rev(y_limits)

  if (rlang::is_null(y_expand)) y_expand <- c(0, 0)

  y_scale <- ggplot2::scale_y_continuous(
    limits = y_limits,
    expand = y_expand,
    breaks = y_breaks,
    labels = y_labels,
    oob = scales::oob_keep,
    position = y_position,
    sec.axis = y_sec_axis,
    transform = y_transform
  )

  return(y_scale)
}


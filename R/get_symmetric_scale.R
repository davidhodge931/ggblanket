get_scale_x_symmetric <- function(data = NULL,
                                  x = NULL,
                                  x_breaks = NULL,
                                  x_breaks_n = 6,
                                  x_expand = NULL,
                                  x_expand_limits = NULL,
                                  x_labels = NULL,
                                  x_position = "bottom",
                                  x_sec_axis = ggplot2::waiver(),
                                  x_transform = NULL, #scales::transform_identity(), #or "hms", "time" or "date"
                                  position = NULL) {

  x <- rlang::enquo(x)

  x_vctr <- data %>%
    dplyr::pull(!!x)

  if (rlang::is_null(x_transform)) {
    if (inherits(rlang::eval_tidy(x, data), what = c("hms"))) x_transform <- "time"
    if (inherits(rlang::eval_tidy(x, data), what = c("POSIXt"))) x_transform <- "time"
    if (inherits(rlang::eval_tidy(x, data), what = c("Date"))) x_transform <- "date"
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

  if (!rlang::is_null(position)) {
    if (position == "fill") {
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
      x_limits <- c(0, 1)
    }
  }
  else if (rlang::is_null(x_breaks)) {
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

library(scales)
library(tidyverse)
library(palmerpenguins)
library(ggblanket)

set_blanket()

penguins |>
  ggplot() +
  geom_point(aes(x = bill_depth_mm, body_mass_g)) +
  get_scale_x_symmetric(
    penguins,
    x = bill_depth_mm,
    # x_position = "top",
    # x_expand = c(0.05,0.05),
    x_labels = scales::label_currency(accuracy = 1),
    # x_expand_limits = 5,
    # x_breaks = scales::breaks_extended(5, only.loose = TRUE),
    x_breaks_n = 10,
    x_sec_axis = dup_axis(),
    # x_transform = transform_compose(transform_log()),
    x_transform = c("sqrt", "reverse"),
  )

economics |>
  ggplot() +
  geom_point(aes(x = date, y = unemploy)) +
  get_scale_x_symmetric(
    economics,
    x = date,
    # x_transform = "date",
    # x_breaks_n = 10,
    )

 economics |>
  ggplot() +
  geom_point(aes(x = date, y = unemploy)) +
  get_scale_x_symmetric(
    economics,
    x = date,
    x_transform = c("date", "reverse"),
    # x_transform = "date",
    # x_breaks_n = 10,
  )

flights_0101_0102 <-
  nycflights13::flights %>%
  filter(month == 1, day <= 2) %>%
  group_by(time_hour = lubridate::floor_date(time_hour, "hour")) %>%
  summarize(num_flights = n())

p <- flights_0101_0102 %>%
  ggplot() +
  geom_point(aes(x = time_hour, y = num_flights)) +
  get_scale_x_symmetric(flights_0101_0102, x = time_hour)

ggplot_build(p)[[1]]

flights_0101_0102 |>
  mutate()
  mutate(time_hour2 = lubridate::as_datetime(time_hour, origin = "1970-01-01"))

lubridate::as_datetime(1357034400)
lubridate::as_date(-915)

p

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aes_contrast.R
\name{aes_contrast}
\alias{aes_contrast}
\title{A colour aesthetic that automatically contrasts with fill.}
\usage{
aes_contrast(
  theme_family = "grey_mode",
  col_pal_dark = NULL,
  col_pal_light = NULL
)
}
\arguments{
\item{theme_family}{The ggblanket theme family default colours. Either "grey_mode", "light_mode", or "dark_mode". Defaults to "grey_mode".}

\item{col_pal_dark}{A dark colour for use on light fill, which over-rides defaults selected based on the theme_family.}

\item{col_pal_light}{A light colour for use on dark fill, which over-rides defaults selected based on the theme_family.}
}
\value{
An aesthetic
}
\description{
A colour aesthetic that automatically contrasts with fill. Can be spliced into \link[ggplot2:aes]{ggplot2::aes} with \link[rlang:splice-operator]{rlang::!!!}.
}
\examples{
library(palmerpenguins)
library(dplyr)
library(ggplot2)

penguins |>
  count(species, sex) |>
  gg_col(
    x = sex,
    y = n,
    col = species,
    position = position_dodge2(preserve = "single"),
    width = 0.75,
    x_labels = \(x) stringr::str_to_sentence(x),
  ) +
  geom_text(
    mapping = aes(y = n - (max(n * 0.04)), label = n,
                  !!!aes_contrast()),
    position = position_dodge2(width = 0.75, preserve = "single"),
    show.legend = FALSE,
  )
}
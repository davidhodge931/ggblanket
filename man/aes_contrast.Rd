% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aes_contrast.R
\name{aes_contrast}
\alias{aes_contrast}
\title{A colour aesthetic that automatically contrasts with fill.}
\usage{
aes_contrast(contrast_pal = lightness[2:3])
}
\arguments{
\item{contrast_pal}{A vector of a dark colour and then a light colour (e.g. \code{greyness[2:3]} or \code{darkness[1:2]}). Defaults to \code{lightness[2:3]}.}
}
\value{
An aesthetic
}
\description{
A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into \link[ggplot2:aes]{ggplot2::aes} with \link[rlang:splice-operator]{rlang::!!!}.
}
\examples{
library(ggplot2)
library(dplyr)
library(stringr)
library(palmerpenguins)

set_blanket()

penguins |>
  count(species, sex) |>
  gg_col(
    x = sex,
    y = n,
    col = species,
    position = position_dodge2(preserve = "single"),
    width = 0.75,
    x_labels = \(x) str_to_sentence(x),
  ) +
  geom_text(
    mapping = aes(label = n, !!!aes_contrast()),
    position = position_dodge2(width = 0.75, preserve = "single"),
    vjust = 1.33,
    show.legend = FALSE,
  )

penguins |>
  count(species, sex) |>
  gg_col(
    x = n,
    y = sex,
    col = species,
    position = position_dodge2(preserve = "single"),
    width = 0.75,
    y_labels = \(x) str_to_sentence(x),
    mode = dark_mode_r(),
  ) +
  geom_text(
    mapping = aes(label = n, !!!aes_contrast(darkness[1:2])),
    position = position_dodge2(width = 0.75, preserve = "single"),
    hjust = 1.25,
    show.legend = FALSE,
  )
}

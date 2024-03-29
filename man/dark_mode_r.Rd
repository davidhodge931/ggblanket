% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dark_mode.R
\name{dark_mode_r}
\alias{dark_mode_r}
\title{Dark mode theme with right legend}
\usage{
dark_mode_r(base_size = 11, base_family = "")
}
\arguments{
\item{base_size}{The base size of the text. Defaults to 11.}

\item{base_family}{The base family of the text. Defaults to "".}
}
\value{
A ggplot theme.
}
\description{
Dark mode theme for a ggplot visualisation with legend at right. It uses the colours from \code{darkness}.
}
\examples{
library(palmerpenguins)
library(ggplot2)

set_blanket()

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = species,
    mode = dark_mode_r()
  )

}

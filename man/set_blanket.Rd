% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_blanket.R
\name{set_blanket}
\alias{set_blanket}
\title{Set a style}
\usage{
set_blanket(
  ...,
  mode = light_mode_r(),
  colour = "#357BA2FF",
  colour_text = "#121B24FF",
  colour_label = colour_text,
  colour_reference_line = colour_text,
  colour_curve = colour_reference_line,
  fill = colour,
  fill_label = colour_label,
  alpha = 1,
  alpha_area = 0.9,
  alpha_bar = 0.9,
  alpha_boxplot = 0.6,
  alpha_crossbar = 0.6,
  alpha_density = 0.6,
  alpha_label = 0.05,
  alpha_polygon = 0.9,
  alpha_rect = 0.9,
  alpha_ribbon = 0.6,
  alpha_smooth = 0.6,
  alpha_tile = 0.9,
  alpha_violin = 0.9,
  alpha_recursive = NULL,
  linewidth = 0.66,
  linewidth_reference_line = 0.33,
  linewidth_curve = linewidth_reference_line,
  size_point = 1.5,
  size_pointrange = 0.2,
  size_sf = 1.5,
  size_text = 11/2.835052,
  size_label = size_text,
  family_text = "",
  family_label = family_text,
  col_palette_d = jumble,
  col_palette_na_d = "#CDC5BFFF",
  col_palette_c = viridisLite::mako(n = 9, direction = -1),
  col_palette_na_c = "#988F88FF",
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_o = "#988F88FF",
  theme = light_mode_r() + mode_orientation_to_x()
)
}
\arguments{
\item{...}{Provided to force user argument naming etc.}

\item{mode}{A ggplot2 theme (e.g. \code{\link[=light_mode_t]{light_mode_t()}} or \code{\link[=dark_mode_r]{dark_mode_r()}}) that anticipates \verb{gg_*} side-effects of removing relevant axis line/ticks and gridlines per the \code{mode_orientation}.}

\item{colour}{A default hex colour for the colour of geoms without a more specific \verb{colour_*} argument.}

\item{colour_text}{A default hex colour for the colour of the "text" geom.}

\item{colour_label}{A default hex colour for the colour of the "label" geom.}

\item{colour_reference_line}{A default hex colour for the colour of the "hline", "vline" and "abline" geoms.}

\item{colour_curve}{A default hex colour for the colour of the "curve" geom.}

\item{fill}{A default hex colour for the fill of geoms without a more specific \verb{fill_*} argument.}

\item{fill_label}{A default hex colour for the fill of the "label" geom.}

\item{alpha}{A default alpha for geoms without a more specific \verb{alpha_*} argument.}

\item{alpha_area}{A default alpha for the "area" geom.}

\item{alpha_bar}{A default alpha for the "bar" geom.}

\item{alpha_boxplot}{A default alpha for the "boxplot" geom.}

\item{alpha_crossbar}{A default alpha for the "crossbar" geom.}

\item{alpha_density}{A default alpha for the "density" geom.}

\item{alpha_label}{A default alpha for the "label" geom.}

\item{alpha_polygon}{A default alpha for the "polygon" geom.}

\item{alpha_rect}{A default alpha for the "rect" geom.}

\item{alpha_ribbon}{A default alpha for the "ribbon" geom.}

\item{alpha_smooth}{A default alpha for the "smooth" geom.}

\item{alpha_tile}{A default alpha for the "tile" geom.}

\item{alpha_violin}{A default alpha for the "violin" geom.}

\item{alpha_recursive}{A default alpha applied to all geoms.}

\item{linewidth}{A default linewidth for geoms.}

\item{linewidth_reference_line}{A default linewidth for the the "hline", "vline" and "abline" geoms.}

\item{linewidth_curve}{A default linewidth for the the "curve" geom.}

\item{size_point}{A default size for the "point" geom.}

\item{size_pointrange}{A default size for the "pointrange" geom.}

\item{size_sf}{A default size for the "sf" geom.}

\item{size_text}{A default size for the "text" geom.}

\item{size_label}{A default size for the "label" geom.}

\item{family_text}{A default family for the "text" geom.}

\item{family_label}{A default family for the "text" geom.}

\item{col_palette_d}{For a discrete scale, a character vector of hex codes.}

\item{col_palette_na_d}{For a discrete scale, a hex code.}

\item{col_palette_c}{For a continuous scale, a character vector of hex codes.}

\item{col_palette_na_c}{For a continuous scale, a hex code.}

\item{col_palette_o}{For an ordinal scale, a \verb{scales::pal_*()} function.}

\item{col_palette_na_o}{For an ordinal scale, a hex code.}

\item{theme}{A ggplot2 theme that the \verb{gg_*} function will add without side-effects. Note, the \code{mode} takes precedence, unless \code{mode = NULL}.}
}
\value{
A globally set style.
}
\description{
Set a style by setting:
\itemize{
\item the mode
\item geom defaults
\item col_palettes for discrete, continuous and ordinal scales.
}

Alternatively, use the \verb{weave_*} functions to only apply a subset of these.
}
\examples{
library(ggplot2)
library(ggblanket)
library(palmerpenguins)

set_blanket(
  mode = dark_mode_r(),
  colour = orange,
  annotation_colour = darkness[1],
)

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    x_breaks = scales::breaks_pretty(3),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.25), y = I(0.75), label = "Here")

penguins |>
  gg_histogram(
    x = flipper_length_mm,
    x_breaks = scales::breaks_pretty(3),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.75), y = I(0.75), label = "Here")

}

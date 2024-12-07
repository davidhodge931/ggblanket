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
  fill = colour,
  text_colour = "#121B24FF",
  text_size = 11/2.835052,
  text_family = "",
  reference_line_colour = "#121B24FF",
  reference_line_linewidth = 0.33,
  col_palette_d = jumble,
  col_palette_c = viridisLite::mako(n = 9, direction = -1),
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_d = "#CDC5BFFF",
  col_palette_na_c = "#988F88FF",
  col_palette_na_o = "#988F88FF",
  theme = light_mode_r() + mode_orientation_to_x()
)
}
\arguments{
\item{...}{Provided to force user argument naming etc.}

\item{mode}{A ggplot2 theme (e.g. \code{\link[=light_mode_t]{light_mode_t()}} or \code{\link[=dark_mode_r]{dark_mode_r()}}) that anticipates \verb{gg_*} side-effects of removing relevant axis line/ticks and gridlines per the \code{mode_orientation}.}

\item{colour}{A default hex colour for the colour of geoms (other than text or reference line geoms).}

\item{fill}{A default hex colour for the fill of geoms (other than text or reference line geoms).}

\item{text_colour}{A default hex colour for the colour (and fill) of the "text" and "label" geoms.}

\item{text_size}{A default size for the "text" and "label" geoms.}

\item{text_family}{A default family for the "text" and "label" geoms.}

\item{reference_line_colour}{A default hex colour for the colour of the "hline", "vline", "abline" and "curve" geoms.}

\item{reference_line_linewidth}{A default linewidth for the the "hline", "vline", "abline" and "curve" geoms.}

\item{col_palette_d}{For a discrete scale, a character vector of hex codes.}

\item{col_palette_c}{For a continuous scale, a character vector of hex codes.}

\item{col_palette_o}{For an ordinal scale, a \verb{scales::pal_*()} function.}

\item{col_palette_na_d}{For a discrete scale, a hex code.}

\item{col_palette_na_c}{For a continuous scale, a hex code.}

\item{col_palette_na_o}{For an ordinal scale, a hex code.}

\item{theme}{A ggplot2 theme that the \verb{gg_*} function will add without side-effects if the mode is set/weaved to \code{NULL} (and also is applied to ggplot code outside of ggblanket).}
}
\value{
A globally set style.
}
\description{
Weave the style by setting:
\enumerate{
\item the mode to be added with \verb{gg_*()} side-effects
\item the colour/fill geom default, and other defaults for text, reference line and curve geoms
\item the col_palettes for discrete, continuous and ordinal colour/fill scales
\item a theme to be added \emph{without} \verb{gg_*()} side-effects.
}

Alternatively, use the \verb{weave_*} functions to only apply a subset of these.
\code{\link[ggplot2:update_defaults]{ggplot2::update_geom_defaults()}} can be used to further fine-tune geom defaults.
}
\examples{
library(ggplot2)
library(ggblanket)
library(palmerpenguins)

set_blanket(
  mode = dark_mode_r(),
  colour = "#E7298AFF",
  text_colour = darkness[1],
  reference_line_colour = darkness[1],
  col_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
                    "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF"),
)

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.25), y = I(0.75), label = "Here")

penguins |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.75), y = I(0.75), label = "Here")

}

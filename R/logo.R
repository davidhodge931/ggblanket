library(tidyverse)
library(hexSticker)
library(ggblanket)
library(palmerpenguins)

plot <- iris |>
  gg_density(x = Sepal.Length,
             col = Species,
             alpha = 1,
             position = "stack",
             col_legend_place = "n")

sticker(
  plot,
  package = "ggblanket",
  p_color = "white",
  h_color = ggblanket:::pal_viridis_reorder(1),
  h_fill = "#C1D1DD",
  p_size = 25,
  p_y = 1.1,
  p_x = 1.0,
  s_width = 7,
  s_height = 30,
  filename = "logo.png",
  white_around_sticker = TRUE
)

usethis::use_logo("logo.png")

rm("logo.png")

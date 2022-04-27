library(tidyverse)
library(hexSticker)
library(simplevis)

nz <- simplevis::example_borders %>%
  mutate(dummy = "1") %>%
  sf::st_as_sf()

plot <- gg_sf_col(
  nz,
  dummy,
  title = "",
  pal = "#f8fafb",
  col_labels = "",
  col_title = ""
) +
  theme(panel.background = element_rect(fill = "#EAEFF3")) +
  theme(panel.grid = element_blank())

plot

sticker(
  plot,
  package = "ggblanket",
  p_color = simplevis:::pal_viridis_reorder(1),
  h_color = simplevis:::pal_viridis_reorder(1),
  h_fill = "#C1D1DD",
  p_size = 25,
  p_family = "Helvetica",
  p_y = 1.1,
  p_x = 1.0,
  s_x = 0.3,
  s_y = 0.74,
  s_width = 10,
  s_height = 15,
  filename = "logo.png",
  white_around_sticker = TRUE
)

usethis::use_logo("logo.png")

file.remove("logo.png")

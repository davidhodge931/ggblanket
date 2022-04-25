# library(tidyverse)
# library(hexSticker)
# library(ggblanket)
#
# pal_interior <- "white"
# pal_sides <- pal_viridis_reorder(1)
# pal <- c(pal_interior, pal_sides)
# scales::show_col(pal)
#
# bg <- gg_density(
#   penguins,
#   x = body_mass_g,
#   col = sex,
#   col_na_rm = T,
#   pal = pal,
#   position = "fill",
#   theme = gg_theme(void = T),
#   col_legend_place = "n"
# )
#
# sticker(
#   bg,
#   package = "ggblanket",
#   p_color = ggblanket:::pal_viridis_reorder(1),
#   h_color = ggblanket:::pal_viridis_reorder(1),
#   h_fill = "#C1D1DD",
#   p_size = 25,
#   p_family = "Helvetica",
#   p_y = 1.0,
#   p_x = 1.0,
#   s_x = 0.3,
#   s_y = 0.74,
#   s_width = 10,
#   s_height = 15,
#   filename = "logo.png",
#   white_around_sticker = TRUE
# )
#
# usethis::use_logo("logo.png")
#
# rm("logo.png")

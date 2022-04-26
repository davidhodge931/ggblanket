# library(tidyverse)
# library(hexSticker)
# library(ggblanket)
# library(palmerpenguins)
#
# pal_sides <- pal_d3_reorder(1)
# pal_interior <- pal_d3_reorder(2)[2]
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
#   p_color = "white",
#   h_color = ggblanket:::pal_viridis_reorder(1),
#   h_fill = "#C1D1DD",
#   p_size = 25,
#   p_y = 1.1,
#   p_x = 1.0,
#   s_width = 50,
#   s_height = 150,
#   filename = "logo.png",
#   white_around_sticker = TRUE
# )
#
# usethis::use_logo("logo.png")
#
# rm("logo.png")

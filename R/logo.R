# 1. Make empty logo
#
# library(ggplot2)
# library(hexSticker)
#
# plot <- ggplot() +
#   geom_blank() +
#   theme_void()
#
# sticker(
#   plot,
#   package = "",
#   p_color = "White",
#   h_color = pal_viridis_reorder(1),
#   h_fill = "#C1D1DD",
#   p_size = 25,
#   p_family = "Helvetica",
#   p_y = 1.1,
#   p_x = 1.0,
#   s_x = 0.3,
#   s_y = 0.74,
#   s_width = 10,
#   s_height = 15,
#   filename = "logo.png",
#   white_around_sticker = TRUE
# )
#
# 2. Add to word and add text
# blanket = Karma 72 (from Google fonts)
# gg = Abadi 72
#
# 3. Use snipping tool to make an image
#
# 4. Make background transparent
# https://www.remove.bg/upload
#
# 5. Add to package
# usethis::use_logo(...)

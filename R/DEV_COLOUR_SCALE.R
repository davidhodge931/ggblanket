# penguins |>
#   gg_layer(
#     x = flipper_length_mm,
#     # y = body_mass_g,
#     stat = "bin",
#   )
#
# ?gg_sf
#
# sf::st_read(system.file("shape/nc.shp", package = "sf")) |>
#   gg_layer(
#     y = AREA,
#     # pal = viridis::rocket(9, direction = -1),
#     stat = "bin") +
#   geom_histogram()
#
# library(palmerpenguins)
# library(tidyverse)
#
# penguins |>
#   gg_layer(
#     x = sex,
#     y = body_mass_g,
#     # geom = "path",
#     position = "jitter",
#     stat = "identity"
#   )
#
# penguins |>
#   gg_point(
#     x = sex,
#     y = body_mass_g,
#     y_breaks = scales::breaks_pretty(4))
#
#
# penguins |>
#   gg_wrap(
#     x = sex,
#     y = body_mass_g,
#     # mapping = aes(alpha = species),
#     geom = "path",
#     position = "jitter",
#     stat = "identity"
#   )
#
#
# penguins |>
#   # mutate(species = as.character(species)) |>
#   pull(sex) |>
#   levels()
#
# bench::mark({
#   plot <- diamonds |>
#   ggplot(aes(
#     x = carat,
#     y = price))
#
# plot_build <- ggplot_build(plot)
# plot_build <- ggplot_build(plot)
#
#   })
#
# bench::mark(print(plot2))
#
# ###################################################
# pal <- viridisLite::mako(18, direction = -1)
# pal_na <- "#bebebe"
#
# col_rescale <- scales::rescale()
# col_labels <- scales::label_comma()
# col_breaks <- scales::breaks_pretty(5)
# col_limits <- NULL
# col_trans <- "identity"
# col_oob <- scales::oob_keep
# col_expand <- ggplot2::waiver()
#
# #continuous with gradient
# diamonds |>
#   gg_layer(
#     x = carat,
#     y = price,
#     geom = "hex",
#     stat = "binhex",
#   ) +
#   scale_fill_gradientn(
#     colours = pal,
#     values = col_rescale,
#     limits = col_limits,
#     expand = col_expand,
#     breaks = col_breaks,
#     labels = col_labels,
#     trans = col_trans,
#     oob = col_oob,
#     na.value = pal_na,
#   ) +
#   scale_colour_gradientn(
#     colours = pal,
#     values = col_rescale,
#     limits = col_limits,
#     expand = col_expand,
#     breaks = col_breaks,
#     labels = col_labels,
#     trans = col_trans,
#     oob = col_oob,
#     na.value = pal_na,
#   ) +
#   expand_limits(colour = 7500,
#                 fill = 7500)
#
# #discrete
# pal <- guardian()
# pal_na <- "#bebebe"
#
# pal_guardian <- guardian()
#
# col_labels <- ggplot2::waiver()
# col_breaks <- ggplot2::waiver()
# col_limits <- NULL
# col_expand <- ggplot2::waiver()
#
# penguins |>
#   ggplot(aes(flipper_length_mm, species, colour = sex)) +
#   geom_point() +
#   scale_colour_manual(values = c('red', 'blue', 'green', 'brown'))
#
# penguins |>
#   ggplot(aes(flipper_length_mm, species, colour = sex)) +
#   geom_point() +
#   scale_colour_manual(values = guardian())
#
# penguins |>
#   ggplot(aes(flipper_length_mm, species, colour = species)) +
#   geom_point() +
#   scale_colour_manual(values = \(x) scales::pal_hue(x))
#
# scales::
#
# scales::pal_hue()(3)
# scales::show_col(RColorBrewer::brewer.pal(8, "Dark2"))
#
# # pal_dark_mode
# library(scales)
# show_col(pal_hue()(4))
#
# pal
# pal_na
# pal_guardian
# pal_census
#
# bench::mark({
# pal_mako <- viridisLite::mako(18, direction = -1)
#
# ggplot2::faithfuld |>
#   gg_raster(
#     x = waiting,
#     y = eruptions,
#     col = density,
#     pal = pal_mako,
#     pal_na = "grey")
# })
#
# ?gg_raster
#
#     # geom = "point",
#     # stat = "identity",
#   # ) +
#   # scale_fill_manual(
#   #   values = pal)
#
# scales::show_col(viridis(256, option = "D"))
#
# penguins |>
#   gg_point(
#     x = species,
#     y = sex,
#     col = species
#     # geom = "point",
#     # stat = "identity",
#   ) +
#   scale_fill_manual(
#     values = pal,
#     limits = col_limits,
#     expand = col_expand,
#     breaks = col_breaks,
#     labels = col_labels,
#     na.value = pal_na,
#     drop = FALSE, #consider col_drop argument
#   ) +
#   scale_colour_manual(
#     values = pal,
#     limits = col_limits,
#     expand = col_expand,
#     breaks = col_breaks,
#     labels = col_labels,
#     na.value = pal_na,
#     drop = FALSE, #consider col_drop argument
#   )
#
#
#
#
#
# #Get plot data and flipped status
# # plot_build <- ggplot2::ggplot_build(plot2)
# # plot_data <- plot_build$data[[1]]
#
# #correct for plots where col is null, but there is a colour scale
# purrr::map_chr(ggplot2::ggplot_build(plot2)$plot$scales$scales, \(x) rlang::call_name((x[["call"]])))
# print(col_scales)
#
#
# plot2
# plot_build <- ggplot2::ggplot_build(plot2)
# plot_data <- plot_build$data[[1]]
#
# #correct for plots where col is null, but there is a colour scale
# col_scales <- purrr::map_chr(plot_build$plot$scales$scales, \(x) rlang::call_name((x[["call"]])))
#
#
# diamonds |>
#   # ggplot(aes()) +
#   gg_layer(
#     x = carat,
#     y = price,
#     geom = "hex",
#     stat = "binhex",
#     position = "identity",
#     pal = "red",
#     # params = list(colour = "red")
#   )
#
# ?gg_hex
#
# penguins |>
#   ggplot(aes(x = sex,
#              y = body_mass_g)) +
#   layer(
#     geom = GeomPoint,
#     stat = StatSummary,
#     position = position_jitter(),
#   )
#
# rev(pal_mako)
#
# scales::show_col()
#
#
# penguins |>
#   gg_layer(x = sex,
#            y = body_mass_g,
#            geom = "path",
#            stat = "summary")
#
# penguins |>
#   ggplot(aes(x = sex,
#              y = body_mass_g)) +
#   layer(geom = "point",
#         stat = "identity",
#         position = "identity",
#         params = list(alpha = 0.2, colour = "red", fill = "blue")
#         )
#
# penguins |>
#   ggplot() +
#   geom_point(
#     aes(x = flipper_length_mm, y = NULL),
#     stat = "bin",
#   )
#
# penguins |>
#   ggplot() +
#   geom_point(
#     aes(x = flipper_length_mm, z = NULL, sample = NULL),
#     stat = "bin",
#   )
#
# ###
# penguins |>
#   ggplot() +
#   geom_bar(
#     aes(x = sex, y = NULL),
#     stat = "count",
#   )
#
# ?gg_rect
#
# ###
# penguins |>
#   ggplot() +
#   geom_bar(
#     aes(x = sex, fill = NULL),
#     stat = "count",
#   )
#
# data.frame(
#   x = rep(c(2, 5, 7, 9, 12), 2),
#   y = rep(c(1, 2), each = 5),
#   z = factor(c(rep(1:3, each = 3), 4)),
#   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
# ) %>%
#   dplyr::mutate(
#     xmin = x - w / 2,
#     xmax = x + w / 2,
#     ymin = y,
#     ymax = y + 1
#   ) %>%
#   ggplot() +
#   geom_point(
#     aes(xmin = xmin,
#     xmax = xmax,
#     ymin = ymin,
#     ymax = ymax,
#     fill = NULL,
#     col = NULL,
#     geometry = NULL,
#     x = NULL,
#      y = NULL,
#   ))

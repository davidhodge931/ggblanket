# testthat::skip_if(getRversion() <= package_version("4.1.0"))
# testthat::skip_on_os(c("mac", "linux"))
#
# library(palmerpenguins)
# library(ggplot2)
# set_blanket(col = "#121b24")
#
# test_name <- "theme_lighter_right"
#
# test_that(test_name, {
#   set.seed(123)
#
#   p <- penguins |>
#     gg_jitter(
#       x = flipper_length_mm,
#       y = island,
#       col = bill_depth_mm,
#       mapping = aes(alpha = species, shape = species),
#       theme = theme_lighter()
#     ) +
#     scale_alpha_manual(values = c(1, 1, 0.33))
#
#   vdiffr::expect_doppelganger(test_name, p)
# })
#
# test_name <- "theme_lighter_top"
#
# test_that(test_name, {
#   set.seed(123)
#
#   p <- penguins |>
#     gg_point(
#       x = flipper_length_mm,
#       y = island,
#       col = bill_depth_mm,
#       facet = sex,
#       mapping = aes(alpha = species, shape = species),
#       theme = theme_lighter(legend_position = "top")
#     ) +
#     scale_alpha_manual(values = c(1, 1, 0.33))
#
#   vdiffr::expect_doppelganger(test_name, p)
# })
#
# test_name <- "theme_lighter_bottom"
#
# test_that(test_name, {
#   set.seed(123)
#
#   p <- penguins |>
#     gg_point(
#       x = flipper_length_mm,
#       y = island,
#       col = bill_depth_mm,
#       facet = sex,
#       mapping = aes(alpha = species, shape = species),
#       theme = theme_lighter(legend_position = "bottom")
#     ) +
#     scale_alpha_manual(values = c(1, 1, 0.33))
#
#   vdiffr::expect_doppelganger(test_name, p)
# })
#
# set_blanket()

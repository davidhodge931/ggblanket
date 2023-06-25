library(ggplot2)
# theme_set(theme_gray)
# theme_get()

gg_theme_light2 <- function (
    base_size = 10,
    base_family = "",
    base_line_size = base_size/33,
    base_rect_size = base_size/33) {

  half_line <- base_line_size / 2

  theme(line = element_line(colour = "#121b24", linewidth = base_line_size, linetype = 1, lineend = "square"),
        rect = element_rect(fill = "#e6ecf2", colour = "#e6ecf2", linewidth = base_rect_size, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "#121b24", size = base_size, lineheight = 0.9,
                            hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(),
                            debug = FALSE),
        axis.line = NULL,
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.text = NULL,

        axis.text.x = element_text(margin = margin(t = base_size * 0.2), vjust = 1),
        axis.text.x.top = element_text(margin = margin(b = base_size * 0.2), vjust = 0),
        axis.text.y = element_text(margin = margin(r = base_size * 0.2), hjust = 1),
        axis.text.y.right = element_text(margin = margin(l = base_size * 0.2), hjust = 0),

        axis.ticks.length.x = grid::unit(base_line_size * 11, "pt"),
        axis.ticks.length.x.top = NULL,
        axis.ticks.length.x.bottom = NULL,
        axis.ticks.length.y = grid::unit(base_line_size * 8.25, "pt"),
        axis.ticks.length.y.left = NULL,
        axis.ticks.length.y.right = NULL,

        axis.title = NULL,
        axis.title.x = element_text(margin = margin(t = base_size * 0.75)),
        axis.title.x.top = element_text(margin = margin(b = base_size * 0.75)),
        axis.title.y = element_text(margin = margin(r = base_size * 1), angle = 90),
        axis.title.y.right = element_text(margin = margin(l = base_size * 1), angle = -90),

        legend.spacing = unit(base_size / 8, "pt"),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.margin = margin(),
        legend.key = NULL,
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0, vjust = 0),
        legend.title.align = NULL,
        legend.position = "right",
        legend.direction = "vertical",
        legend.justification = "left",
        legend.box = NULL,
        legend.box.margin = ggplot2::margin(t = -2.5),
        legend.box.background = NULL,
        legend.box.spacing = NULL,
        panel.background = element_rect(fill = "#fcfdfe", colour = "#fcfdfe"),
        panel.border = element_blank(),
        panel.grid = element_line(colour = "#DBE1E7", linewidth = rel(0.33)),
        panel.grid.major = NULL,
        panel.grid.minor = element_blank(),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        strip.background = NULL,
        strip.clip = "inherit",

        strip.text = NULL,
        strip.text.x = element_text(margin = margin(b = base_size * 0.5)),
        strip.text.x.bottom = element_text(margin = margin(t = base_size * 0.5)),
        strip.text.y = element_text(margin = margin(l = base_size * 2/3), angle = -90),
        strip.text.y.left = element_text(margin = margin(r = base_size * 2/3), angle = 90),

        strip.placement = "outside",
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = unit(half_line/2, "pt"),
        strip.switch.pad.wrap = unit(half_line/2, "pt"),

        plot.background = element_rect(),
        plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0, vjust = 1, margin = margin(t = ((base_size ^ 0.5) * -0.5) - 3.85, b = base_size * 1.75)),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(t = base_size * -1.5, b = base_size * 1.75)),
        plot.caption = element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(t = base_size * 1.25)),
        plot.caption.position = "plot",
        plot.tag = element_text(size = rel(1.2), hjust = 0, vjust = 0.5),
        plot.tag.position = "topleft",
        plot.margin = ggplot2::margin(t = 33 * base_line_size, r = 44 * base_line_size, b = 24.2 * base_line_size, l = 22 * base_line_size),
        complete = TRUE
  )
}



(2) / (10)


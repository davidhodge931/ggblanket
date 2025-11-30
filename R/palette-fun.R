# Updated helper function - works with any input and any aesthetic
as_discrete_palette <- function(palette) {
  # Already a function - return as-is
  if (is.function(palette)) {
    return(palette)
  }

  # NULL or waiver - return as-is (let ggplot2 handle defaults)
  if (is.null(palette) || inherits(palette, "waiver")) {
    return(palette)
  }

  # Atomic vector (character, numeric, etc.) - convert to function
  if (is.atomic(palette)) {
    return(function(n) {
        palette[seq_len(n)]
    })
  }

  # Anything else - error
  stop("palette must be a function, vector, or NULL", call. = FALSE)
}

# =============================================================================
# SIMPLE EXAMPLES - ALL AESTHETICS
# =============================================================================

library(ggplot2)
library(palmerpenguins)

# Test 1: FILL - color vector
# -----------------------------------------------------------------
fill_vec <- c("coral", "steelblue", "forestgreen")
as_discrete_palette(fill_vec)(3)
scales::as_discrete_pal(fill_vec)(3)
# [1] "coral"       "steelblue"   "forestgreen"

# Test 2: COLOUR - named color vector
# -----------------------------------------------------------------
colour_vec <- c("Adelie" = "red", "Chinstrap" = "blue", "Gentoo" = "green")
as_discrete_palette(colour_vec)(3)
scales::as_discrete_pal(colour_vec)(3)

# [1] "red"   "blue"  "green"

# Test 3: SHAPE - numeric vector
# -----------------------------------------------------------------
shape_vec <- c(21, 22, 23, 24, 25)
shape_vec <- c("Adelie" = 21, "Chinstrap" = 23, "Gentoo" = 25)

as_discrete_palette(shape_vec)(3)

# [1] 21 22 23

# Test 4: LINETYPE - numeric vector
# -----------------------------------------------------------------
linetype_vec <- c(1, 2, 3, 4, 5, 6)
as_discrete_palette(linetype_vec)(3)
# [1] 1 2 3

# Test 5: LINETYPE - character vector
# -----------------------------------------------------------------
linetype_char <- c("solid", "dashed", "dotted")
as_discrete_palette(linetype_char)(3)
# [1] "solid"  "dashed" "dotted"

# Test 6: SIZE - numeric vector
# -----------------------------------------------------------------
size_vec <- c(1, 2, 3, 4)
as_discrete_palette(size_vec)(3)
# [1] 1 2 3

# Test 7: ALPHA - numeric vector
# -----------------------------------------------------------------
alpha_vec <- c(0.3, 0.6, 0.9)
as_discrete_palette(alpha_vec)(3)
# [1] 0.3 0.6 0.9

# Test 8: FUNCTION input - returns unchanged
# -----------------------------------------------------------------
fn <- scales::pal_viridis()
identical(as_discrete_palette(fn), fn)
# [1] TRUE

# Test 9: NULL input - returns NULL
# -----------------------------------------------------------------
as_discrete_palette(NULL)
# NULL

# Test 10: Recycling warning when not enough values
# -----------------------------------------------------------------
short_vec <- c("red", "blue")
as_discrete_palette(short_vec)(5)
# Warning: Not enough values in palette. Recycling.
# [1] "red"  "blue" "red"  "blue" "red"

# =============================================================================
# VISUAL EXAMPLES - In actual plots
# =============================================================================

# Example A: FILL with vector
penguins |>
  ggplot(aes(flipper_length_mm, body_mass_g, fill = species)) +
  geom_point(shape = 21, size = 3) +
  discrete_scale("fill", palette = as_discrete_palette(c("coral", "steelblue", "forestgreen")))

# Example B: SHAPE with vector
penguins |>
  ggplot(aes(flipper_length_mm, body_mass_g, shape = species)) +
  geom_point(size = 3) +
  discrete_scale("shape", palette = as_discrete_palette(c(21, 22, 23)))

# Example C: LINETYPE with character vector
economics_long |>
  dplyr::filter(variable %in% c("pop", "unemploy", "psavert")) |>
  ggplot(aes(date, value01, linetype = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  discrete_scale("linetype", palette = as_discrete_palette(c("solid", "dashed", "dotted")))

# Example D: SIZE with numeric vector
penguins |>
  ggplot(aes(flipper_length_mm, body_mass_g, size = species)) +
  geom_point() +
  discrete_scale("size", palette = as_discrete_palette(c(2, 4, 6)))

# Example E: Works with functions too (no change needed)
penguins |>
  ggplot(aes(flipper_length_mm, body_mass_g, fill = species)) +
  geom_point(shape = 21, size = 3) +
  discrete_scale("fill", palette = as_discrete_palette(scales::pal_viridis()))


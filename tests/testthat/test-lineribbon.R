testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)

set_blanket()

set.seed(123)

## ---------------------------------------------------------------------------------------------------
test_name <- "datetime"

test_that(test_name, {
  set.seed(123)
  one <- sample(5:15, 10)
  two <- rev(one)

  p <- cbind.data.frame(
    x = rep(1:10, 2),
    y = c(one, two),
    l = c(one - 1, two - 1),
    h = c(one + 1, two + 1),
    id = rep(c("one", "two"), each = 10)
  ) |>
    gg_lineribbon(
      x = x,
      y = y,
      ymin = l,
      ymax = h,
      col = id,
      mapping = aes(linetype = id),
      blend = "multiply",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

na_if_double <- function(x) {
  if (is.object(x)) {
    return(x)
  }

  if (is.integer(x)) {
    x <- as.double(x)
  }

  if (is.numeric(x)) {
    x <- dplyr::na_if(x, Inf)
  }

  x
}

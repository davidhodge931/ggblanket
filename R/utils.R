#' Convert Infinite values to NA
#'
#' @param x A vector
#'
#' @noRd
na_if_inf <- function(x) {
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

continuous_scales_col <- c(
  "scale_colour_continuous",
  "scale_colour_binned",
  "scale_colour_brewer",
  "scale_colour_date",
  "scale_colour_datetime",
  "scale_colour_distiller",
  "scale_colour_fermenter",
  "scale_colour_gradient",
  "scale_colour_gradient2",
  "scale_colour_gradientn",
  "scale_colour_grey",
  "scale_colour_steps",
  "scale_colour_steps2",
  "scale_colour_stepsn",
  "scale_colour_viridis_b",
  "scale_colour_viridis_c",

  "scale_fill_continuous",
  "scale_fill_binned",
  "scale_fill_brewer",
  "scale_fill_date",
  "scale_fill_datetime",
  "scale_fill_distiller",
  "scale_fill_fermenter",
  "scale_fill_gradient",
  "scale_fill_gradient2",
  "scale_fill_gradientn",
  "scale_fill_grey",
  "scale_fill_steps",
  "scale_fill_steps2",
  "scale_fill_stepsn",
  "scale_fill_viridis_b",
  "scale_fill_viridis_c"
)

discrete_scales_col <- c(
  "scale_colour_discrete",
  "scale_colour_manual",
  "scale_colour_ordinal",
  "scale_colour_viridis_d",

  "scale_fill_discrete",
  "scale_fill_manual",
  "scale_fill_ordinal",
  "scale_fill_viridis_d"
)

continuous_scales_alpha <- c(
  "scale_alpha_continuous",
  "scale_alpha",
  "scale_alpha_binned",
  "scale_alpha_date",
  "scale_alpha_datetime"
)

discrete_scales_alpha <- c(
  "scale_alpha_discrete",
  "scale_alpha_manual",
  "scale_alpha_ordinal"
)

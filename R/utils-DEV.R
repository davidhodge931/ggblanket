#' Detect if input is aesthetic or fixed value
#' @noRd
detect_aesthetic_or_value <- function(quo_input, arg_name = "col") {
  if (rlang::quo_is_null(quo_input)) {
    return(list(is_aesthetic = FALSE, value = NULL))
  }

  # Check if it's a symbol or call (aesthetic)
  if (rlang::quo_is_symbol(quo_input) || rlang::quo_is_call(quo_input)) {
    return(list(is_aesthetic = TRUE, value = quo_input))
  }

  # Try to evaluate
  tryCatch({
    eval_value <- rlang::eval_tidy(quo_input)

    # Check based on argument type
    if (arg_name %in% c("col", "colour", "fill")) {
      # Color: single character string
      if (is.character(eval_value) && length(eval_value) == 1) {
        return(list(is_aesthetic = FALSE, value = eval_value))
      }
    } else if (arg_name == "shape") {
      # Shape: single numeric or character
      if ((is.numeric(eval_value) || is.character(eval_value)) &&
          length(eval_value) == 1) {
        return(list(is_aesthetic = FALSE, value = eval_value))
      }
    } else if (arg_name == "linetype") {
      # Linetype: single numeric or character
      if ((is.numeric(eval_value) || is.character(eval_value)) &&
          length(eval_value) == 1) {
        return(list(is_aesthetic = FALSE, value = eval_value))
      }
    }

    # Otherwise treat as aesthetic
    return(list(is_aesthetic = TRUE, value = quo_input))

  }, error = function(e) {
    # If evaluation fails, treat as aesthetic
    return(list(is_aesthetic = TRUE, value = quo_input))
  })
}

#' Create base ggplot with aesthetic mappings
#'
#' @param data A data frame or tibble.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend,z,col,colour,fill,group,subgroup,label,text,sample An unquoted aesthetic variable.
#' @param shape,linetype,linewidth,size Can be either aesthetic or fixed value
#' @param mapping Additional aesthetic mappings
#'
#' @noRd
initialise_ggplot <- function(
    data,
    x = NULL,
    xmin = NULL,
    xmax = NULL,
    xend = NULL,
    y = NULL,
    ymin = NULL,
    ymax = NULL,
    yend = NULL,
    z = NULL,
    col = NULL,  # This represents both colour and fill
    colour = NULL,  # This represents both colour and fill
    fill = NULL,  # This represents both colour and fill
    shape = NULL,
    linetype = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    subgroup = NULL,
    sample = NULL,
    label = NULL,
    text = NULL,
    linewidth = NULL,
    size = NULL,
    mapping = NULL
) {
  # Quote all aesthetics
  aes_list <- list(
    x = rlang::enquo(x),
    y = rlang::enquo(y),
    colour = rlang::enquo(colour),
    fill = rlang::enquo(fill),
    col = rlang::enquo(col),
    xmin = rlang::enquo(xmin),
    xmax = rlang::enquo(xmax),
    xend = rlang::enquo(xend),
    ymin = rlang::enquo(ymin),
    ymax = rlang::enquo(ymax),
    yend = rlang::enquo(yend),
    z = rlang::enquo(z),
    group = rlang::enquo(group),
    subgroup = rlang::enquo(subgroup),
    sample = rlang::enquo(sample),
    label = rlang::enquo(label),
    text = rlang::enquo(text),
    shape = rlang::enquo(shape),
    linetype = rlang::enquo(linetype),
    linewidth = rlang::enquo(linewidth),
    size = rlang::enquo(size)
  )

  # Build base aesthetics
  base_aes <- ggplot2::aes(
    xmin = !!aes_list$xmin,
    xmax = !!aes_list$xmax,
    xend = !!aes_list$xend,
    ymin = !!aes_list$ymin,
    ymax = !!aes_list$ymax,
    yend = !!aes_list$yend,
    z = !!aes_list$z,
    group = !!aes_list$group,
    subgroup = !!aes_list$subgroup,
    sample = !!aes_list$sample,
    label = !!aes_list$label,
    text = !!aes_list$text,
    shape = !!aes_list$shape,
    linetype = !!aes_list$linetype,
    linewidth = !!aes_list$linewidth,
    size = !!aes_list$size
  )

  # Add x and y if provided
  if (!rlang::quo_is_null(aes_list$x)) {
    base_aes <- utils::modifyList(base_aes, ggplot2::aes(x = !!aes_list$x))
  }

  if (!rlang::quo_is_null(aes_list$y)) {
    base_aes <- utils::modifyList(base_aes, ggplot2::aes(y = !!aes_list$y))
  }

  # Add col/fill if provided (col maps to both colour and fill)
  if (!rlang::quo_is_null(aes_list$col)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(colour = !!aes_list$col, fill = !!aes_list$col)
    )
  }
  if (!rlang::quo_is_null(aes_list$colour)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(colour = !!aes_list$colour)
    )
  }
  if (!rlang::quo_is_null(aes_list$fill)) {
    base_aes <- utils::modifyList(
      base_aes,
      ggplot2::aes(fill = !!aes_list$fill)
    )
  }

  # Merge with additional mapping
  final_aes <- if (!is.null(mapping)) {
    utils::modifyList(base_aes, mapping)
  } else {
    base_aes
  }

  # Create plot
  data |>
    ggplot2::ggplot(mapping = final_aes)
}

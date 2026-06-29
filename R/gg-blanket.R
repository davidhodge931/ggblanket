#' Any ggplot
#'
#' @description A blank ggplot with [geom_blank()][ggplot2::geom_blank()] defaults for the geom, stat and position.
#'
#' Note this is an all-purpose custom wrapper intended for use with any geom.
#'
#' @param data A data frame.
#' @param ... Arguments passed to the geom layer, including geom params.
#' @param geom A geom as a string (`"point"`). Note relevant geom library must be loaded.
#' @param stat A stat as a string (`"identity"`). Note relevant stat library must be loaded.
#' @param position A position as a function (`ggplot2::position_identity()`).
#' @param before A ggplot2 layer to add before the geom layer. Unaffected by border transformations.
#' @param with A function to apply to the geom layer.
#' @param border Whether to apply border colour and linewidth. `TRUE` forces border on, `FALSE` forces off.
#' @param theme A complete theme function. Defaults to that globally set.
#' @param refine A bare function from the ggrefine package. Defaults to that globally set.
#' @param x Variable mapped to x.
#' @param xmin Variable mapped to xmin.
#' @param xmax Variable mapped to xmax.
#' @param xend Variable mapped to xend.
#' @param xintercept Variable mapped to xintercept.
#' @param y Variable mapped to y.
#' @param ymin Variable mapped to ymin.
#' @param ymax Variable mapped to ymax.
#' @param yend Variable mapped to yend.
#' @param yintercept Variable mapped to yintercept.
#' @param z Variable mapped to z.
#' @param fill Variable mapped to fill, or a set value. When mapped, colour inherits the same mapping unless colour is specified separately.
#' @param colour Variable mapped to colour, or a set value. When not specified and fill is mapped, colour inherits from fill.
#' @param alpha Variable mapped to alpha, or a set value.
#' @param shape Variable mapped to shape, or a set value.
#' @param linetype Variable mapped to linetype, or a set value.
#' @param linewidth Variable mapped to linewidth, or a set value.
#' @param size Variable mapped to size, or a set value.
#' @param stroke Variable mapped to stroke, or a set value.
#' @param label Variable mapped to label, or a set value.
#' @param weight Variable mapped to weight, or a set value.
#' @param group Variable mapped to group, or a set value.
#' @param width Variable mapped to width, or a set value.
#' @param height Variable mapped to height, or a set value.
#' @param slope Variable mapped to slope, or a set value.
#' @param intercept Variable mapped to intercept, or a set value.
#' @param sample Variable mapped to sample, or a set value.
#' @param angle Variable mapped to angle, or a set value.
#' @param radius Variable mapped to radius, or a set value.
#' @param mapping Additional aesthetic mappings from [ggplot2::aes()], merged with individual aesthetic arguments.
#' @param x_type Scale type for x. One of `"continuous"`, `"discrete"`, or `"binned"`. Auto-detected if `NULL`.
#' @param x_subtype Scale subtype for x. One of `"date"`, `"datetime"`, `"time"`, or `NA`. Auto-detected if `NULL`.
#' @param x_breaks Breaks for the x scale. Defaults to [scales::breaks_pretty()] for date/datetime subtypes, otherwise [scales::breaks_extended()].
#' @param x_drop Whether to drop unused levels for a discrete x scale. Defaults to `TRUE`.
#' @param x_expand Expansion for the x scale. Defaults to zero expansion where the limit is zero, otherwise 5% expansion.
#' @param x_guide Guide for the x scale. Defaults to [ggplot2::waiver()].
#' @param x_labels Labels for the x scale. Defaults to [scales::label_date_short()] for date/datetime, otherwise [scales::label_number()].
#' @param x_limits Limits for the x scale. Accepts a vector or a function.
#' @param x_minor_breaks Minor breaks for the x scale.
#' @param x_name Name/title for the x scale. Defaults to [ggplot2::waiver()].
#' @param x_oob Out-of-bounds handler for the x scale. Defaults to [scales::oob_censor].
#' @param x_palette Palette for a discrete x scale. Defaults to `seq_len`.
#' @param x_position Position of the x axis. Either `"bottom"` (default) or `"top"`.
#' @param x_sec_axis Secondary axis for x. Defaults to [ggplot2::waiver()].
#' @param x_transform Transform for the x scale. Auto-detected from subtype if `NULL`.
#' @param y_type Scale type for y. One of `"continuous"`, `"discrete"`, or `"binned"`. Auto-detected if `NULL`.
#' @param y_subtype Scale subtype for y. One of `"date"`, `"datetime"`, `"time"`, or `NA`. Auto-detected if `NULL`.
#' @param y_breaks Breaks for the y scale. Defaults to [scales::breaks_pretty()] for date/datetime subtypes, otherwise [scales::breaks_extended()].
#' @param y_drop Whether to drop unused levels for a discrete y scale. Defaults to `TRUE`.
#' @param y_expand Expansion for the y scale. Defaults to zero expansion where the limit is zero, otherwise 5% expansion.
#' @param y_guide Guide for the y scale. Defaults to [ggplot2::waiver()].
#' @param y_labels Labels for the y scale. Defaults to [scales::label_date_short()] for date/datetime, otherwise [scales::label_number()].
#' @param y_limits Limits for the y scale. Accepts a vector or a function.
#' @param y_minor_breaks Minor breaks for the y scale.
#' @param y_name Name/title for the y scale. Defaults to [ggplot2::waiver()].
#' @param y_oob Out-of-bounds handler for the y scale. Defaults to [scales::oob_censor].
#' @param y_palette Palette for a discrete y scale. Defaults to `seq_len`.
#' @param y_position Position of the y axis. Either `"left"` (default) or `"right"`.
#' @param y_sec_axis Secondary axis for y. Defaults to [ggplot2::waiver()].
#' @param y_transform Transform for the y scale. Auto-detected from subtype if `NULL`.
#' @param fill_type Scale type for fill. One of `"continuous"`, `"discrete"`, or `"binned"`. Auto-detected if `NULL`.
#' @param fill_subtype Scale subtype for fill. Auto-detected if `NULL`.
#' @param fill_breaks Breaks for the fill scale.
#' @param fill_drop Whether to drop unused levels for a discrete fill scale. Defaults to `TRUE`.
#' @param fill_guide Guide for the fill scale.
#' @param fill_labels Labels for the fill scale.
#' @param fill_limits Limits for the fill scale.
#' @param fill_name Name/title for the fill scale.
#' @param fill_oob Out-of-bounds handler for the fill scale. Defaults to [scales::oob_censor].
#' @param fill_rescaler Rescaler for the fill scale. Defaults to [scales::rescale].
#' @param fill_palette Palette for the fill scale.
#' @param fill_transform Transform for the fill scale. Auto-detected from subtype if `NULL`.
#' @param colour_type Scale type for colour. Inherits from `fill_type` if `NULL`.
#' @param colour_subtype Scale subtype for colour. Inherits from `fill_subtype` if `NULL`.
#' @param colour_breaks Breaks for the colour scale. Inherits from `fill_breaks` if `NULL`.
#' @param colour_drop Whether to drop unused levels for a discrete colour scale. Inherits from `fill_drop` if `NULL`.
#' @param colour_guide Guide for the colour scale. Inherits from `fill_guide` if `NULL`.
#' @param colour_labels Labels for the colour scale. Inherits from `fill_labels` if `NULL`.
#' @param colour_limits Limits for the colour scale. Inherits from `fill_limits` if `NULL`.
#' @param colour_name Name/title for the colour scale. Inherits from `fill_name` if `NULL`.
#' @param colour_oob Out-of-bounds handler for the colour scale. Inherits from `fill_oob` if `NULL`.
#' @param colour_rescaler Rescaler for the colour scale. Inherits from `fill_rescaler` if `NULL`.
#' @param colour_palette Palette for the colour scale.
#' @param colour_transform Transform for the colour scale. Inherits from `fill_transform` if `NULL`.
#' @param alpha_type Scale type for alpha. One of `"continuous"`, `"discrete"`, or `"binned"`. Auto-detected if `NULL`.
#' @param alpha_subtype Scale subtype for alpha. Auto-detected if `NULL`.
#' @param alpha_breaks Breaks for the alpha scale.
#' @param alpha_drop Whether to drop unused levels for a discrete alpha scale. Defaults to `TRUE`.
#' @param alpha_guide Guide for the alpha scale. Defaults to `NULL`.
#' @param alpha_labels Labels for the alpha scale.
#' @param alpha_limits Limits for the alpha scale.
#' @param alpha_name Name/title for the alpha scale.
#' @param alpha_oob Out-of-bounds handler for the alpha scale. Defaults to [scales::oob_censor].
#' @param alpha_palette Palette for the alpha scale.
#' @param alpha_transform Transform for the alpha scale.
#' @param size_type Scale type for size. One of `"continuous"`, `"discrete"`, or `"binned"`. Auto-detected if `NULL`.
#' @param size_subtype Scale subtype for size. Auto-detected if `NULL`.
#' @param size_breaks Breaks for the size scale.
#' @param size_drop Whether to drop unused levels for a discrete size scale. Defaults to `TRUE`.
#' @param size_guide Guide for the size scale. Defaults to `NULL`.
#' @param size_labels Labels for the size scale.
#' @param size_limits Limits for the size scale.
#' @param size_name Name/title for the size scale.
#' @param size_oob Out-of-bounds handler for the size scale. Defaults to [scales::oob_censor].
#' @param size_palette Palette for the size scale.
#' @param size_transform Transform for the size scale.
#' @param linewidth_type Scale type for linewidth. One of `"continuous"`, `"discrete"`, or `"binned"`. Auto-detected if `NULL`.
#' @param linewidth_subtype Scale subtype for linewidth. Auto-detected if `NULL`.
#' @param linewidth_breaks Breaks for the linewidth scale.
#' @param linewidth_drop Whether to drop unused levels for a discrete linewidth scale. Defaults to `TRUE`.
#' @param linewidth_guide Guide for the linewidth scale. Defaults to `NULL`.
#' @param linewidth_labels Labels for the linewidth scale.
#' @param linewidth_limits Limits for the linewidth scale.
#' @param linewidth_name Name/title for the linewidth scale.
#' @param linewidth_oob Out-of-bounds handler for the linewidth scale. Defaults to [scales::oob_censor].
#' @param linewidth_palette Palette for the linewidth scale.
#' @param linewidth_transform Transform for the linewidth scale.
#' @param linetype_type Scale type for linetype. Only `"discrete"` is supported.
#' @param linetype_breaks Breaks for the linetype scale.
#' @param linetype_drop Whether to drop unused levels for the linetype scale. Defaults to `TRUE`.
#' @param linetype_guide Guide for the linetype scale. Defaults to `NULL`.
#' @param linetype_labels Labels for the linetype scale.
#' @param linetype_limits Limits for the linetype scale.
#' @param linetype_name Name/title for the linetype scale.
#' @param linetype_palette Palette for the linetype scale.
#' @param shape_type Scale type for shape. Only `"discrete"` is supported.
#' @param shape_breaks Breaks for the shape scale.
#' @param shape_drop Whether to drop unused levels for the shape scale. Defaults to `TRUE`.
#' @param shape_guide Guide for the shape scale. Defaults to `NULL`.
#' @param shape_labels Labels for the shape scale.
#' @param shape_limits Limits for the shape scale.
#' @param shape_name Name/title for the shape scale.
#' @param shape_palette Palette for the shape scale.
#' @param facet_wrap Variables to facet by, passed to [ggplot2::facet_wrap()]. Accepts a bare variable name or [ggplot2::vars()] for multiple variables.
#' @param facet_rows Row variables for [ggplot2::facet_grid()]. Accepts a bare variable name or [ggplot2::vars()] for multiple variables.
#' @param facet_cols Column variables for [ggplot2::facet_grid()]. Accepts a bare variable name or [ggplot2::vars()] for multiple variables.
#' @param facet_axes Which axes to draw on facet panels. Defaults to `"margins"`.
#' @param facet_axis_labels Which axis labels to draw on facet panels. Defaults to `"all"`.
#' @param facet_drop Whether to drop unused factor levels in facets. Defaults to `TRUE`.
#' @param facet_labeller Labeller for facet strip labels. Defaults to `"label_value"`.
#' @param facet_ncol Number of columns for [ggplot2::facet_wrap()].
#' @param facet_nrow Number of rows for [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet scales are fixed or free. Defaults to `"fixed"`.
#' @param facet_space Whether facet space is fixed or free. Defaults to `"fixed"`.
#' @param coord_clip Whether drawing is clipped to the panel. Either `"on"` or `"on"`.
#' @param coord_ratio Aspect ratio expressed as y / x, for [ggplot2::coord_cartesian()].
#' @param coord_reverse Which axes to reverse. One of `"none"` (default), `"x"`, `"y"`, or `"xy"`.
#' @param coord_xlim,coord_ylim Zoom limits within the coordinate system.
#' @param title Plot title passed to [ggplot2::labs()].
#' @param subtitle Plot subtitle passed to [ggplot2::labs()].
#' @param caption Plot caption passed to [ggplot2::labs()].
#' @param ggplot A base ggplot object to use. Defaults to `NULL`, which uses `\(x) ggplot2::ggplot(x)`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' gg_blanket(
#'   data = mtcars,
#'   geom = "point",
#'   x = wt,
#'   y = mpg,
#'   fill = hp,
#' )
#'
#' gg_blanket(
#'   data = mtcars,
#'   geom = "point",
#'   x = wt,
#'   y = mpg,
#'   fill = factor(cyl),
#'   facet_wrap = cyl,
#' )
gg_blanket <- function(
    data,
    ...,
    # geom
    geom = "blank",
    stat = "identity",
    position = ggplot2::position_identity(),

    # gg_blanket specific
    before = NULL,
    with = NULL,


    border = NULL, theme = NULL, refine = NULL,

    # aesthetics
    x = NULL,
    xmin = NULL,
    xmax = NULL,
    xend = NULL,
    xintercept = NULL,
    y = NULL,
    ymin = NULL,
    ymax = NULL,
    yend = NULL,
    yintercept = NULL,
    z = NULL,
    fill = NULL,
    colour = NULL,
    alpha = NULL,
    shape = NULL,
    linetype = NULL,
    linewidth = NULL,
    size = NULL,
    stroke = NULL,
    label = NULL,
    weight = NULL,
    group = NULL,
    width = NULL,
    height = NULL,
    slope = NULL,
    intercept = NULL,
    sample = NULL,
    angle = NULL,
    radius = NULL,
    mapping = ggplot2::aes(),
    # x scale
    x_type = NULL,
    x_subtype = NULL,
    x_breaks = NULL,
    x_drop = TRUE,
    x_expand = NULL,
    x_guide = ggplot2::waiver(),
    x_labels = NULL,
    x_limits = NULL,
    x_minor_breaks = ggplot2::waiver(),
    x_name = ggplot2::waiver(),
    x_oob = scales::oob_censor,
    x_palette = seq_len,
    x_position = "bottom",
    x_sec_axis = ggplot2::waiver(),
    x_transform = NULL,
    # y scale
    y_type = NULL,
    y_subtype = NULL,
    y_breaks = NULL,
    y_drop = TRUE,
    y_expand = NULL,
    y_guide = ggplot2::waiver(),
    y_labels = NULL,
    y_limits = NULL,
    y_minor_breaks = ggplot2::waiver(),
    y_name = ggplot2::waiver(),
    y_oob = scales::oob_censor,
    y_palette = seq_len,
    y_position = "left",
    y_sec_axis = ggplot2::waiver(),
    y_transform = NULL,
    # fill scale
    fill_type = NULL,
    fill_subtype = NULL,
    fill_breaks = ggplot2::waiver(),
    fill_drop = TRUE,
    fill_guide = NULL,
    fill_labels = NULL,
    fill_limits = NULL,
    fill_name = ggplot2::waiver(),
    fill_oob = scales::oob_censor,
    fill_rescaler = scales::rescale,
    fill_palette = NULL,
    fill_transform = NULL,
    # colour scale
    colour_type = NULL,
    colour_subtype = NULL,
    colour_breaks = NULL,
    colour_drop = NULL,
    colour_guide = NULL,
    colour_labels = NULL,
    colour_limits = NULL,
    colour_name = NULL,
    colour_oob = NULL,
    colour_rescaler = NULL,
    colour_palette = NULL,
    colour_transform = NULL,
    # alpha scale
    alpha_type = NULL,
    alpha_subtype = NULL,
    alpha_breaks = ggplot2::waiver(),
    alpha_drop = TRUE,
    alpha_guide = NULL,
    alpha_labels = NULL,
    alpha_limits = NULL,
    alpha_name = ggplot2::waiver(),
    alpha_oob = scales::oob_censor,
    alpha_palette = NULL,
    alpha_transform = NULL,
    # size scale
    size_type = NULL,
    size_subtype = NULL,
    size_breaks = ggplot2::waiver(),
    size_drop = TRUE,
    size_guide = NULL,
    size_labels = NULL,
    size_limits = NULL,
    size_name = ggplot2::waiver(),
    size_oob = scales::oob_censor,
    size_palette = NULL,
    size_transform = NULL,
    # linewidth scale
    linewidth_type = NULL,
    linewidth_subtype = NULL,
    linewidth_breaks = ggplot2::waiver(),
    linewidth_drop = TRUE,
    linewidth_guide = NULL,
    linewidth_labels = NULL,
    linewidth_limits = NULL,
    linewidth_name = ggplot2::waiver(),
    linewidth_oob = scales::oob_censor,
    linewidth_palette = NULL,
    linewidth_transform = NULL,
    # linetype scale
    linetype_type = NULL,
    linetype_breaks = ggplot2::waiver(),
    linetype_drop = TRUE,
    linetype_guide = NULL,
    linetype_labels = NULL,
    linetype_limits = NULL,
    linetype_name = ggplot2::waiver(),
    linetype_palette = NULL,
    # shape scale
    shape_type = NULL,
    shape_breaks = ggplot2::waiver(),
    shape_drop = TRUE,
    shape_guide = NULL,
    shape_labels = NULL,
    shape_limits = NULL,
    shape_name = ggplot2::waiver(),
    shape_palette = NULL,
    # facet
    facet_wrap = NULL,
    facet_rows = NULL,
    facet_cols = NULL,
    facet_axes = "margins",
    facet_axis_labels = "all",
    facet_drop = TRUE,
    facet_labeller = "label_value",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    # coord
    coord_xlim = NULL,
    coord_ylim = NULL,
    coord_clip = NULL,
    coord_reverse = "none",
    coord_ratio = NULL,


    # titles
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    ggplot = NULL
) {

  #set blanket, if not already set
  if (!isTRUE(getOption("ggblanket.initialised"))) {
    set_blanket()
  }

  ### get geom info
  geom_info <- get_geom_info(geom)
  geom_fn   <- geom_info$fn
  geom_str  <- geom_info$str

  ### get options
  current_theme <- theme %||% ggplot2::get_theme()

  refine <- refine %||%
    if (!is.null(ggplot) || geom_str == "sf") {
      void_drop
    } else {
      get_refine() %||% modern_drift
    }

  # Resolve border functions and linewidth from options, with sensible fallbacks
  colour_border_fn <- get_colour_border()

  fill_border_fn <- get_fill_border()

  linewidth_border_default <- get_linewidth_border()

  ### make aesthetics list
  aesthetics <- rlang::enquos(
    x          = x,
    xmin       = xmin,
    xmax       = xmax,
    xend       = xend,
    y          = y,
    ymin       = ymin,
    ymax       = ymax,
    yend       = yend,
    z          = z,
    fill       = fill,
    colour     = colour,
    alpha      = alpha,
    shape      = shape,
    linetype   = linetype,
    linewidth  = linewidth,
    size       = size,
    stroke     = stroke,
    label      = label,
    weight     = weight,
    group      = group,
    width      = width,
    height     = height,
    slope      = slope,
    intercept  = intercept,
    xintercept = xintercept,
    yintercept = yintercept,
    sample     = sample,
    angle      = angle,
    radius     = radius,
    .ignore_empty = "all",
    .ignore_null  = "all"
  )

  ### identify whether aesthetics mapped/fixed
  separated <- separate_fixed_and_mapped_aesthetics(aesthetics, data)

  is_fill_mapped      <- "fill"      %in% names(separated$mapped) || "fill"      %in% names(mapping)
  is_colour_mapped    <- "colour"    %in% names(separated$mapped) || "colour"    %in% names(mapping)
  is_linewidth_mapped <- "linewidth" %in% names(separated$mapped) || "linewidth" %in% names(mapping)

  is_fill_fixed      <- "fill"      %in% names(separated$fixed)
  is_colour_fixed    <- "colour"    %in% names(separated$fixed)

  ### identify if border geom
  is_border_geom      <- geom_info$is_border_geom
  is_colour_border    <- if (is.null(border)) is_border_geom else isTRUE(border)
  is_fill_border      <- if (is.null(border)) is_border_geom else isTRUE(border)
  is_linewidth_border <- if (is.null(border)) is_border_geom else isTRUE(border)

  ### ensure colour is inherited from fill
  if (is_fill_mapped & !is_colour_mapped & !is_colour_fixed) {
    fill_aesthetic          <- separated$mapped$fill %||% mapping$fill
    separated$mapped$colour <- fill_aesthetic
    is_colour_mapped        <- TRUE
  }

  # Check if colour and fill are mapped to the same variable
  colour_fill_same <- FALSE
  if (is_colour_mapped && is_fill_mapped) {
    fill_var         <- rlang::as_label(separated$mapped$fill   %||% mapping$fill)
    colour_var       <- rlang::as_label(separated$mapped$colour %||% mapping$colour)
    colour_fill_same <- identical(fill_var, colour_var)
  }

  ### compute fixed colour — apply colour_border_fn if border geom
  computed_colour <- NULL
  if (!is_colour_mapped) {
    computed_colour <- separated$fixed[["colour"]] %||%
      separated$fixed[["fill"]] %||%
      current_theme$geom@fill
    if (is_colour_border && !is.null(computed_colour)) {
      computed_colour <- colour_border_fn(computed_colour)
    }
  }

  ### compute fixed linewidth — use border default if border geom
  computed_linewidth <- NULL
  if (!is_linewidth_mapped) {
    if (is_linewidth_border) {
      computed_linewidth <- separated$fixed[["linewidth"]] %||%
        linewidth_border_default
    } else {
      computed_linewidth <- separated$fixed[["linewidth"]] %||%
        current_theme$geom@linewidth
    }
  }

  is_stroke_mapped <- "stroke" %in% names(separated$mapped) || "stroke" %in% names(mapping)
  is_stroke_fixed  <- "stroke" %in% names(separated$fixed)

  computed_stroke <- NULL
  if (!is_stroke_mapped && !is_stroke_fixed && is_colour_border && geom_info$is_stroke_geom) {
    computed_stroke <- get_stroke()
  }

  # Remove colour and linewidth from separated$fixed as they are now computed
  separated$fixed <- separated$fixed[
    !names(separated$fixed) %in% c("colour", "linewidth")
  ]

  # Combine fixed aesthetic values with additional parameters
  all_params <- utils::modifyList(
    separated$fixed,
    rlang::dots_list(..., .ignore_empty = "all")
  )

  # Add colour and linewidth back as params if computed
  if (!is.null(computed_colour))    all_params$colour    <- computed_colour
  if (!is.null(computed_linewidth)) all_params$linewidth <- computed_linewidth
  if (!is.null(computed_stroke)) all_params$stroke <- computed_stroke

  ### compute fixed fill — apply fill_border_fn if border geom and fill not user-specified
  if (!is_fill_mapped && !is_fill_fixed) {
    computed_fill <- current_theme$geom@fill
    if (is_fill_border && !is.null(computed_fill)) {
      computed_fill <- fill_border_fn(computed_fill)
    }
    if (!is.null(computed_fill)) all_params$fill <- computed_fill
  }

  # Combine individual aesthetics with mapping argument
  final_mapping <- combine_aesthetics(separated$mapped, mapping)

  ### build base plot
  plot <- if (is.null(ggplot)) {
    ggplot2::ggplot(data, mapping = final_mapping)
  } else if (is.function(ggplot)) {
    ggplot(data)
  } else {
    ggplot
  }

  plot <- plot + current_theme

  ### before
  if (!is.null(before)) {
    fixed_before_colour <- separated$fixed[["colour"]] %||%
      separated$fixed[["fill"]]
    if (!is.null(fixed_before_colour)) {
      plot <- plot +
        ggplot2::theme(
          geom = ggplot2::element_geom(colour = fixed_before_colour)
        )
    }
    plot <- plot + before

    before_palette <- colour_palette %||%
      fill_palette %||%
      current_theme$palette.fill.discrete %||%
      current_theme$palette.colour.discrete

    plot <- plot +
      ggplot2::discrete_scale(
        aesthetics = "colour",
        palette = if (!is.null(before_palette)) {
          as_discrete_palette(before_palette)
        } else {
          NULL
        },
        breaks = colour_breaks %||% fill_breaks,
        drop   = colour_drop   %||% fill_drop,
        guide = if (colour_fill_same && is.null(colour_guide)) {
          ggplot2::guide_none()
        } else {
          colour_guide %||% fill_guide %||% ggplot2::guide_legend()
        },
        labels = colour_labels %||% fill_labels %||% ggplot2::waiver(),
        limits = colour_limits %||% fill_limits,
        name   = colour_name   %||% fill_name
      )
    plot <- plot + ggnewscale::new_scale_colour()
  }

  ### layer
  layer_data <- if (is.data.frame(data)) data else NULL

  layer_args <- c(
    list(position = position),
    if (!is.null(ggplot) && geom_str == "sf") list(inherit.aes = FALSE),
    if (!is.null(layer_data)) list(data = layer_data),
    if (!is.null(final_mapping)) list(mapping = final_mapping),
    if (!is.null(stat)) list(stat = stat),
    all_params
  )

  if (!is.null(with)) {
    plot <- plot + rlang::exec(geom_fn, !!!layer_args) |> with()
  } else {
    plot <- plot + rlang::exec(geom_fn, !!!layer_args)
  }

  ### facet
  facet_wrap_quo <- rlang::enquo(facet_wrap)
  facet_rows_quo <- rlang::enquo(facet_rows)
  facet_cols_quo <- rlang::enquo(facet_cols)

  facet_wrap <- if (rlang::quo_is_null(facet_wrap_quo)) {
    NULL
  } else if (inherits(rlang::quo_get_expr(facet_wrap_quo), "quosures")) {
    rlang::quo_get_expr(facet_wrap_quo)
  } else if (rlang::is_call(rlang::quo_get_expr(facet_wrap_quo), "vars")) {
    rlang::eval_tidy(facet_wrap_quo)
  } else {
    ggplot2::vars(!!facet_wrap_quo)
  }

  facet_rows <- if (rlang::quo_is_null(facet_rows_quo)) {
    NULL
  } else if (inherits(rlang::quo_get_expr(facet_rows_quo), "quosures")) {
    rlang::quo_get_expr(facet_rows_quo)
  } else if (rlang::is_call(rlang::quo_get_expr(facet_rows_quo), "vars")) {
    rlang::eval_tidy(facet_rows_quo)
  } else {
    ggplot2::vars(!!facet_rows_quo)
  }

  facet_cols <- if (rlang::quo_is_null(facet_cols_quo)) {
    NULL
  } else if (inherits(rlang::quo_get_expr(facet_cols_quo), "quosures")) {
    rlang::quo_get_expr(facet_cols_quo)
  } else if (rlang::is_call(rlang::quo_get_expr(facet_cols_quo), "vars")) {
    rlang::eval_tidy(facet_cols_quo)
  } else {
    ggplot2::vars(!!facet_cols_quo)
  }

  if (!is.null(facet_wrap)) {
    plot <- plot +
      ggplot2::facet_wrap(
        facets      = facet_wrap,
        nrow        = facet_nrow,
        ncol        = facet_ncol,
        scales      = facet_scales,
        space       = facet_space,
        labeller    = facet_labeller,
        drop        = facet_drop,
        axes        = facet_axes,
        axis.labels = facet_axis_labels
      )
  } else if (!is.null(facet_rows) | !is.null(facet_cols)) {
    plot <- plot +
      ggplot2::facet_grid(
        rows        = facet_rows,
        cols        = facet_cols,
        scales      = facet_scales,
        space       = facet_space,
        labeller    = facet_labeller,
        drop        = facet_drop,
        axes        = facet_axes,
        axis.labels = facet_axis_labels
      )
  }

  # Applied early so ggplot_build() sees user-specified limits when computing scale_info
  if (!is.null(x_limits)) {
    if (is.function(x_limits)) {
      plot <- plot + ggplot2::scale_x_continuous(limits = x_limits)
    } else {
      plot <- plot + ggplot2::xlim(x_limits)
    }
  }

  if (!is.null(y_limits)) {
    if (is.function(y_limits)) {
      plot <- plot + ggplot2::scale_y_continuous(limits = y_limits)
    } else {
      plot <- plot + ggplot2::ylim(y_limits)
    }
  }

  ### identify scales and orientation
  built  <- ggplot2::ggplot_build(plot)
  nrows  <- length(unique(built$layout$layout$ROW))
  ncols  <- length(unique(built$layout$layout$COL))

  scale_info <- identify_scale(built)

  coord_type <- stringr::str_to_lower(stringr::str_remove(
    class(built@layout$coord)[1],
    "Coord"
  ))

  x_type         <- x_type         %||% scale_info$x$type
  y_type         <- y_type         %||% scale_info$y$type
  fill_type      <- fill_type      %||% scale_info$fill$type
  colour_type    <- colour_type    %||% fill_type %||% scale_info$colour$type
  alpha_type     <- alpha_type     %||% scale_info$alpha$type
  size_type      <- size_type      %||% scale_info$size$type
  linewidth_type <- linewidth_type %||% scale_info$linewidth$type
  linetype_type  <- linetype_type  %||% scale_info$linetype$type
  shape_type     <- shape_type     %||% scale_info$shape$type

  x_subtype         <- x_subtype         %||% scale_info$x$subtype
  y_subtype         <- y_subtype         %||% scale_info$y$subtype
  fill_subtype      <- fill_subtype      %||% scale_info$fill$subtype
  colour_subtype    <- colour_subtype    %||% fill_subtype %||% scale_info$colour$subtype
  alpha_subtype     <- alpha_subtype     %||% scale_info$alpha$subtype
  size_subtype      <- size_subtype      %||% scale_info$size$subtype
  linewidth_subtype <- linewidth_subtype %||% scale_info$linewidth$subtype

  is_x_mapped <- "x" %in% names(separated$mapped) || "x" %in% names(mapping)
  is_y_mapped <- "y" %in% names(separated$mapped) || "y" %in% names(mapping)

  orientation <- get_orientation(
    x_type      = x_type,
    y_type      = y_type,
    built       = built,
    is_x_mapped = is_x_mapped,
    is_y_mapped = is_y_mapped
  )

  ### x scale
  if (x_type == "discrete") {
    plot <- plot +
      ggplot2::scale_x_discrete(
        breaks       = x_breaks %||% ggplot2::waiver(),
        minor_breaks = x_minor_breaks %||% ggplot2::waiver(),
        drop         = x_drop,
        expand       = x_expand %||% ggplot2::waiver(),
        guide        = x_guide,
        labels       = x_labels %||% ggplot2::waiver(),
        limits       = x_limits,
        name         = x_name,
        palette      = x_palette,
        position     = x_position,
        sec.axis     = x_sec_axis
      )
  } else if (x_type == "continuous") {
    plot <- plot +
      ggplot2::scale_x_continuous(
        breaks       = x_breaks %||%
          if (!is.na(x_subtype)) {
            if (ncols == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
          } else {
            if (ncols == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
          },
        minor_breaks = x_minor_breaks %||% ggplot2::waiver(),
        expand       = x_expand %||% get_expand(scale_info$x$limits),
        guide        = x_guide,
        labels       = x_labels %||% get_labels(coord_type, x_subtype, "x"),
        limits       = x_limits,
        name         = x_name,
        oob          = x_oob,
        position     = x_position,
        sec.axis     = x_sec_axis,
        transform    = x_transform %||% get_transform(x_subtype)
      )
  } else if (x_type == "binned") {
    plot <- plot +
      ggplot2::scale_x_binned(
        breaks    = x_breaks %||%
          if (!is.na(x_subtype)) {
            if (ncols == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
          } else {
            if (ncols == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
          },
        expand    = x_expand %||% get_expand(scale_info$x$limits),
        guide     = x_guide,
        labels    = x_labels %||% get_labels(coord_type, x_subtype, "x"),
        limits    = x_limits,
        name      = x_name,
        oob       = x_oob,
        position  = x_position,
        transform = x_transform %||% get_transform(x_subtype)
      )
  }

  ### y scale
  if (y_type == "discrete") {
    plot <- plot +
      ggplot2::scale_y_discrete(
        breaks       = y_breaks %||% ggplot2::waiver(),
        minor_breaks = y_minor_breaks %||% ggplot2::waiver(),
        drop         = y_drop,
        expand       = y_expand %||% ggplot2::waiver(),
        guide        = y_guide,
        labels       = y_labels %||% ggplot2::waiver(),
        limits       = y_limits,
        name         = y_name,
        palette      = y_palette,
        position     = y_position,
        sec.axis     = y_sec_axis
      )
  } else if (y_type == "continuous") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        breaks       = y_breaks %||%
          if (!is.na(y_subtype)) {
            if (nrows == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
          } else {
            if (nrows == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
          },
        minor_breaks = y_minor_breaks %||% ggplot2::waiver(),
        expand       = y_expand %||% get_expand(scale_info$y$limits),
        guide        = y_guide,
        labels       = y_labels %||% get_labels(coord_type, y_subtype, "y"),
        limits       = y_limits,
        name         = y_name,
        oob          = y_oob,
        position     = y_position,
        sec.axis     = y_sec_axis,
        transform    = y_transform %||% get_transform(y_subtype)
      )
  } else if (y_type == "binned") {
    plot <- plot +
      ggplot2::scale_y_binned(
        breaks    = y_breaks %||%
          if (!is.na(y_subtype)) {
            if (nrows == 1) scales::breaks_pretty(n = 5) else scales::breaks_pretty(n = 4)
          } else {
            if (nrows == 1) scales::breaks_extended(n = 5) else scales::breaks_extended(n = 4)
          },
        expand    = y_expand %||% get_expand(scale_info$y$limits),
        guide     = y_guide,
        labels    = y_labels %||% get_labels(coord_type, y_subtype, "y"),
        limits    = y_limits,
        name      = y_name,
        oob       = y_oob,
        position  = y_position,
        transform = y_transform %||% get_transform(y_subtype)
      )
  }

  # Resolve scale NA and override values, applying border functions if border geom
  fill_na       <- jumble::grey
  fill_override <- jumble::slate

  colour_na       <- if (is_colour_border) colour_border_fn(fill_na)      else fill_na
  colour_override <- if (is_colour_border) colour_border_fn(fill_override) else fill_override

  if (is_fill_border) {
    fill_na       <- fill_border_fn(fill_na)
    fill_override <- fill_border_fn(fill_override)
  }

  ### fill scale
  if (!is.null(fill_type)) {
    if (fill_type == "discrete") {
      fill_palette <- fill_palette %||%
        current_theme$palette.fill.discrete %||%
        scales::pal_hue()

      if (is_named_palette(fill_palette)) {
        fill_values <- if (is_fill_border) {
          stats::setNames(fill_border_fn(fill_palette), names(fill_palette))
        } else {
          fill_palette
        }
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "fill",
            values   = fill_values,
            breaks   = fill_breaks %||% ggplot2::waiver(),
            drop     = fill_drop,
            guide    = fill_guide %||% ggplot2::guide_legend(),
            labels   = fill_labels %||% ggplot2::waiver(),
            limits   = fill_limits,
            name     = fill_name,
            na.value = fill_na
          )
      } else {
        fill_palette_scaled <- if (is_fill_border) {
          apply_border_to_palette(fill_palette, fill_border_fn)
        } else {
          fill_palette
        }
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "fill",
            palette    = as_discrete_palette(fill_palette_scaled),
            breaks     = fill_breaks %||% ggplot2::waiver(),
            drop       = fill_drop,
            guide      = fill_guide %||% ggplot2::guide_legend(),
            labels     = fill_labels %||% ggplot2::waiver(),
            limits     = fill_limits,
            name       = fill_name,
            na.value   = fill_na
          )
      }
    } else if (fill_type %in% c("continuous", "binned")) {
      fill_palette <- fill_palette %||%
        current_theme$palette.fill.continuous %||%
        scales::pal_gradient_n(viridis::turbo(n = 256))
      fill_palette_scaled <- if (is_fill_border) {
        apply_border_to_palette(fill_palette, fill_border_fn)
      } else {
        fill_palette
      }

      if (fill_type == "continuous") {
        plot <- plot +
          ggplot2::continuous_scale(
            aesthetics = "fill",
            palette    = as_continuous_palette(fill_palette_scaled),
            breaks     = fill_breaks %||% ggplot2::waiver(),
            guide      = fill_guide %||% ggplot2::guide_colorbar(),
            labels     = fill_labels %||% get_labels(coord_type, fill_subtype, "fill"),
            limits     = fill_limits,
            name       = fill_name,
            oob        = fill_oob,
            rescaler   = fill_rescaler,
            transform  = fill_transform %||% get_transform(fill_subtype),
            na.value   = fill_na
          )
      } else if (fill_type == "binned") {
        plot <- plot +
          ggplot2::binned_scale(
            aesthetics = "fill",
            palette    = as_continuous_palette(fill_palette_scaled),
            breaks     = fill_breaks %||% ggplot2::waiver(),
            guide      = fill_guide %||% ggplot2::guide_bins(),
            labels     = fill_labels %||% get_labels(coord_type, fill_subtype, "fill"),
            limits     = fill_limits,
            name       = fill_name,
            oob        = fill_oob,
            rescaler   = fill_rescaler,
            transform  = fill_transform %||% get_transform(fill_subtype),
            na.value   = fill_na
          )
      }
    }

    plot <- plot +
      ggplot2::theme(geom = ggplot2::element_geom(fill = fill_override))
  }

  ### colour scale
  if (!is.null(colour_type)) {
    if (colour_type == "discrete") {
      if (is_colour_border) {
        colour_palette <- colour_palette %||%
          (if (!is.null(fill_palette)) apply_border_to_palette(fill_palette, colour_border_fn)) %||%
          apply_border_to_palette(scales::pal_hue(), colour_border_fn)
      } else {
        colour_palette <- colour_palette %||%
          fill_palette %||%
          current_theme$palette.fill.discrete %||%
          scales::pal_hue()
      }

      if (is_named_palette(colour_palette)) {
        colour_values <- if (is.null(names(colour_palette)) && is_named_palette(fill_palette)) {
          stats::setNames(colour_palette, names(fill_palette))
        } else {
          colour_palette
        }
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "colour",
            values   = colour_values,
            breaks   = colour_breaks %||% fill_breaks,
            drop     = colour_drop   %||% fill_drop,
            guide    = colour_guide  %||% fill_guide %||% ggplot2::guide_legend(),
            labels   = colour_labels %||% fill_labels %||% ggplot2::waiver(),
            limits   = colour_limits %||% fill_limits,
            name     = colour_name   %||% fill_name,
            na.value = colour_na
          )
      } else {
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "colour",
            palette    = as_discrete_palette(colour_palette),
            breaks     = colour_breaks %||% fill_breaks,
            drop       = colour_drop   %||% fill_drop,
            guide      = colour_guide  %||% fill_guide %||% ggplot2::guide_legend(),
            labels     = colour_labels %||% fill_labels %||% ggplot2::waiver(),
            limits     = colour_limits %||% fill_limits,
            name       = colour_name   %||% fill_name,
            na.value   = colour_na
          )
      }
    } else if (colour_type %in% c("continuous", "binned")) {
      if (is_colour_border) {
        colour_palette <- colour_palette %||%
          apply_border_to_palette(fill_palette, colour_border_fn) %||%
          apply_border_to_palette(current_theme$palette.fill.continuous, colour_border_fn) %||%
          apply_border_to_palette(scales::pal_gradient_n(viridis::turbo(n = 256)), colour_border_fn)
      } else {
        colour_palette <- colour_palette %||%
          fill_palette %||%
          current_theme$palette.fill.continuous %||%
          scales::pal_gradient_n(viridis::turbo(n = 256))
      }

      if (colour_type == "continuous") {
        plot <- plot +
          ggplot2::continuous_scale(
            aesthetics = "colour",
            palette    = as_continuous_palette(colour_palette),
            breaks     = colour_breaks %||% fill_breaks,
            guide = if (colour_fill_same && is.null(colour_guide)) {
              ggplot2::guide_none()
            } else {
              colour_guide %||% fill_guide %||% ggplot2::guide_colourbar()
            },
            labels     = colour_labels %||% fill_labels %||%
              get_labels(coord_type, colour_subtype, "colour"),
            limits     = colour_limits %||% fill_limits,
            name       = colour_name   %||% fill_name,
            oob        = colour_oob    %||% fill_oob,
            rescaler   = colour_rescaler %||% fill_rescaler,
            transform  = colour_transform %||% get_transform(colour_subtype),
            na.value   = colour_na
          )
      } else if (colour_type == "binned") {
        plot <- plot +
          ggplot2::binned_scale(
            aesthetics = "colour",
            palette    = as_continuous_palette(colour_palette),
            breaks     = colour_breaks %||% fill_breaks,
            guide = if (colour_fill_same && is.null(colour_guide)) {
              ggplot2::guide_none()
            } else {
              colour_guide %||% fill_guide %||% ggplot2::guide_bins()
            },
            labels     = colour_labels %||% fill_labels %||%
              get_labels(coord_type, colour_subtype, "colour"),
            limits     = colour_limits %||% fill_limits,
            name       = colour_name   %||% fill_name,
            oob        = colour_oob    %||% fill_oob,
            rescaler   = colour_rescaler %||% fill_rescaler,
            transform  = colour_transform %||% get_transform(colour_subtype),
            na.value   = colour_na
          )
      }
    }

    plot <- plot +
      ggplot2::theme(geom = ggplot2::element_geom(colour = colour_override))
  }

  # Add alpha scale
  if (!is.null(alpha_type)) {
    if (alpha_type == "discrete") {
      if (is_named_palette(alpha_palette)) {
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "alpha",
            values     = alpha_palette,
            breaks     = alpha_breaks,
            drop       = alpha_drop,
            guide      = alpha_guide %||% ggplot2::guide_legend(),
            labels     = alpha_labels %||% ggplot2::waiver(),
            limits     = alpha_limits,
            name       = alpha_name
          )
      } else {
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "alpha",
            palette    = as_discrete_palette(alpha_palette),
            breaks     = alpha_breaks,
            drop       = alpha_drop,
            guide      = alpha_guide %||% ggplot2::guide_legend(),
            labels     = alpha_labels %||% ggplot2::waiver(),
            limits     = alpha_limits,
            name       = alpha_name
          )
      }
    } else if (alpha_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "alpha",
          palette    = as_continuous_palette(alpha_palette),
          breaks     = alpha_breaks,
          guide      = alpha_guide %||% ggplot2::guide_legend(),
          labels     = alpha_labels %||% get_labels(coord_type, alpha_subtype, "alpha"),
          limits     = alpha_limits,
          name       = alpha_name,
          oob        = alpha_oob,
          transform  = alpha_transform %||% get_transform(alpha_subtype)
        )
    } else if (alpha_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "alpha",
          palette    = as_continuous_palette(alpha_palette),
          breaks     = alpha_breaks,
          guide      = alpha_guide %||% ggplot2::guide_bins(),
          labels     = alpha_labels %||% get_labels(coord_type, alpha_subtype, "alpha"),
          limits     = alpha_limits,
          name       = alpha_name,
          oob        = alpha_oob,
          transform  = alpha_transform %||% get_transform(alpha_subtype)
        )
    }
  }

  # Add size scale
  if (!is.null(size_type)) {
    if (size_type == "discrete") {
      if (is_named_palette(size_palette)) {
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "size",
            values     = size_palette,
            breaks     = size_breaks,
            drop       = size_drop,
            guide      = size_guide %||% ggplot2::guide_legend(),
            labels     = size_labels %||% ggplot2::waiver(),
            limits     = size_limits,
            name       = size_name
          )
      } else {
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "size",
            palette    = as_discrete_palette(size_palette),
            breaks     = size_breaks,
            drop       = size_drop,
            guide      = size_guide %||% ggplot2::guide_legend(),
            labels     = size_labels %||% ggplot2::waiver(),
            limits     = size_limits,
            name       = size_name
          )
      }
    } else if (size_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "size",
          palette    = as_continuous_palette(size_palette),
          breaks     = size_breaks,
          guide      = size_guide %||% ggplot2::guide_legend(),
          labels     = size_labels %||% get_labels(coord_type, size_subtype, "size"),
          limits     = size_limits,
          name       = size_name,
          oob        = size_oob,
          transform  = size_transform %||% get_transform(size_subtype)
        )
    } else if (size_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "size",
          palette    = as_continuous_palette(size_palette),
          breaks     = size_breaks,
          guide      = size_guide %||% ggplot2::guide_bins(),
          labels     = size_labels %||% get_labels(coord_type, size_subtype, "size"),
          limits     = size_limits,
          name       = size_name,
          oob        = size_oob,
          transform  = size_transform %||% get_transform(size_subtype)
        )
    }
  }

  # Add linewidth scale
  if (!is.null(linewidth_type)) {
    if (linewidth_type == "discrete") {
      if (is_named_palette(linewidth_palette)) {
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "linewidth",
            values     = linewidth_palette,
            breaks     = linewidth_breaks,
            drop       = linewidth_drop,
            guide      = linewidth_guide %||% ggplot2::guide_legend(),
            labels     = linewidth_labels %||% ggplot2::waiver(),
            limits     = linewidth_limits,
            name       = linewidth_name
          )
      } else {
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "linewidth",
            palette    = as_discrete_palette(linewidth_palette),
            breaks     = linewidth_breaks,
            drop       = linewidth_drop,
            guide      = linewidth_guide %||% ggplot2::guide_legend(),
            labels     = linewidth_labels %||% ggplot2::waiver(),
            limits     = linewidth_limits,
            name       = linewidth_name
          )
      }
    } else if (linewidth_type == "continuous") {
      plot <- plot +
        ggplot2::continuous_scale(
          aesthetics = "linewidth",
          palette    = as_continuous_palette(linewidth_palette),
          breaks     = linewidth_breaks,
          guide      = linewidth_guide %||% ggplot2::guide_legend(),
          labels     = linewidth_labels %||% get_labels(coord_type, linewidth_subtype, "linewidth"),
          limits     = linewidth_limits,
          name       = linewidth_name,
          oob        = linewidth_oob,
          transform  = linewidth_transform %||% get_transform(linewidth_subtype)
        )
    } else if (linewidth_type == "binned") {
      plot <- plot +
        ggplot2::binned_scale(
          aesthetics = "linewidth",
          palette    = as_continuous_palette(linewidth_palette),
          breaks     = linewidth_breaks,
          guide      = linewidth_guide %||% ggplot2::guide_bins(),
          labels     = linewidth_labels %||% get_labels(coord_type, linewidth_subtype, "linewidth"),
          limits     = linewidth_limits,
          name       = linewidth_name,
          oob        = linewidth_oob,
          transform  = linewidth_transform %||% get_transform(linewidth_subtype)
        )
    }
  }

  # Add linetype scale
  if (!is.null(linetype_type)) {
    if (linetype_type == "discrete") {
      if (is_named_palette(linetype_palette)) {
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "linetype",
            values     = linetype_palette,
            breaks     = linetype_breaks,
            drop       = linetype_drop,
            guide      = linetype_guide %||% ggplot2::guide_legend(),
            labels     = linetype_labels %||% ggplot2::waiver(),
            limits     = linetype_limits,
            name       = linetype_name
          )
      } else {
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "linetype",
            palette    = as_discrete_palette(linetype_palette),
            breaks     = linetype_breaks,
            drop       = linetype_drop,
            guide      = linetype_guide %||% ggplot2::guide_legend(),
            labels     = linetype_labels %||% ggplot2::waiver(),
            limits     = linetype_limits,
            name       = linetype_name
          )
      }
    }
  }

  # Add shape scale
  if (!is.null(shape_type)) {
    if (shape_type == "discrete") {
      if (is_named_palette(shape_palette)) {
        plot <- plot +
          ggplot2::scale_discrete_manual(
            aesthetics = "shape",
            values     = shape_palette,
            breaks     = shape_breaks,
            drop       = shape_drop,
            guide      = shape_guide %||% ggplot2::guide_legend(),
            labels     = shape_labels %||% ggplot2::waiver(),
            limits     = shape_limits,
            name       = shape_name
          )
      } else {
        plot <- plot +
          ggplot2::discrete_scale(
            aesthetics = "shape",
            palette    = as_discrete_palette(shape_palette),
            breaks     = shape_breaks,
            drop       = shape_drop,
            guide      = shape_guide %||% ggplot2::guide_legend(),
            labels     = shape_labels %||% ggplot2::waiver(),
            limits     = shape_limits,
            name       = shape_name
          )
      }
    }
  }

  ### titles
  if (!is.null(title))    plot <- plot + ggplot2::labs(title    = title)
  if (!is.null(subtitle)) plot <- plot + ggplot2::labs(subtitle = subtitle)
  if (!is.null(caption))  plot <- plot + ggplot2::labs(caption  = caption)

  ### coord
  plot <- plot +
    get_coord(
      coord_type        = coord_type,
      coord_xlim      = coord_xlim,
      coord_ylim      = coord_ylim,
      coord_clip = coord_clip %||% get_coord_clip() %||% "on",
      coord_reverse     = coord_reverse,
      coord_ratio       = coord_ratio
    )

  if (x_type == "discrete" & y_type == "discrete") discrete <- "both"
  else if (x_type == "discrete" & y_type != "discrete") discrete <- "x"
  else if (x_type != "discrete" & y_type == "discrete") discrete <- "y"
  else if (x_type != "discrete" & y_type != "discrete") discrete <- "none"

  ### theme refine
  plot <- plot +
    refine(discrete = discrete, orientation = orientation)

  return(plot)
}

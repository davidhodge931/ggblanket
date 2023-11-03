# ggblanket 5.1.0900

* Removed some unnecessary messages.
* Changed default continuous `pal` to `viridisLite::mako`.
* Fixed bug where `col_breaks` was not working for a numeric `col`.
* Fixed bug where `str_keep_seq` was not working for date, datetime or time class.
* Fixed bug so that time class variables work.
* Improved default positional breaks.
* Ensured positional breaks take expand into account.
* Updated `gg_qq`, so it includes a `geom_qq_line` layer, has nicer `x_title`, `y_title` and `coord` defaults. 
* Added more positional scale arguments to `gg_sf` (i.e. `*_limits`, `*_expand`, `*_breaks`, and `*_labels`).

# ggblanket 5.1.0

* Fixed bug where `col` variable was reversing for non-flipped.
* Added `pal_light_mode` and `pal_dark_mode`.

# ggblanket 5.0.0

* Added `mapping` argument to allow extra aesthetics to be included, such as `shape` (or use delayed evaluation for aesthetics other than `col`, `colour`, `fill` or `alpha`).
* Added `stat` argument back for flexibility.
* Added `linetype_title`, `shape_title`, and `size_title` arguments.
* Added `str_keep_seq` helper function to support keeping labels in a sequence.
* Breaking: made horizontal y characters and factors plot values from low at bottom to high at top.  
* Breaking: removed `lower`, `middle`, `upper`, `xlower`, `xmiddle`, and `xupper` arguments from `gg_blank`.
* Updated `*_breaks` defaults.
* Made plot order logical variables with `TRUE` first.  
* Added `pal_discrete2` with 6 colour blind safe colours for a light background theme. 

# ggblanket 4.0.0

* Breaking: removed the `stat` argument for all `gg_*` functions except `gg_blank`.
* Breaking: renamed `pal_hue` to `pal_discrete` and made colours relatively red/green colour blind safe.
* Breaking: renamed `gg_bin2d` to `gg_bin_2d`.
* Breaking: changed `*_oob` default back to `scales::oob_keep`.
* Improved scales to work better with transformations, limits, breaks etc.
* Added `gg_contour`, `gg_contour_filled`, `gg_density2d`, `gg_density2d_filled` functions.
* Made default to keep unused factors across all scales and facets.
* Made default continuous label function to drop trailing zeros.
* Made default legend place simpler: bottom, unless continuous. Otherwise right.
* Added `text` aesthetic in for `plotly::ggplotly`. 

# ggblanket 3.0.0

* New theme functions: `light_mode` and `dark_mode`.
* Breaking: removed `gg_theme`. 
* Breaking: renamed `*_grid` arguments to `*_gridlines`.
* Breaking: removed `void` argument.
* Breaking: removed `gg_function`.
* Breaking: made `col_rescale` require a function (e.g. `scales::rescale()`).
* Added `facet_switch` argument.
* Made `col_legend_place = "none"` only remove the col legend.  
* Made `gg_bin2d` and `gg_hex` scales calculate in the same way as other functions.
* For raster, removed col aesthetic so that legend would work.
* Improved `gg_sf` default `alpha`.
* Supported `gg_sf` to work with non-standard named geometry. 

# ggblanket 2.0.0

* Made ggblanket work better with adding layers.
* Made the `pal` no longer inherit to layers where no `col` aesthetic.
* Made x and y scales work better, including with NA in the limits.
* Added 2 colours to `pal_hue`.
* Breaking: changed default `*oob` argument to `scales::oob_censor`.
* Breaking: changed default `coord` argument to include `clip = "off"` argument.
* Breaking: removed `add_tooltip` function.
* Breaking: changed pal functions to `pal_blue`, `pal_grey`, and `pal_hue`.

# ggblanket 1.8.0

* Updated default colour palette.
* Updated default NA colour. 

# ggblanket 1.7.0

* Updated `gg_theme` default axis-line and gridline thickness.
* Supported named `pal` vectors.
* Removed `clip` argument.
* Added in `coord` argument.
* Added in `*_oob` arguments.
* Improved how `*_limits` works.
* Removed default gridlines for `gg_sf`.
* Fixed bug with default `x_expand`.
* Fixed bug with `gg_histogram` scales.
* Critical fix to support dplyr 1.1.0.
* Supported colouring date, datetime and time variables.
* Made datetime breaks default to waiver.
* Breaking: removed `void` argument from `gg_theme`.
* Corrected bug with `gg_raster` legend not showing.
* Updated `gg_raster` `*_limits` and `*_expand` defaults.

# ggblanket 1.6.0

* Breaking: renamed `add_tooltip_text` to `add_tooltip` and changed column name to "tooltip".
* Breaking: `coord` argument removed.
* Added `clip` argument and defaulted to "on".
* Made `x` and `y` limits also act within the coord. 
* Made `gg_blank` more powerful and flexible.
* Improved horizontal flipped plot scales. 
* Fixed bugs relating to free `facet_scales` and scale limits.
* Improved vignette.

# ggblanket 1.5.0

* Breaking: redesigned `gg_theme`.
* Added automatic gridline removal.
* Added `gg_bin2d` function.
* Added `gg_hex` function.
* Added `col_trans` and `col_rescale` arguments.
* Made `col_legend_place` default "r" where numeric col variable. 
* Made `*_title = ""` equivalent to `+ labs(* = NULL)`.
* Supported `x` and `y` datetime variables.
* Supported `x` and `y` time variables.
* Redesigned internal code for `x`, `y` and `col` scales.

# ggblanket 1.4.0

* Breaking: Removed `col_intervals` argument.
* Breaking: Removed `*_oob` argument.
* Breaking: In `gg_theme`, changed `*_style` arguments to `*_face`.
* Breaking: In `gg_theme`, changed `font` arguments to `family`.
* Added `gg_polygon` function.
* Added `col_continuous` argument to support colouring by "steps". 
* Added `col_legend_rev` argument to reverse legends.
* Added `facet_layout` argument for more faceting flexibility.
* Added `facet_space` argument to support proportional facet panels.
* Removed `width` default.
* Improved default legend look for where continuous gradient.
* Improved scales where `y` is NULL and plot is horizontal.
* Fixed bug with `position = "fill"`.
* Updated messages.
* Added vignette.

# ggblanket 1.3.0

* Breaking: removed `facet_intervals` argument.
* Breaking: made extensive changes to `gg_theme` function.
* Added `facet2` aesthetic in to support effortless grid faceting.
* Changed default background theme colours.
* Fixed bug with `*_limits` and `*_include` not working correctly.  
* Added `*_sec_axis` arguments.
* Changed `*_oob` default to `scales::oob_keep`.
* Removed reversal of logical variable order.
* Defaulted `col_legend_place` to `"b"`.  

# ggblanket 1.2.0

* Breaking: Removed `*_breaks_n` and `*_breaks_width`.
* Added support for `*_breaks` to receive a function.
* Added `*_include` argument. 
* Added `*_trans` argument.
* Modified default `x_breaks` behaviour for when both x and y are numeric or date.
* Enhanced `gg_blank` to work with only one x or y.
* Provided support for `ggplotly`.
* Added new `add_tooltip_text` function.
* Added new `titles` argument to apply a function across titles.
* Support `gg_` functions working with more stats.
* Removed `size` argument, in anticipation of ggplot2 3.4.0.
* Updated `gg_theme` title vjust and margins.

# ggblanket 1.1.0

* Remove default sentence case transformation of categorical variable labels.
* Updated scales to work in a consistent way with `position = "fill"`.

# ggblanket 1.0.0

* Initial release.

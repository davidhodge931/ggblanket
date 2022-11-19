# ggblanket 1.6.1

* Breaking: removed `void` argument from `gg_theme`.
* Added `void` argument to `gg_sf`, `gg_raster` and `gg_blank`.
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
* Added `facet2` aesthetic in to support effortless grid facetting.
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

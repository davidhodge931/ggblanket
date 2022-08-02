# ggblanket 1.2.8

* Added *_sec_axis arguments.

# ggblanket 1.2.7

* Added a bg_legend_pal argument to gg_theme.
* Changed default background theme colours.

# ggblanket 1.2.6

* Changed *_oob default to scales::oob_keep.

# ggblanket 1.2.5

* gg_theme updated for seperate agruments for background pal.
* Breaking: changed gg_theme y_grid to grid_h and x_grid to grid_v.

# ggblanket 1.2.4

* Breaking: gg_theme rearranged the prefixes of arguments.
* Improved the position of vertical facet labels.

# ggblanket 1.2.3

* Added `facet2` aesthetic in to support effortless grid facetting.
* Removed reversal of logical variable order.
* Breaking: removed `facet_intervals` argument.
* Breaking: removed void argument from `gg_theme`.

# ggblanket 1.2.2

* Defaulted col_legend_place to bottom.  

# ggblanket 1.2.1
* Fixed bug with `*_limits` and `*_include` not working correctly.  

# ggblanket 1.2.0

* Added support for `*_breaks` to receive a function.
* Added `*_include` argument. 
* Added `*_trans` argument.
* Modified default `x_breaks` behaviour for when both x and y are numeric or date.
* Breaking: Removed `*_breaks_n` and `*_breaks_width`.
* Enhanced `gg_blank` to work with only one x or y.
* Provided support for `ggplotly`.
* Added new `add_tooltip_text` function.
* Added new `titles` argument to apply a function across titles.
* Support `gg_` functions working with more stats.
* Removed `size` argument, in anticipation of ggplot2 3.4.0.
* Updated `gg_theme` title vjust and margins.

# ggblanket 1.1.0

* Remove default sentence case transformation of categorical variable labels.
* Updated scales to work in a consistent way with position = "fill".
* Other minor changes.

# ggblanket 1.0.0

* Initial release.

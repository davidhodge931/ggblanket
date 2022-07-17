# ggblanket 1.2.2

* Defaulted col_legend_place to bottom.  

# ggblanket 1.2.1

* Fixed issue with `*_limits` and `*_include` not working correctly.  

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

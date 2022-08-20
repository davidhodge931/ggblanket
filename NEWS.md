# ggblanket 1.3.6

* Updated default col breaks to breaks_pretty(4).

# ggblanket 1.3.5

* Reverted to scales::oob_keep. 
* Breaking: removed option to adjust oob. 

# ggblanket 1.3.4

* Added `facet_space` argument.

# ggblanket 1.3.3

* Corrected unintended affect on scales with v1.3.2.

# ggblanket 1.3.2

* Corrected unintended affect on scales with v1.3.1.

# ggblanket 1.3.1

* Changed `*_oob` default back to censor, and fix `position = "fill"` bug

# ggblanket 1.3.0

* Added `facet2` aesthetic in to support effortless grid facetting.
* Changed default background theme colours.
* Fixed bug with `*_limits` and `*_include` not working correctly.  
* Added *_sec_axis arguments.
* Changed *_oob default to `scales::oob_keep`.
* Removed reversal of logical variable order.
* Defaulted col_legend_place to bottom.  
* Breaking: removed `facet_intervals` argument.
* Breaking: made extensive changes to `gg_theme` function.

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

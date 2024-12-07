% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weave.R
\name{weave_theme}
\alias{weave_theme}
\title{Set a theme (without side-effects)}
\usage{
weave_theme(theme = light_mode_r() + mode_orientation_to_x())
}
\arguments{
\item{theme}{A ggplot2 theme that the \verb{gg_*} function will add without side-effects if the mode is set/weaved to \code{NULL} (and also is applied to ggplot code outside of ggblanket).}
}
\description{
Set a theme to be \code{+}-ed on unmodified to \verb{gg_*} functions. Note, the \code{mode} takes precedence, unless the set/weaved mode is \code{mode = NULL}.
}


# ********************************************************************************
# * `plot`:
# ********************************************************************************

# `plot.default()` relies on `xy.coords()`, which relies on `as.double()`. We
# have `vec_cast.double.{class}()` implementations to make this work on our
# classes, so we can just fall through to `plot.default()` for basic
# functionality. To get appropriate tick labels, we need to implement some
# helper methods:

#' @export
Axis.epicalendar_caldate <- function(x = NULL, ...)  {
  Axis(as.Date(x), ...)
}

#' @export
Axis.epicalendar_calweek <- function(x = NULL, ...)  {
  # XXX Using labels out of `format` would be better but doesn't seem dead
  # simple.
  Axis(as.Date(owning_caldate_of(x)), ...)
}

# ********************************************************************************
# * `ggplot2`:
# ********************************************************************************

# TODO

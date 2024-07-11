#' @include epicalendar-utils.R
NULL

# TODO look into clock pkg, consider lubridate compatibility, look at gregorian package

# TODO port over calweek

# TODO difftime arith if possible with base difftimes

# ********************************************************************************
# * Constants:
# ********************************************************************************

# Choose an caldate origin that will be represented by the integer 0L within
# caldate objects. The current selection is the same as the Date origin, which
# is mapped to 0L by as.integer, which allows for quick conversions from Date
# objects. However, certain other operations could be faster with a different
# choice of caldate origin.
caldate_origin_Date <- as.Date("1970-01-01")
caldate_origin_Date_as_integer <- as.integer(caldate_origin_Date)
caldate_origin_POSIXlt <- as.POSIXlt(caldate_origin_Date)
caldate_origin_year <- caldate_origin_POSIXlt$year + 1900L
caldate_origin_wday0 <- caldate_origin_POSIXlt$wday

# Leap year patterns repeat every 400 years, as does much of the common
# information of interest about dates. Precalculate some date information for
# 400 years' worth of dates, to use to more quickly extract information about
# other dates. This is not particularly memory-efficient, and caching issues may
# make it not very time-efficient either; an alternative to consider is to use
# iterator concepts and/or lazy "algebraic" data frames building off of these
# concepts to compactly represent the patterns below.
date_cycle_period <- 365L * 400L + 100L - 4L + 1L
date_cycle_years_per_cycle <- 400L
date_cycle_dates <- as.Date("1970-01-01") + seq_len(date_cycle_period) - 1L
date_cycle_year_offset_pattern <- as.POSIXlt(date_cycle_dates)$year %>%
  {
    . - .[[1L]]
  }
date_cycle_yday0_pattern <- as.POSIXlt(date_cycle_dates)$yday %>%
  {
    . - .[[1L]]
  }
date_cycle_mon0_pattern <- as.POSIXlt(date_cycle_dates)$mon
date_cycle_mday1_pattern <- as.POSIXlt(date_cycle_dates)$mday

# ********************************************************************************
# * Construction of / conversion to caldates:
# ********************************************************************************

# FIXME better package name

# TODO reconsider *_of* naming convention to match rlang, vctrs... incoming "type" as
# first part of name.  And maybe get_{prop} rather than {prop}_of().

# XXX consider new_caldate

# TODO check dots empty

# TODO remove branching based on origin... just insert package-level aborts at each function that relies on instead

# TODO vctrs coercion and cast functionality

# TODO tz = to handle areas near international date line that have skipped or repeated a day?

caldate_class_head <- "epicalendar_caldate"
caldate_class_vec <- c("epicalendar_caldate", "vctrs_vctr")
caldate_class_vec_with_package <- `attr<-`(caldate_class_vec, "package", "epicalendar")

setOldClass(caldate_class_vec,
  prototype = `attr<-`(`class<-`(
    vctrs::new_vctr(integer(0L)),
    caldate_class_vec_with_package
  ), ".S3Class", caldate_class_vec)
)

new_caldate <- function(days_from_origin) {
  # days_from_origin should be integer-class
  asS4(`attr<-`(`class<-`(days_from_origin, caldate_class_vec_with_package), ".S3Class", caldate_class_vec))
}

#' @export
vec_proxy.epicalendar_caldate <- function(x, ...) {
  # vctrs allows us to return "structured" data as a proxy, but to try to
  # prevent mishaps, this implementation gives integer vectors with no special
  # classes, attributes, or S4 flag (though potentially preserving any
  # underlying ALTREP):
  #
  # `as.integer` strips attributes (and at least sometimes S4 flag, but docs
  # don't guarantee that)
  as.integer(unclass(asS4(x, FALSE, FALSE)))
}

#' @export
vec_restore.epicalendar_caldate <- function(x, to, ...) {
  # default vec_restore restores class and other attributes
  asS4(NextMethod())
}

#' @export
caldate_of <- function(obj, ...) UseMethod("caldate_of", obj)

stopifnot(caldate_origin_Date_as_integer == 0L)
#' @export
caldate_of.Date <- function(obj, ...) {
  new_caldate(vec_cast(unclass(obj), integer()))
}

#' @export
caldate_of.character <- function(obj, ...) {
  caldate_of(as.Date(obj, ...))
}

#' @export
caldate_of.epicalendar_caldate <- function(obj, ...) obj

#' @export
caldate_of_ymd_vecs <- function(y, m, d) {
  # TODO speed up with tables?
  caldate_of(as.Date(paste0(y, "-", m, "-", d)))
}
#' @export
caldate_of_ymd.integer <- function(obj) {
  d <- obj %% 100L
  ym <- obj %/% 100L
  m <- ym %% 100L
  y <- ym %/% 100L
  caldate_of_ymd_vecs(y, m, d)
}
#' @export
caldate_of_ymd.numeric <- function(obj) {
  if (!all(as.integer(obj) == obj)) {
    stop("obj must not have fractional part")
  }
  caldate_of_ymd.integer(as.integer(obj))
}
# XXX below names vs. mon1, mday1?...
# XXX default keys being 1L, 2L, 3L?
#' @export
caldate_of_ymd.list <- function(obj, y_key = "year", m_key = "mon", d_key = "mday") {
  y <- obj[[y_key]]
  m <- obj[[m_key]]
  d <- obj[[d_key]]
  caldate_of_ymd_vecs(y, m, d)
}
#' @export
caldate_of_ymd.data.frame <- caldate_of_ymd.list


# ********************************************************************************
# * Getting information out of a caldate:
# ********************************************************************************

# FIXME generics...

# XXX might want calyear class, fine, distinct name from "year".  What about classes for other concepts though?

# FIXME unclass -> setS4(, FALSE)

#' @export
year_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- vec_proxy(obj)
  periods_from_origin <- days_from_origin %/% date_cycle_period
  period_offset <- days_from_origin %% date_cycle_period
  caldate_origin_year +
    date_cycle_years_per_cycle * periods_from_origin +
    date_cycle_year_offset_pattern[period_offset + 1L]
}

#' @export
yday0_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- vec_proxy(obj)
  period_offset <- days_from_origin %% date_cycle_period
  date_cycle_yday0_pattern[period_offset + 1L]
}
#' @export
yday1_of.epicalendar_caldate <- function(obj) {
  yday0_of(obj) + 1L
}

#' @export
mon0_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- vec_proxy(obj)
  period_offset <- days_from_origin %% date_cycle_period
  date_cycle_mon0_pattern[period_offset + 1L]
}
#' @export
mon1_of.epicalendar_caldate <- function(obj) {
  mon0_of.epicalendar_caldate(obj) + 1L
}

#' @export
mday1_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- vec_proxy(obj)
  period_offset <- days_from_origin %% date_cycle_period
  date_cycle_mday1_pattern[period_offset + 1L]
}
#' @export
mday0_of.epicalendar_caldate <- function(obj) {
  mday1_of.epicalendar_caldate(obj) - 1L
}

#' @export
wday0_of.epicalendar_caldate <- function(obj) {
  # Note:
  # * obj's wday0 is congruent to (vec_proxyed obj + offset) mod 7
  # * caldate.origin.wday0 is congruent to (vec_proxyed origin + offset) mod 7
  # * vec_proxyed origin = 0L
  # * so caldate.origin.wday0 is congruent to offset mod 7
  # * so obj's wday0 is congruent to (vec_proxyed obj + caldate.origin.wday0) mod 7
  days_from_origin <- vec_proxy(obj)
  wday0 <- (days_from_origin + caldate_origin_wday0) %% 7L
  wday0
}
#' @export
wday1_of.epicalendar_caldate <- function(obj) {
  wday0 <- get_wday0.epicalendar_caldate(obj)
  wday7 <- wday0 + 7L * (wday0 == 0L) # (implicit conversion from TRUE/FALSE to 1/0)
  wday7
}

#' @export
n0_of_same_wday_in_year_up_to.epicalendar_caldate <- function(obj) {
  yday0_of(obj) %/% 7L
}
#' @export
n1_of_same_wday_in_year_up_to.epicalendar_caldate <- function(obj) {
  # XXX potential good use of an inlining function
  yday0_of(obj) %/% 7L + 1L
}

# FIXME rely on vctrs if possible:

stopifnot(caldate_origin_Date_as_integer == 0L)
#' @export
as.Date.epicalendar_caldate <-
  function(x, ...) {
    # XXX assumes things about structure of Date objects... check if they can be relied on
    # TODO reject dots
    `class<-`(vec_proxy(x), "Date")
  }

# ********************************************************************************
# * General convenience functions for caldates:
# ********************************************************************************

#' @export
is_caldate <- function(x) {
  inherits(x, "epicalendar_caldate")
}

# `vctrs` will provide a nice `print` implementation based on our `format`
# implementation, as well as `vec_size`, extractors, setters, etc. However,
# since we set the S4 flag, we also get a bunch of behaviors that mask the
# `vctrs` ones. We'll need to implement S4 methods that stand out in front of
# those unwanted behaviors and forward to `vctrs`.

# route to the `vctrs` implementation and avoid the default `show` which outputs
# a very unclear message. The flag also gives us many other behaviors that mask
# the nice vctrs offerings, and require manual forwarding to avoid.

#' @export
format.epicalendar_caldate <- function(x, ...) {
  format.Date(as.Date(x), ...)
}

#' @export
setMethod("show", "epicalendar_caldate", function(object) {
  print(asS4(object, FALSE, FALSE))
})

# `print`, `vec_size`, extractors, setters, ...: provided by vctrs.

# ********************************************************************************
# * Arithmetic for caldates:
# ********************************************************************************

# #' @method vec_arith epicalendar_caldate
# #' @export
# #' @export vec_arith.epicalendar_caldate
# vec_arith.epicalendar_caldate <- function(op, x, y, ...) {
#   UseMethod("vec_arith.epicalendar_caldate", y)
# }

# #' @method vec_arith.epicalendar_caldate default
# #' @export
# vec_arith.epicalendar_caldate.default <- function(op, x, y, ...) {
#   # FIXME why isn't this being used in caldate_of(Sys.Date()) + (Sys.Date() - Sys.Date()) ?
#   stop_incompatible_op(op, x, y)
# }

# #' @method vec_arith.epicalendar_caldate integer
# #' @export
# vec_arith.epicalendar_caldate.integer <- function(op, x, y, ...) {
#   switch(op,
#     "+" = new_vctr(unclass(x) + y, class = "epicalendar_caldate"),
#     "-" = new_vctr(unclass(x) - y, class = "epicalendar_caldate"),
#     stop_incompatible_op(op, x, y)
#   )
# }

# #' @method vec_arith.epicalendar_caldate double
# #' @export
# vec_arith.epicalendar_caldate.double <- function(op, x, y, ...) {
#   vec_arith.epicalendar_caldate.integer(op, x, vec_cast(y, integer()), ...)
# }

# #' @method vec_arith.epicalendar_caldate difftime
# #' @export
# vec_arith.epicalendar_caldate.difftime <- function(op, x, y, ...) {
#   if (!attr(y, "units") %in% c("days", "weeks")) {
#     cli_abort('Cannot perform arithmetic on <epicalendar_caldate> Cannot add difftime with units of "{attr(y, "units")}"')
#   }
#   # FIXME finish
# }

#' @export
setMethod("+", c("epicalendar_caldate", "integer"), function(e1, e2) {
  vec_restore(vec_proxy(e1) + e2, e1)
})

setOldClass("difftime") # FIXME might need more information to be correct / compatible with other oldclasses of it.  E.g., contains (numeric? double? R-version-dependent?) and slots (for units)

#' @export
setMethod("+", c("epicalendar_caldate", "difftime"), function(e1, e2) {
  if (!attr(y, "units") %in% c("days", "weeks")) {
    cli_abort('Cannot perform arithmetic on <epicalendar_caldate> Cannot add difftime with units of "{attr(y, "units")}"')
  }
  vec_restore(vec_proxy(e1) + as.integer(e2, units = "days"), e1)
})

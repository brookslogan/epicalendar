#' @include epicalendar-utils.R
NULL

# TODO look into clock pkg, consider lubridate compatibility

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

#' @export
caldate_of <- function(obj, ...) UseMethod("caldate_of", obj)
#' @export
caldate_of.Date <-
  # XXX below branching probably commented out because broke roxygen2
  #
  # if (caldate.origin.Date.as.integer == 0L) {
  #     function(obj, ...) {
  #         `class<-`(
  #             as.integer(obj),
  #             "caldate"
  #         )
  #     }
  # } else {
  function(obj, ...) {
    # `class<-`(
    #     as.integer(obj) - caldate.origin.Date.as.integer,
    #     "caldate"
    # )
    # XXX maybe we should wrap in a list + use a vec_proxy in order to avoid vctrs:::vec_arith.difftime.numeric... if that's actually whats going on and not just another form of:
    # FIXME consider also what's going on with transpose... seems like Ops.difftime wins out and potentially does the wrong thing; related: https://github.com/r-lib/vctrs/issues/160, maybe https://github.com/wch/r-source/blob/02011ce032940791908769df154c3238cf06af9f/src/main/arithmetic.c#L390, https://github.com/wch/r-source/blob/02011ce032940791908769df154c3238cf06af9f/src/main/arithmetic.c#L520
    # ... or try S4, or ALTREP magic?
    # ... or try to detect attachment of "units" attribute or raise errors when it's set?
    # is it allowed to attach global calling handlers to try to promote specific warnings to errors?
    # masquerade everything as regular Dates and see if special logic makes it work?
    #
    # at least try to get a hard error, and provide an "advance"/etc. method that actually works
    new_vctr(
      vec_cast(unclass(obj), integer()) - caldate_origin_Date_as_integer,
      class = "epicalendar_caldate"
    )
  }
# }
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

#' @export
year_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- unclass(obj)
  periods_from_origin <- days_from_origin %/% date_cycle_period
  period_offset <- days_from_origin %% date_cycle_period
  caldate_origin_year +
    date_cycle_years_per_cycle * periods_from_origin +
    date_cycle_year_offset_pattern[period_offset + 1L]
}

#' @export
yday0_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- unclass(obj)
  period_offset <- days_from_origin %% date_cycle_period
  date_cycle_yday0_pattern[period_offset + 1L]
}
#' @export
yday1_of.epicalendar_caldate <- function(obj) {
  yday0_of(obj) + 1L
}

#' @export
mon0_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- unclass(obj)
  period_offset <- days_from_origin %% date_cycle_period
  date_cycle_mon0_pattern[period_offset + 1L]
}
#' @export
mon1_of.epicalendar_caldate <- function(obj) {
  mon0_of.epicalendar_caldate(obj) + 1L
}

#' @export
mday1_of.epicalendar_caldate <- function(obj) {
  days_from_origin <- unclass(obj)
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
  # * obj's wday0 is congruent to (unclassed obj + offset) mod 7
  # * caldate.origin.wday0 is congruent to (unclassed origin + offset) mod 7
  # * unclassed origin = 0
  # * so caldate.origin.wday0 is congruent to offset mod 7
  # * so obj's wday0 is congruent to (unclassed obj + caldate.origin.wday0) mod 7
  days_from_origin <- unclass(obj)
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

#' @export
as.Date.epicalendar_caldate <-
  if (caldate_origin_Date_as_integer == 0L) {
    function(x, ...) {
      # XXX assumes things about structure of Date objects... check if they can be relied on
      x_as_Date <- x
      class(x_as_Date) <- "Date"
      x_as_Date
    }
  } else {
    function(x, ...) {
      as.Date(unclass(x), caldate_origin_Date)
    }
  }

# ********************************************************************************
# * General convenience functions for caldates:
# ********************************************************************************

#' @export
is_caldate <- function(x) {
  inherits(x, "epicalendar_caldate")
}

#' @export
format.epicalendar_caldate <- function(x, ...) {
  format.Date(as.Date(x), ...)
}

# `print`, `vec_size`, extractors, setters: provided by vctrs

# ********************************************************************************
# * Arithmetic for caldates:
# ********************************************************************************

#' @method vec_arith epicalendar_caldate
#' @export
#' @export vec_arith.epicalendar_caldate
vec_arith.epicalendar_caldate <- function(op, x, y, ...) {
  UseMethod("vec_arith.epicalendar_caldate", y)
}

#' @method vec_arith.epicalendar_caldate default
#' @export
vec_arith.epicalendar_caldate.default <- function(op, x, y, ...) {
  # FIXME why isn't this being used in caldate_of(Sys.Date()) + (Sys.Date() - Sys.Date()) ?
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.epicalendar_caldate integer
#' @export
vec_arith.epicalendar_caldate.integer <- function(op, x, y, ...) {
  switch(op,
    "+" = new_vctr(unclass(x) + y, class = "epicalendar_caldate"),
    "-" = new_vctr(unclass(x) - y, class = "epicalendar_caldate"),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.epicalendar_caldate double
#' @export
vec_arith.epicalendar_caldate.double <- function(op, x, y, ...) {
  vec_arith.epicalendar_caldate.integer(op, x, vec_cast(y, integer()), ...)
}

#' @method vec_arith.epicalendar_caldate difftime
#' @export
vec_arith.epicalendar_caldate.difftime <- function(op, x, y, ...) {
  if (! attr(y, "units") %in% c("days", "weeks")) {
    cli_abort('Cannot perform arithmetic on <epicalendar_caldate> Cannot add difftime with units of "{attr(y, "units")}"')
  }
  # FIXME finish
}

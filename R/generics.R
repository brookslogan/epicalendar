#' @title Extract "unique" information about time points or time intervals
#' @name getters
#'
#' @description
#'
#' Here, "unique" means that that every possible time point or time interval of
#' an accepted class should be guaranteed to have exactly one plausible value.
#' This is meant to avoid bugs by forbidding, for example, [`year_of()`] from
#' operating on [`calweek`]s, since a single week can contain days belonging to
#' two different years (try feeding through a disambiguating function such as
#' [`owning_caldate_of()`] first).
#'
#' These functions are all vectorized in such a way that the output should have
#' a [`vctrs::vec_size()`] equal to that of `obj` if it's the only argument, or
#' the [`vctrs::vec_size_common()`] of all arguments if there are multiple. For
#' brevity, the descriptions below will describe their operation on length-1
#' arguments.
#'
#' @param obj object representing a time point or time interval
NULL

#' @describeIn getters The uniquely-associated year, as an integer
#' @export
year_of <- function(obj) UseMethod("year_of", obj)

#' @describeIn getters For the uniquely-associated date, the 0-indexed number of
#'   days it is from the start of the containing year (e.g., January 1 of of
#'   any year has a `yday0_of` of 0)
#' @export
yday0_of <- function(obj) UseMethod("yday0_of", obj)

#' @describeIn getters For the uniquely-associated date, the 1-indexed number of
#'   days it is from the start of the containing year (e.g., January 1 of any
#'   year has a `yday1_of` of 1); this matches [`lubridate::yday()`]
#' @export
yday1_of <- function(obj) UseMethod("yday1_of", obj)

#' @describeIn getters For the uniquely-associated month, the 0-indexed number
#'   of months it is from the start of the containing year (e.g., January of any
#'   year has a `mon0_of` of 0); this matches `$mon` on `POSIXlt`s
#' @export
mon0_of <- function(obj) UseMethod("mon0_of", obj)

#' @describeIn getters For the uniquely-associated month, the 1-indexed number
#'   of months it is from the start of the containing year (e.g., January of any
#'   year has a `mon1_of` of 1); this matches [`lubridate::month()`]
#' @export
mon1_of <- function(obj) UseMethod("mon1_of", obj)

#' @describeIn getters For the uniquely-associated date, the 0-indexed number of
#'   days it is from the start of the containing month (e.g., January 1 of any
#'   year has a `mday0_of` of 0)
#' @export
mday0_of <- function(obj) UseMethod("mday0_of", obj)

#' @describeIn getters For the uniquely-associated date, the 1-indexed number of
#'   days it is from the start of the containing month (e.g., January 1 of any
#'   year has a `mday1_of` of 1); this matches `$mday` of `POSIXlt`s and
#'   `lubridate::mday()`
#' @export
mday1_of <- function(obj) UseMethod("mday1_of", obj)

#' @describeIn getters For the uniquely-associated date, the 0-indexed number of
#'   days it is from the start of the containing Sunday-to-Saturday week (e.g.,
#'   Sunday of any week has a `wday0_of` of 0); this matches `$wday` of `POSIXlt`s
#' @export
wday0_of <- function(obj) UseMethod("wday0_of", obj)

#' @describeIn getters For the uniquely-associated date, the 1-indexed number of
#'   days it is from the start of the containing Sunday-to-Saturday week (e.g.,
#'   Sunday of any week has a `wday1_of` of 1); this matches [`lubridate::wday()`]
#' @export
wday1_of <- function(obj) UseMethod("wday1_of", obj)

#' @describeIn getters For the uniquely-associated date, the "7-indexed" number
#'   of days it is from the start of the containing Sunday-to-Saturday week
#'   (e.g., Sunday of any week has a `wday7_of` of 7, and Monday has a
#'   `wday7_of` of 1); this matches "%u" of [`format.Date()`] (converted to an
#'   integer)
#' @export
wday7_of <- function(obj) UseMethod("wday7_of", obj)

#' @describeIn getters For the uniquely-associated date, the 0-indexed number of
#'   days with the same day of week it is from the start of the containing year
#'   (e.g., January 1 through January 6 of any year each have a
#'   `n0_of_same_wday_in_year_up_to` of 0)
#' @export
n0_of_same_wday_in_year_up_to <- function(obj) UseMethod("n0_of_same_wday_in_year_up_to", obj)

#' @describeIn getters For the uniquely-associated date, the 1-indexed number of
#'   days with the same day of week it is from the start of the containing year
#'   (e.g., January 1 through January 6 of any year each have a
#'   `n1_of_same_wday_in_year_up_to ` of 1)
#' @export
n1_of_same_wday_in_year_up_to <- function(obj) UseMethod("n1_of_same_wday_in_year_up_to", obj)








#' @describeIn getters For classes such as calweeks that define "owning" dates,
#'   the uniquely-associated owning date as a caldate object.
#' @export
owning_caldate_of <- function(obj) UseMethod("owning_caldate_of")

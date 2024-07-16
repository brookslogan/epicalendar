
# ********************************************************************************
# * Construction of / conversion to calweeks:
# ********************************************************************************

calweek_class_head <- "epicalendar_calweek"
calweek_class_vec <- c("epicalendar_calweek", "vctrs_vctr")
calweek_class_vec_with_package <- `attr<-`(calweek_class_vec, "package", "epicalendar")
calweek_attribute_keys = c("starting_wday0", "owning_wday0", "convention_name", "year_week_separator")

#' @export
setOldClass(
  calweek_class_vec,
  prototype = structure(
    vctrs::new_vctr(integer(0L)),
    class = calweek_class_vec_with_package,
    .S3Class = calweek_class_vec,
    starting_wday0 = 1L,
    owning_wday0 = 4L,
    convention_name = "isoweek",
    year_week_separator = "-W"
  )
)

#' @export
new_calweek <- function(owning_days_from_origin, starting_wday0, owning_wday0, convention_name = NA_character_, year_week_separator = "CW") {
  owning_days_from_origin <- vctrs::vec_cast(assert_integerish(owning_days_from_origin, any.missing = FALSE), integer())
  starting_wday0 <- as.integer(assert_int(starting_wday0, lower = 0L, upper = 6L))
  owning_wday0 <- as.integer(assert_int(owning_wday0, lower = 0L, upper = 6L))
  assert_string(convention_name, na.ok = TRUE)
  assert_string(year_week_separator)
  if (!all(wday0_of(new_caldate(owning_days_from_origin)) == owning_wday0)) {
    cli_abort("Owning dates from `owning_days_from_origin` don't all have a wday0_of `owning_wday0`")
  }
  asS4(structure(
    vctrs::new_vctr(owning_days_from_origin),
    class = calweek_class_vec_with_package,
    .S3Class = calweek_class_vec,
    starting_wday0 = starting_wday0,
    owning_wday0 = owning_wday0,
    convention_name = convention_name,
    year_week_separator = year_week_separator
  ))
}

#' @export
setMethod("initialize", calweek_class_head, function(.Object, ...) {
  new_calweek(...)
})

#' @export
vec_proxy.epicalendar_calweek <- function(x, ...) {
  # vctrs allows us to return "structured" data as a proxy, but to try to
  # prevent mishaps, this implementation gives integer vectors with no special
  # classes, attributes, or S4 flag (though potentially preserving any
  # underlying ALTREP):
  #
  as.integer(unclass(asS4(x, FALSE, FALSE)))
  # ^ `as.integer` strips attributes (and at least sometimes the S4 flag, but
  # docs don't guarantee that)
}

#' @export
vec_restore.epicalendar_calweek <- function(x, to, ...) {
  # default vec_restore restores class and other attributes
  asS4(NextMethod())
}

# ********************************************************************************
# * General convenience functions for calweeks:
# ********************************************************************************

#' @export
is_calweek <- function(x) {
  inherits(x, "epicalendar_calweek")
}

#' @export
owning_caldate_of.epicalendar_calweek <- function(obj) {
  new_caldate(vec_proxy(obj))
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
format.epicalendar_calweek <- function(x, ...) {
  owning_caldates <- owning_caldate_of(x)
  owning_year <- year_of(owning_caldates)
  owning_week <- n1_of_same_wday_in_year_up_to(owning_caldates)
  parts <- vec_recycle_common(owning_year, attr(x, "year_week_separator"), sprintf("%02d", owning_week))
  pasted_parts <- paste0(parts[[1L]], parts[[2L]], parts[[3L]])
  pasted_parts
}

#' @export
setMethod("show", "epicalendar_calweek", function(object) {
  print(asS4(object, FALSE, FALSE))
})

# `print`, `vec_size`, extractors, setters, ...: provided by vctrs.

#' @export
vec_ptype_abbr.epicalendar_calweek <- function(x, ...) {
  convention_name <- attr(x, "convention_name")
  if (is.na(convention_name)) {
    "calweek"
  } else {
    convention_name
  }
}

#' @export
vec_ptype_full.epicalendar_calweek <- function(x, ...) {
  convention_name <- attr(x, "convention_name")
  if (is.na(convention_name)) {
    convention_name <- paste0("custom(", attr(x, "starting_wday0"), ", ", attr(x, "owning_wday0"), ")")
  }
  paste0("epicalendar::calweek(", convention_name, ")")
}

# ********************************************************************************
# * Arithmetic for calweeks:
# ********************************************************************************

#' @export
setMethod("+", c("epicalendar_calweek", "integer"), function(e1, e2) {
  vec_restore(vec_proxy(e1) + 7L * e2, e1)
})

#' @export
setMethod("-", c("epicalendar_calweek", "integer"), function(e1, e2) {
  vec_restore(vec_proxy(e1) - 7L * e2, e1)
})

#' @export
setMethod("+", c("epicalendar_calweek", "difftime"), function(e1, e2) {
  if (!attr(e2, "units") %in% c("days", "weeks")) {
    cli_abort('<epicalendar_calweek> + <difftime> requires difftime to have units of days or weeks, not "{attr(y, "units")}"')
  }
  vec_restore(vec_proxy(e1) + 7L * vctrs::vec_cast(as.numeric(e2, units = "weeks"), integer()), e1)
})

#' @export
setMethod("-", c("epicalendar_calweek", "difftime"), function(e1, e2) {
  if (!attr(e2, "units") %in% c("days", "weeks")) {
    cli_abort('<epicalendar_calweek> - <difftime> requires difftime to have units of days or weeks, not "{attr(y, "units")}"')
  }
  vec_restore(vec_proxy(e1) - 7L * vctrs::vec_cast(as.numeric(e2, units = "weeks"), integer()), e1)
})

#' @export
setMethod("+", c("difftime", "epicalendar_calweek"), function(e1, e2) {
  stop_incompatible_op("+", e1, e2, details = "try <epicalendar_calweek> + <difftime>")
})

#' @export
setMethod("-", c("difftime", "epicalendar_calweek"), function(e1, e2) {
  stop_incompatible_op("-", e1, e2)
})

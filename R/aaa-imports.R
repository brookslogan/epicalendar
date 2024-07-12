# Note: this file needs to be run before others relying on the magrittr pipe or
# difftime S4 registration. So we have an aaa- prefix so the default C-locale
# collation will put this first, and we don't need to @include it in a bunch of
# other files and have a Collate:.

# Normal imports:

#' @importFrom vctrs new_vctr vec_cast vec_arith stop_incompatible_op vec_proxy vec_restore
#' @importFrom cli cli_abort
#' @importFrom methods setOldClass setMethod
NULL

# Defining the pipe here rather than @importFrom-ing it allows us to remove and
# rebuild the NAMESPACE file without having errors trying to build package
# member objects using `%>%`:
`%>%` <- magrittr::`%>%`

# We need to register difftime in the S4 system in order to use setMethod with
# it. There's the option of providing additional type information when
# registering it, or keeping it simple. This package shouldn't need to rely on
# that extra information, but it'd be nice to try to prevent that information
# from being lost if another package provides it.

# Some testing is here:
# https://github.com/brookslogan/epicalendar/issues/1#issuecomment-2224104620

# Here's an approach that attaches the type information and avoids some warning
# messages from the above testing. However, (i) other packages with simple
# definitions can cause the type information not to be attached, (ii) if a
# package provides only part of the type information, it can raise hard errors,
# and (iii) if `contains = "double"` ever works and another package uses it,
# then this may also cause other warnings and imprecision.

# setClass("epicalendar_difftime_S4", contains = "numeric", slots = c("units" = "character"))
# setOldClass("difftime", S4Class = "epicalendar_difftime_S4")

# Instead, we'll stay with the simple approach... but this means that any other
# packages that do provide the type information are going to have to check for
# this simple definition and manually try to overwrite it. They'd have to do it
# for interop with other packages anyway.

setOldClass("difftime")


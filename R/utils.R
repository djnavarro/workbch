# utility functions that are not workbch-specific

# returns a list of expressions
capture_dots <- function(...) {
  as.list(substitute(list(...)))[-1L]
}

# return LHS, unless LHS is null in which case return RHS
# as a reminder, the source code from rlang:
#
# `%||%` <- function (x, y)
# {
#   if (is_null(x))
#     y
#   else x
# }

#' @importFrom rlang %||%
NULL

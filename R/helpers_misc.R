# miscellaneous helper functions

# returns a list of expressions
capture_dots <- function(...) {
  as.list(substitute(list(...)))[-1L]
}

`%||%` <- function(x, y) {
  if(is.null(x)) {
    return(y)
  }
  return(x)
}

# tidying -----------------------------------------------------------------


# find the jobs or tasks that need to be hidden and hide them
apply_mask <- function(tbl) {
  tbl <- dplyr::filter(tbl, status %in% c("active", "inactive"))
  return(tbl)
}

# used in generating the markdown output
prettify <- function(x) {
  kableExtra::kable_styling(
    knitr::kable(x),
    "striped",
    font_size = 12)
}

# format a date string
format_date <- function(date) {
  date <- lubridate::dmy(date)
  date <- format(date, format = "%d %b %Y")
  return(date)
}

# print methods -----------------------------------------------------------


# Specify a print method for a workbench tibble
#' @export
print.wkbch_tbl <- function(x, n = 100, ...) {
  x <- de_wkbch_tbl(x)
  print(x, n = n, ...)
}

# strange coercion
as_wkbch_tbl <- function(x) {
  class(x) <- c("wkbch_tbl", class(x))
  return(x)
}

# remove wkbch class
de_wkbch_tbl <- function(x) {
  class(x) <- setdiff(class(x), "wkbch_tbl")
  return(x)
}



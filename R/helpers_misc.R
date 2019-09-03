# miscellaneous helper functions

# convenience function: I use NULL to indicate user has not set the
# but I prefer is_set(value) when reading the code
is_set <- function(x) {
  !is.null(x)
}

# find the jobs that need to be hidden and hide them
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

# repeatedly ask user until a stop signal is reached
multireadline <- function(prompt, stop = "") {
  out <- character(0)
  ask <- "BLAH"
  while(ask != stop) {
    ask <- readline(prompt = prompt)
    out <- c(out, ask)
  }
  return(out[-length(out)])
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



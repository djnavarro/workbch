
split_tags <- function(tags) {
  tags <- strsplit(tags, "|", fixed = TRUE)[[1]]
  tags <- trimws(tags, which = "both")
  return(tags)
}

split_url <- function(url) {
  url <- strsplit(url, "|", fixed = TRUE)[[1]]
  url <- trimws(url, which = "both")
  return(url)
}

idstring <- function() {
  paste0(sample(c(letters, LETTERS), 10, TRUE), collapse="")
}

# Specify a print method for a workbench tibble
#' @export
print.wkbch_tbl <- function(x, n = 100, ...) {
  class(x) <- setdiff(class(x), "wkbch_tbl")
  print(x, n = n, ...)
}

# A very bad way to coerce tibble to workbench tibble
as_wkbch_tbl <- function(x) {
  class(x) <- c("wkbch_tbl", class(x))
  return(x)
}

#' @importFrom rlang %||%
NULL

# # returns a list of expressions
# capture_dots <- function(...) {
#   as.list(substitute(list(...)))[-1L]
# }


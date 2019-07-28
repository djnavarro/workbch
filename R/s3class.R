
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

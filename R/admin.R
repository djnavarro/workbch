
#' Get or set the location of projectr files
#'
#' @param path Path to the folder
#'
#' @return Path to the folder
#' @export
projectr_home <- function(path = NULL) {
  if(!is.null(path)) {
    options(projectr.home = path)
  }
  job_home()
}

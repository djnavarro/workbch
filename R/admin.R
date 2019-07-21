
#' Set the location of workbch files
#'
#' @param path Path to the folder
#'
#' @return Path to the folder
#' @export
set_home <- function(path = NULL) {
  if(!is.null(path)) {
    options(workbch.home = path)
  }
  job_home()
}

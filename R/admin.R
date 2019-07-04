
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



#' Open the RStudio project linked to the job
#'
#' @param name Name of job to open
#' @export
open_project <- function(name) {
  jobs <- job_read()
  if(rstudioapi::isAvailable()) {
    rstudioapi::openProject(jobs[[name]]$path)
  }
}

#' Navigate to the RStudio project linked with a job
#'
#' @param name Name of job to open
#' @export
goto_project <- function(name) {
  jobs <- job_read()
  if(rstudioapi::isAvailable()) {
    rstudioapi::openProject(jobs[[name]]$path)
  }
}

#' Navigate to a URL linked to a job
#'
#' @param name name of the project
#' @param site label denoting the site (e.g., "github")
#' @export
goto_url <- function(name, site) {

  jobs <- job_read()
  url <- jobs[[name]]$url[[site]]
  utils::browseURL(url)

}



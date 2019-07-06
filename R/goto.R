

#' Navigate to the RStudio project linked with a job
#'
#' @param name name of job to open
#' @export
#' @examples
#' \dontrun{
#'
#' goto_project("myjob")
#' }
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
#' @examples
#' \dontrun{
#'
#' goto_url("myjob", "github")
#' }
goto_url <- function(name, site) {

  jobs <- job_read()
  url <- jobs[[name]]$url[[site]]
  utils::browseURL(url)

}

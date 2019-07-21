# defines the "goto" family of functions

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

  use_rstdio <- FALSE
  if(rstudioapi::isAvailable()) {
    files <- list.files(jobs[[name]]$path)
    rproj <- grep(".*\\.Rproj$", files)
    if(length(rproj) > 0) {
      use_rstdio <- TRUE
    }
  }

  if(use_rstdio) {
    rstudioapi::openProject(jobs[[name]]$path)
  } else {
    message("setting working directory to ", jobs[[name]]$path)
    setwd(jobs[[name]]$path)
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
  urls <- jobs[[name]]$urls
  if(!(site %in% urls$site)) {
    stop("'", name, "' does not have a link for site '", site, "'", call. = FALSE)
  }
  link <- urls$link[urls$site == site]
  utils::browseURL(link)

}

# defines the "goto" family of functions

#' Navigate to a job
#'
#' @param jobname name of job to open
#' @export
#' @examples
#' \dontrun{
#'
#' goto_job("myjob")
#' }
goto_job <- function(jobname) {
  jobs <- job_read()

  use_rstdio <- FALSE
  if(rstudioapi::isAvailable()) {
    files <- list.files(jobs[[jobname]]$path)
    rproj <- grep(".*\\.Rproj$", files)
    if(length(rproj) > 0) {
      use_rstdio <- TRUE
    }
  }

  if(use_rstdio) {
    rstudioapi::openProject(jobs[[jobname]]$path)
  } else {
    message("setting working directory to ", jobs[[jobname]]$path)
    setwd(jobs[[jobname]]$path)
  }
}




#' Navigate to a URL linked to a job
#'
#' @param jobname name of the job
#' @param site label denoting the site (e.g., "github")
#' @export
#' @examples
#' \dontrun{
#'
#' goto_url("myjob", "github")
#' }
goto_url <- function(jobname, site) {

  jobs <- job_read()
  urls <- jobs[[jobname]]$urls
  if(!(site %in% urls$site)) {
    stop("'", jobname, "' does not have a link for site '", site, "'", call. = FALSE)
  }
  link <- urls$link[urls$site == site]
  utils::browseURL(link)

}

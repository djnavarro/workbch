
#' Navigate to a URL associated with a job
#'
#' @param site label denoting the site (e.g., "github")
#' @param jobname name of the job
#'
#' @details The \code{job_openurl()} function opens a URL associated with a
#' job in a browser window. The \code{site} argument is the site nickname
#' (e.g. "github", "homepage", etc) and the \code{jobname} argument specifies
#' the job to use.
#'
#' If no argument is specified \code{job_openurl()} will attempt to guess the
#' current job by looking at any open RStudio projects. If no project is open
#' or the RStudio API is not available it attempts to guess by looking at the
#' working directory.
#'
#' @export
job_openurl <- function(site, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  jobname <- jobname %||% job_getcurrent(jobs)

  # check jobname
  verify_jobname(jobname)
  verify_jobexists(jobname, jobs)

  urls <- jobs[[jobname]]$urls
  if(!(site %in% urls$site)) {
    stop("'", jobname, "' does not have a link for site '", site, "'", call. = FALSE)
  }
  link <- urls$link[urls$site == site]
  utils::browseURL(link)

}

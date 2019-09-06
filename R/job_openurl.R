
#' Navigate to a URL associated with a job
#'
#' @param site label denoting the site (e.g., "github")
#' @param jobname name of the job
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

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
goto_job <- function(jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()

  # if the user doesn't specify a job, prompt them
  if(is.null(jobname)) {
    job_name <- prompt_jobname()
  }

  # check jobname
  verify_jobname(jobname)
  if(!job_exists(jobname, jobs)) {
    stop("job '", jobname, "' does not exist", call. = FALSE)
  }

  # if we're not in RStudio, just change working directory
  if(!rstudioapi::isAvailable()) {
    message("setting working directory to ", jobs[[jobname]]$path)
    setwd(jobs[[jobname]]$path)
    return(invisible(NULL))
  }

  # if we're in RStudio... read files in job and look for Rproj file
  files <- list.files(jobs[[jobname]]$path)
  rproj <- grep(".*\\.Rproj$", files)

  # if there's a project we can switch to, do so...
  if(length(rproj) > 0) {
    rstudioapi::openProject(jobs[[jobname]]$path)
    return(invisible(NULL))
  }

  # if not, close the project and change working directory...
  rstudioapi::executeCommand("closeProject")
  message("setting working directory to ", jobs[[jobname]]$path)
  setwd(jobs[[jobname]]$path)
  return(invisible(NULL))

}




#' Navigate to a URL linked to a job
#'
#' @param site label denoting the site (e.g., "github")
#' @param jobname name of the job
#' @export
goto_url <- function(site, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}

  # check jobname
  verify_jobname(jobname)
  if(!job_exists(jobname, jobs)) {
    stop("job '", jobname, "' does not exist", call. = FALSE)
  }

  urls <- jobs[[jobname]]$urls
  if(!(site %in% urls$site)) {
    stop("'", jobname, "' does not have a link for site '", site, "'", call. = FALSE)
  }
  link <- urls$link[urls$site == site]
  utils::browseURL(link)

}

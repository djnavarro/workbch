#' Navigate to a job
#'
#' @param jobname name of job to open
#' @export
job_switch <- function(jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()

  # if the user doesn't specify a job, prompt them
  jobname <- jobname %||% prompt_jobname()

  # check jobname
  verify_jobname(jobname)
  verify_jobexists(jobname, jobs)

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

  # if not, just change working directory...
  #rstudioapi::executeCommand("closeProject")
  message("setting working directory to ", jobs[[jobname]]$path)
  setwd(jobs[[jobname]]$path)
  return(invisible(NULL))

}



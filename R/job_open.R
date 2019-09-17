#' Navigate to a job
#'
#' @param jobname name of job to open
#' @details The \code{job_open()} function opens a workbch job. The meaning of
#' "open" here depends on context. If there is an RStudio project linked to the
#' job and the RStudio API is available, then \code{job_open()} will switch the
#' to the RStudio project that \code{jobname} is linked to. If this is not
#' possible an there is a path associated with the job, then \code{job_open()}
#' will change the working directory to that path. If neither option is possible
#' then this function does nothing.
#'
#' Note that when working interactively, \code{job_open()} can be called without
#' specfying the \code{jobname}. In this case the user will be presented with
#' prompts to select the desired job.
#' @export
job_open <- function(jobname = NULL) {

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

prompt_jobname <- function() {
  opts <- job_list()$jobname # todo... don't call user facing function!!!!
  if(length(opts)>9) opts <- opts[1:9]
  for(i in 1:length(opts)) {
    cat(i, ":", opts[i], "\n")
  }
  cat("other: please type jobname\n")
  jnum <- readline("which job? ")
  if(jnum %in% as.character(1:length(opts))) {
    jobname <- opts[as.numeric(jnum)]
  } else {
    jobname <- jnum
  }
  return(jobname)
}



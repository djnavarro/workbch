
#' Path to the job home
#'
#' @param jobname Name of job (defaults to current job)
#'
#' @details The \code{job_home()} function returns the file path for `jobname`.
#' This can be useful if you need to specify the location of files relative to
#' a project.
#'
#' If no argument is specified \code{job_home()} will attempt to guess the
#' current job by looking at any open RStudio projects. If no project is open
#' or the RStudio API is not available it attempts to guess by looking at the
#' working directory.
#'
#' @return Path to the job folder as a character string
#'
#' @export
job_home <- function(jobname = NULL) {

  # read jobs and use the current job if the user does
  # not specify an argument
  jobs <- job_read()
  if(is.null(jobname)) {
    jobname <- suppressMessages(job_getcurrent(jobs))
  } else {
    verify_jobname(jobname)
    verify_jobexists(jobname, jobs)
  }

  path <- jobs[[jobname]]$path
  if(length(path) == 0 || is.na(path)) {
    stop("No path known for job '", jobname, "'", call. = FALSE)
  }
  return(normalizePath(path))
}


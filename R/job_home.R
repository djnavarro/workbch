
#' Path to the job home
#'
#' @param jobname Name of job
#' @export
job_home <- function(jobname) {
  verify_jobname(jobname)
  jobs <- job_read()
  path <- jobs[[jobname]]$path
  if(length(path) == 0) {
    stop("No path known for job '", jobname, "'", call. = FALSE)
  }
  return(normalizePath(path))
}


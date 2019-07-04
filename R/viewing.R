
# to avoid the 'no visible binding' NOTE:
utils::globalVariables(c("priority", "status", "owner", "name"))

#' List jobs
#'
#' @param ... expression to be passed to dplyr::filter
#' @export
view_joblist <- function(...) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("name", "owner", "priority", "status", "deadline", "description")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, name)
  if(...length() > 0) {
    job_tbl <- dplyr::filter(job_tbl, ...)
  }
  return(job_tbl)
}


#' View jobs by priority
#'
#' @param priority numeric vector of priorities to display
#' @param ... expression to be passed to view_jobs
#'
#' @return tibble of jobs
#' @export
view_priorities <- function(priority = 1, ...) {
  jobs <- view_joblist()
  jobs <- dplyr::filter(jobs, priority %in% {{priority}})
  return(jobs)
}

#' Show the details of a job
#'
#' @param name Name of job to display
#' @export
view_job <- function(name) {
  jobs <- job_read()
  print(jobs[[name]])
}

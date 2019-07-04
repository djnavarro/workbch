
# to avoid the 'no visible binding' NOTE:
utils::globalVariables(c("priority", "status", "owner", "name"))

#' List jobs
#'
#' @param ... expression to be passed to dplyr::filter
#' @export
job_list <- function(...) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("name", "owner", "priority", "status", "deadline", "description")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, name)
  if(...length() > 0) {
    job_tbl <- dplyr::filter(job_tbl, ...)
  }
  print(job_tbl)
}


#' Show the details of a job
#'
#' @param name Name of job to display
#' @export
job_show <- function(name) {
  jobs <- job_read()
  print(jobs[[name]])
}

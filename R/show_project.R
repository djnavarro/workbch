
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

#' Open a url linked to a job
#'
#' @param name name of the project
#' @param site label denoting the site (e.g., "github")
#' @export
job_browse_url <- function(name, site) {

  jobs <- job_read()
  url <- jobs[[name]]$url[[site]]
  utils::browseURL(url)

}


#' Show the details of a job
#'
#' @param name Name of job to display
#' @export
job_show <- function(name) {
  jobs <- job_read()
  print(jobs[[name]])
}

#' Open the RStudio project linked to the job
#'
#' @param name Name of job to open
#' @export
job_open <- function(name) {
  jobs <- job_read()
  if(rstudioapi::isAvailable()) {
    rstudioapi::openProject(jobs[[name]]$path)
  }
}


#' Edit a job
#'
#' @param name name of the project to edit
#' @param ... expressions to be evaluated in the job
#' @export
job_edit <- function(name, ...) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  j <- jobs[[name]]
  j[names(dots)] <- dots
  jobs[[name]] <- j

  # write
  job_write(jobs)
}


#' Edit the urls in a job
#'
#' @param name name of the project to edit
#' @param ... expressions to be evaluated within the urls field
#' @export
job_edit_urls <- function(name, ...) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  l <- jobs[[name]]$urls
  l[names(dots)] <- dots
  jobs[[name]]$urls <- l

  # write
  job_write(jobs)
}


#' Delete a job
#'
#' @param name Name of job to delete
#' @export
job_delete <- function(name) {
  jobs <- job_read()
  jobs[[name]] <- NULL
  job_write(jobs)
}

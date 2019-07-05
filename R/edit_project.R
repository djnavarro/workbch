
#' Edit a job
#'
#' @param name name of the project to edit
#' @param ... expressions to be evaluated in the job
#' @export
job_edit <- function(name, ...) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # substitute real names if necessary
  if(!is.null(dots$team)) {
    dots$team <- real_name(dots$team)
  }
  if(!is.null(dots$owner)) {
    dots$owner <- real_name(dots$owner)
  }

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  j <- jobs[[name]]
  j[names(dots)] <- dots
  jobs[[name]] <- j

  # ensure that the edits produce a valid job state
  jobs[[name]] <- validate_job(jobs[[name]])

  # write
  job_write(jobs)
}

#' Add or remove people from a team
#'
#' @param name Name of project
#' @param add Vector of names to add to the team
#' @param remove Vector of names to remove from the team
#'
#' @export
job_edit_team <- function(name, add = NULL, remove = NULL) {

  jobs <- job_read()

  if(!is.null(add)) {
    add <- real_name(add)
    jobs[[name]]$team <- unique(c(jobs[[name]]$team, add))
  }

  if(!is.null(remove)) {
    remove <- real_name(remove)

    # cannot remove owner
    if(jobs[[name]]$owner %in% remove) {
      warning(
        "cannot remove owner from a team: use job_edit() to change owner first",
        call. = FALSE
      )
      remove <- setdiff(remove, jobs[[name]]$owner)
    }

    jobs[[name]]$team <- setdiff(jobs[[name]]$team, remove)
  }

  job_write(jobs)
}


#' Edit the urls in a job
#'
#' @param name name of the project to edit
#' @param ... expressions to be evaluated within the urls field
#' @param clean delete all existing URLs first? (default = FALSE)
#' @export
job_edit_urls <- function(name, ..., clean = FALSE) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  if(clean == FALSE) {
    l <- jobs[[name]]$urls
  } else {
    l <- list()
  }
  l[names(dots)] <- dots
  jobs[[name]]$urls <- l

  # ensure that the edits produce a valid job state
  jobs[[name]] <- validate_job(jobs[[name]])

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

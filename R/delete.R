# defines the "delete" family of functions


#' Delete a job
#'
#' @param name name of the job to delete
#' @export
#' @details Deletes a job from the JSON file. At the moment, it does so without
#' asking for the user to confirm, so be careful.
#' @examples
#' \dontrun{
#'
#' delete_job("myjob")
#' }
delete_job <- function(name) {
  jobs <- job_read()
  jobs[[name]] <- NULL
  job_write(jobs)
}


#' Delete a note from a job
#'
#' @param name the job from which the note should be deleted
#' @param id the id number assigned to the note
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_note("myjob", 2)
#' }
delete_note <- function(name, id) {
  jobs <- job_read()
  jb <- jobs[[name]]

  if(id %in% jb$notes$id) {

    jb$notes <- dplyr::filter(jb$notes, id != {{id}})
    jobs[[name]] <- jb
    job_write(jobs)

  } else {
    warning("Note ", id, " does not exist in job '", name, "'", call. = FALSE)
  }
}


#' Delete a task from a job
#'
#' @param name the job from which the task should be deleted
#' @param id the id number assigned to the task
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_task("myjob", 2)
#' }
delete_task <- function(name, id) {
  jobs <- job_read()
  jb <- jobs[[name]]

  if(id %in% jb$tasks$id) {

    jb$tasks <- dplyr::filter(jb$tasks, id != {{id}})
    jobs[[name]] <- jb
    job_write(jobs)


  } else {
    warning("Task ", id, " does not exist in job '", name, "'", call. = FALSE)
  }
}



#' Delete a URL from a job
#'
#' @param name the job from which the URL should be deleted
#' @param site the name of the site to which the URL should be deleted
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_url("myjob", "github")
#' }
delete_url <- function(name, site) {
  jobs <- job_read()
  jb <- jobs[[name]]

  if(site %in% jb$urls$site) {

    jb$urls <- dplyr::filter(jb$urls, site != {{site}})
    jobs[[name]] <- jb
    job_write(jobs)


  } else {
    warning("Site '", site, "' is not listed in job '", name, "'", call. = FALSE)
  }
}

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
#' job_delete_note("myjob", 2)
#' }
delete_note <- function(name, id) {
  jobs <- job_read()
  jb <- jobs[[name]]

  jb$notes <- dplyr::filter(jb$notes, id != {{id}})
  jobs[[name]] <- jb
  job_write(jobs)
}

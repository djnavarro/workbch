# defines the "delete" family of functions


#' Delete a job
#'
#' @param jobname name of the job to delete
#' @export
#' @details Deletes a job from the JSON file. At the moment, it does so without
#' asking for the user to confirm, so be careful.
#' @examples
#' \dontrun{
#'
#' delete_job("myjob")
#' }
delete_job <- function(jobname) {
  jobs <- job_read()

  # throw error if the job doesn't exist
  if(is.null(jobs[[jobname]])) {
    stop("Job '", jobname, "' does not exist", call. = FALSE)
  }

  # not interactive, just do it
  if(!interactive()) {
    jobs[[jobname]] <- NULL
    job_write(jobs)

  # if interactive, ask user
  } else {

    # make the user confurm deleting
    acc <- ""
    while(acc != "y" & acc != "n") {
      acc <- readline(paste0("Delete the workbch entry for job '", jobname , "'? [y/n] "))
      acc <- tolower(acc)
      acc <- trimws(acc, "both")
    }

    # delete if need be
    if(acc == "y") {
      jobs[[jobname]] <- NULL
      job_write(jobs)
      message("Job '", jobname, "' deleted")
      return(invisible(NULL))
    }

    # tell the user it was aborted
    if(acc == "n") {
      message("Aborted. Nothing deleted.")
      return(invisible(NULL))
    }
  }
}


#' Delete a note from a job
#'
#' @param jobname the job from which the note should be deleted
#' @param id the id number assigned to the note
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_note("myjob", 2)
#' }
delete_note <- function(jobname, id) {
  jobs <- job_read()
  jb <- jobs[[jobname]]

  if(id %in% jb$notes$id) {

    jb$notes <- dplyr::filter(jb$notes, id != {{id}})
    jobs[[jobname]] <- jb
    job_write(jobs)

  } else {
    warning("Note ", id, " does not exist in job '", jobname, "'", call. = FALSE)
  }
}


#' Delete a task from a job
#'
#' @param jobname the job from which the task should be deleted
#' @param id the id number assigned to the task
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_task("myjob", 2)
#' }
delete_task <- function(jobname, id) {
  jobs <- job_read()
  jb <- jobs[[jobname]]

  if(id %in% jb$tasks$id) {

    jb$tasks <- dplyr::filter(jb$tasks, id != {{id}})
    jobs[[jobname]] <- jb
    job_write(jobs)


  } else {
    warning("Task ", id, " does not exist in job '", jobname, "'", call. = FALSE)
  }
}



#' Delete a URL from a job
#'
#' @param jobname the job from which the URL should be deleted
#' @param site the name of the site to which the URL should be deleted
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_url("myjob", "github")
#' }
delete_url <- function(jobname, site) {
  jobs <- job_read()
  jb <- jobs[[jobname]]

  if(site %in% jb$urls$site) {

    jb$urls <- dplyr::filter(jb$urls, site != {{site}})
    jobs[[jobname]] <- jb
    job_write(jobs)


  } else {
    warning("Site '", site, "' is not listed in job '", jobname, "'", call. = FALSE)
  }
}

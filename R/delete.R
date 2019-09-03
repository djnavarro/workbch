# defines the "delete" family of functions


#' Delete a job
#'
#' @param jobname name of the job to delete
#' @export
#' @details Deletes a job from the JSON file.
#' @examples
#' \dontrun{
#'
#' delete_job("myjob")
#' }
delete_job <- function(jobname) {
  jobs <- job_read()

  # check jobname
  verify_jobname(jobname)
  verify_jobexists(jobname, jobs)

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


#' Delete a task from a job
#'
#' @param id the id number assigned to the task
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' delete_task(2, "myjob")
#' }
delete_task <- function(id = NULL) {

  # if the user supplies no input display the tasks and then ask for input:
  tsk <- view_tasks(show_hidden = TRUE)

  if(is.null(id)) {
    if(!interactive()) {
      stop("'id' must be specified when not in interactive mode", call. = FALSE)

    } else {
      print(tsk)
      cat("\n")
      id <- readline("  Enter the id of task to delete... ")
      id <- as.numeric(id)
      if(is.na(id)) {
        stop("Task id must be numeric", call. = FALSE)
      }
    }
  }


  # throw error if this is not a recognised task
  if(!(id %in% tsk$id)) {
    stop("Task ", id, " does not exist", call. = FALSE)
  }

  # read the jobs & verify the name
  jobs <- job_read()
  jobname <- tsk$jobname[tsk$id == id]

  jb <- jobs[[jobname]]

  jb$tasks <- dplyr::filter(jb$tasks, id != {{id}})
  jobs[[jobname]] <- jb
  job_write(jobs)
}



#' Delete a URL from a job
#'
#' @param site the name of the site to which the URL should be deleted
#' @param jobname the job from which the URL should be deleted
#'
#' @export
delete_url <- function(site, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  jobname <- jobname %||% job_getcurrent(jobs)

  # check jobname
  verify_jobname(jobname)
  verify_jobexists(jobname, jobs)

  jb <- jobs[[jobname]]

  if(site %in% jb$urls$site) {

    jb$urls <- dplyr::filter(jb$urls, site != {{site}})
    jobs[[jobname]] <- jb
    job_write(jobs)


  } else {
    warning("Site '", site, "' is not listed in job '", jobname, "'", call. = FALSE)
  }
}

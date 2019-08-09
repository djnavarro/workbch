#' Set the properties of an existing task
#
#' @param id id number of the task to be edited
#' @param description brief description of the task
#' @param status should be "active" (default), "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname (defaults to job owner)
#' @param priority numeric (default is to match the job)
#' @param deadline a date (default is to match the job)
#'
#' @name set_task
NULL

set_task <- function(id, description = NULL, status = NULL, owner = NULL,
                     priority = NULL, deadline = NULL) {

  # find the job and the task
  jobs <- job_read()
  tasks <- task_read()
  ind <- which(tasks$id == id)
  if(length(ind) == 0) {
    stop("no task with id '", id, "' exists", call. = FALSE)
  }
  jobname <- tasks$jobname[ind]

  # ------- task description -------
  if(!is.null(description)) {
    verify_description(description)
    jobs[[jobname]]$tasks$description <- description
  }

  # ------- task status -------
  if(!is.null(status)) {
    verify_status(status)
    jobs[[jobname]]$tasks$status <- status
  }

  # ------- task priority -------
  if(!is.null(priority)) {
    verify_priority(priority)
    jobs[[jobname]]$tasks$priority <- priority
  }

  # ------- task deadline -------
  if(!is.null(deadline)) {
    verify_deadline(deadline)
    deadline <- format_date(deadline)
    jobs[[jobname]]$tasks$deadline <- deadline
  }

  # ------- task description -------
  if(!is.null(owner)) {
    owner <- real_name(owner)
    if(owner %in% jobs[[jobname]]$team) {
      jobs[[jobname]]$tasks$owner <- owner
    } else {
      stop("task owner must be a job team member", call. = FALSE)
    }
  }

  return(jobs)

}


#' @rdname set_task
#' @export
set_task_description <- function(id, description) {
  job_write(set_task(id = id, description = description))
}

#' @rdname set_task
#' @export
set_task_status <- function(id, status) {
  job_write(set_task(id = id, status = status))
}

#' @rdname set_task
#' @export
set_task_owner <- function(id, owner) {
  job_write(set_task(id = id, owner = owner))
}

#' @rdname set_task
#' @export
set_task_priority <- function(id, priority) {
  job_write(set_task(id = id, priority = priority))
}

#' @rdname set_task
#' @export
set_task_deadline <- function(id, deadline) {
  job_write(set_task(id = id, deadline = deadline))
}



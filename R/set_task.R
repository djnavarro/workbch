#' Set the properties of an existing task
#
#' @param id unique id number for the task
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
  id <- task_getid(id, tasks)
  jobname <- tasks$jobname[which(tasks$id == id)]

  # ------- task description -------
  if(is_set(description)) {
    verify_description(description)
    jobs[[jobname]]$tasks$description <- description
  }

  # ------- task status -------
  if(is_set(status)) {
    verify_status(status)
    jobs[[jobname]]$tasks$status <- status
  }

  # ------- task priority -------
  if(is_set(priority)) {
    verify_priority(priority)
    jobs[[jobname]]$tasks$priority <- priority
  }

  # ------- task deadline -------
  if(is_set(deadline)) {
    verify_deadline(deadline)
    deadline <- format_date(deadline)
    jobs[[jobname]]$tasks$deadline <- deadline
  }

  # ------- task description -------
  if(is_set(owner)) {
    owner <- ppl_fullname(owner)
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
set_task_description <- function(id = NULL, description = NULL) {
  if(is.null(id)) {
    id <- prompt_taskid(task_read())
    description <- readline("  Enter new task description... ")
  }
  job_write(set_task(id = id, description = description))
}

#' @rdname set_task
#' @export
set_task_status <- function(id = NULL, status = NULL) {
  if(is.null(id)) {
    id <- prompt_taskid(task_read())
    status <- readline("  Enter new task status... ")
  }
  job_write(set_task(id = id, status = status))
}

#' @rdname set_task
#' @export
set_task_owner <- function(id = NULL, owner = NULL) {
  if(is.null(id)) {
    id <- prompt_taskid(task_read())
    owner <- readline("  Enter new task owner... ")
  }
  job_write(set_task(id = id, owner = owner))
}

#' @rdname set_task
#' @export
set_task_priority <- function(id = NULL, priority = NULL) {
  if(is.null(id)) {
    id <- prompt_taskid(task_read())
    priority <- readline("  Enter new task priority... ")
  }
  job_write(set_task(id = id, priority = priority))
}

#' @rdname set_task
#' @export
set_task_deadline <- function(id = NULL, deadline = NULL) {
  if(is.null(id)) {
    id <- prompt_taskid(task_read())
    deadline <- readline("  Enter new task deadline... ")
  }
  job_write(set_task(id = id, deadline = deadline))
}



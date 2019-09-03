#' Set the properties of an existing task
#
#' @param ref either a number (id) or a string to be matched against
#' @param description brief description of the task
#' @param status should be "active" (default), "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname (defaults to job owner)
#' @param priority numeric (default is to match the job)
#' @param deadline a date (default is to match the job)
#'
#' @name set_task
NULL

set_task <- function(ref, description = NULL, status = NULL, owner = NULL,
                     priority = NULL, deadline = NULL) {

  # find the job and the task
  jobs <- job_read()
  tasks <- task_read()
  id <- task_getid(ref, tasks)
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
set_task_description <- function(ref = NULL, description = NULL) {
  if(is.null(ref)) {
    ref <- prompt_taskref(task_read())
    description <- readline("  Enter new task description... ")
  }
  job_write(set_task(ref = ref, description = description))
}

#' @rdname set_task
#' @export
set_task_status <- function(ref = NULL, status = NULL) {
  if(is.null(ref)) {
    ref <- prompt_taskref(task_read())
    status <- readline("  Enter new task status... ")
  }
  job_write(set_task(ref = ref, status = status))
}

#' @rdname set_task
#' @export
set_task_owner <- function(ref = NULL, owner = NULL) {
  if(is.null(ref)) {
    ref <- prompt_taskref(task_read())
    owner <- readline("  Enter new task owner... ")
  }
  job_write(set_task(ref = ref, owner = owner))
}

#' @rdname set_task
#' @export
set_task_priority <- function(ref = NULL, priority = NULL) {
  if(is.null(ref)) {
    ref <- prompt_taskref(task_read())
    priority <- readline("  Enter new task priority... ")
  }
  job_write(set_task(ref = ref, priority = priority))
}

#' @rdname set_task
#' @export
set_task_deadline <- function(ref = NULL, deadline = NULL) {
  if(is.null(ref)) {
    ref <- prompt_taskref(task_read())
    deadline <- readline("  Enter new task deadline... ")
  }
  job_write(set_task(ref = ref, deadline = deadline))
}



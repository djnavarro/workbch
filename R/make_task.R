
# constructor function for task objects
new_task <- function(name, id, description, owner, status = "active",
                     priority = 1, deadline = NA, hidden = FALSE) {
  tibble::tibble(
    name = name,
    id = id,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    deadline = deadline,
    hidden = hidden
  )
}


empty_task <- function() {
  new_task(name = character(0), id = numeric(0), description = character(0),
           owner = character(0), status = character(0), priority = numeric(0),
           deadline = as.Date(character(0)), hidden = logical(0))
}

#' Attach a new task to job
#'
#' @param jobname name of the job the task attaches to
#' @param description brief description of the task
#' @param status should be "active" (default), "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname (defaults to job owner)
#' @param priority numeric (default is to match the job)
#' @param deadline a date (default is to match the job)
#' @param hidden hide the task (default is to match the job)
#' @export
job_addtask <- function(jobname, description, owner = NULL, status = "active",
                        priority = NULL, deadline = NULL, hidden = NULL) {

  # read the jobs
  jobs <- job_read()
  jb <- jobs[[jobname]]

  # throw error if the job doesn't exist
  if(is.null(jb)) {
    stop("there is no job named '", jobname, "'", call. = FALSE)
  }

  # set defaults as needed
  if(is.null(owner)) {owner <- jb$owner}
  if(is.null(priority)) {priority <- jb$priority}
  if(is.null(deadline)) {deadline <- jb$deadline}
  if(is.null(hidden)) {hidden <- jb$hidden}

  # parse the owner name and throw warning if not in team
  owner <- real_name(owner)
  if(!(owner %in% jb$team)) {
    warning("'", owner, "' is not on the team for '", jobname, "'", call. = FALSE)
  }

  # assign the task a unique id number
  id <- current_max_task_id(jobs) + 1

  # create the task object
  tsk <- new_task(name = jobname, id = id, description = description,
                  owner = owner, status = status, priority = priority,
                  deadline = deadline, hidden = hidden)

  # append it to the job
  if(identical(jb$tasks, list())) {
    jb$tasks <- tsk
  } else if(nrow(jb$tasks) == 0) {
    jb$tasks <- tsk
  } else {
    jb$tasks <- dplyr::bind_rows(jb$tasks, tsk)
  }

  # write it to the jobs list
  jobs[[jobname]] <- jb
  job_write(jobs)
}

# retrieve the largest known task number
current_max_task_id <- function(jobs) {
  task_ids <- purrr::map_dbl(jobs, function(j) {
    tsk <- j$tasks
    if(!is.null(tsk) & length(tsk$id) > 0) {
      return(max(tsk$id))
    }
    return(0)
  })
  return(max(task_ids))
}


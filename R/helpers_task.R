
# retrieve the largest known task number
task_maxid <- function(jobs) {
  if(is.null(jobs)) { return(0) }
  task_ids <- purrr::map_dbl(jobs, function(j) {
    tsk <- j$tasks
    if(!is.null(tsk) & length(tsk$id) > 0) {
      return(max(tsk$id))
    }
    return(0)
  })
  return(max(task_ids))
}

# read the tasks
task_read <- function() {
  jobs <- job_read()
  if(is.null(jobs)) {
    tasks <- empty_task()
  } else {
    tasks <- purrr::map_dfr(jobs, function(j) {j$tasks})
  }
  return(tasks)
}

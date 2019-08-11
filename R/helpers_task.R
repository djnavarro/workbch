
# returns a vector of all task id numbers
task_getids <- function(jobs) {
  addid <- function(id, j) {
    if(is.null(j$tasks$id)) {return(id)}
    return(c(id, j$tasks$id))
  }
  ids <- purrr::reduce(jobs, addid, .init = numeric(0))
  return(ids)
}

# make new id value by incrementing maxid (fix this)
task_makeid <- function(ids) {
  max(ids) + 1
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

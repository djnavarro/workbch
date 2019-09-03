
# probably redundant now that soft matching is removed
task_getid <- function(id, tasks) {
  if(!is.numeric(id)) {
    stop("'id' must be numeric", call. = FALSE)
  }
  if(length(which(tasks$id == id)) == 0) {
    stop("no task with id '", id, "' exists", call. = FALSE)
  }
  return(id)
}

# returns a vector of all task id numbers
task_ids <- function(jobs) {
  purrr::reduce(
    .x = jobs,
    .f = function(id, j) { c(id, (j$tasks$id %||% numeric(0L))) },
    .init = numeric(0)
  )
}

# make new id value by incrementing maxid (fix this)
task_makeid <- function(ids) {
  max(ids) + 1
}

# append a new task to an existing list of tasks
task_append <- function(old, new) {
  if(identical(old, list())) { return(new) }
  if(nrow(old) == 0) { return(new) }
  return(dplyr::bind_rows(old, new))
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


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

# the directory where project records should be stored
job_home <- function() {
  getOption("projectr.home")
}

job_file <- function() {
  file.path(job_home(), "projectr.json")
}

# read project data from JSON file if it exists
job_read <- function() {
  if(file.exists(job_file())) {
    return(jsonlite::fromJSON(job_file()))
  }
  return(list())
}

# write project data to JSON file
job_write <- function(jobs) {
  job_str <- jsonlite::toJSON(jobs, pretty = TRUE)
  writeLines(job_str, job_file())
}

#' Create a new project
#'
#' @param name name of the project to create
#' @export
job_create <- function(name, ...) {
  jobs <- job_read()
  jobs[[name]] <- new_job(name = name, ...)
  job_write(jobs)
}

# constructor function for job objects
new_job <- function(name, description = "", status = "active", owner = "me",
                    member = "me", priority = 1, deadline = NA, note = list(),
                    task = list()) {
  list(
    name = name,
    description = description,
    status = status,
    owner = owner,
    member = member,
    priority = priority,
    deadline = deadline,
    task = task,
    note = note
  )
}

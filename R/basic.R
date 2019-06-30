# the directory where project records should be stored
job_home <- function() {
  getOption("projectr.home")
}

job_file <- function() {
  file.path(job_home(), "projectr_jobs.json")
}

ppl_file <- function() {
  file.path(job_home(), "projectr_people.csv")
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
#' @param ... other fields
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

#' List jobs
#'
#' @export
job_list <- function() {
  jobs <- job_read()
  cat(names(jobs), sep = "\n")
}

#' List jobs
#'
#' @param name Name of job to delete
#' @export
job_delete <- function(name) {
  jobs <- job_read()
  jobs[[name]] <- NULL
  job_write(jobs)
}


#' Show the details of a job
#'
#' @param name Name of job to display
#' @export
job_show <- function(name) {
  jobs <- job_read()
  print(jobs[[name]])
}



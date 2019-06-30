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
#' @param description brief description of the project
#' @param status should be "active", "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname
#' @param team should be a vector of names/nicknames
#' @param priority numeric
#' @param deadline a date
#' @param notes list of notes
#' @param tasks list of tasks
#' @export
job_create <- function(name, description, owner, status = "active",
                       team = character(0), priority = 1, deadline = NA, notes = list(),
                       tasks = list()) {

  # parse the names and make sure the owner is on the team
  owner <- real_name(owner)
  team <- real_name(team)
  if(!(owner %in% team)) {
    team <- c(owner, team)
  }

  jobs <- job_read()
  jobs[[name]] <- new_job(name = name, description = description, owner = owner,
                          status = status, team = team, priority = priority,
                          deadline = deadline, notes = notes, tasks = tasks)
  job_write(jobs)
}

# constructor function for job objects
new_job <- function(name, description, owner, status = "active",
                    team = character(0), priority = 1, deadline = NA,
                    notes = list(), tasks = list()) {
  list(
    name = name,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    deadline = deadline,
    tasks = tasks,
    notes = notes
  )
}

#' List jobs
#'
#' @export
job_list <- function() {
  jobs <- job_read()
  cat(names(jobs), sep = "\n")
}

#' Delete a job
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



# the directory where project records should be stored
job_home <- function() {
  getOption("workbch.home")
}

job_file <- function() {
  file.path(job_home(), "workbch_jobs.json")
}

ppl_file <- function() {
  file.path(job_home(), "workbch_people.csv")
}

# read project data from JSON file if it exists
job_read <- function() {
  if(file.exists(job_file())) {
    return(jsonlite::fromJSON(job_file()))
  }
  return(list())
}

# read the tasks
task_read <- function() {
  jobs <- job_read()
  tasks <- purrr::map_dfr(jobs, function(j) {j$tasks})
  return(tasks)
}

# write project data to JSON file
job_write <- function(jobs) {
  job_str <- jsonlite::toJSON(jobs, pretty = TRUE)
  writeLines(job_str, job_file())
}

# read people data from CSV file if it exists
ppl_read <- function() {
  if(file.exists(ppl_file())) {
    return(suppressMessages(readr::read_csv(ppl_file())))
  }
  return(list())
}

# write people data to CSV file
ppl_write <- function(ppl) {
  readr::write_csv(ppl, ppl_file())
}

# returns a list of expressions
capture_dots <- function(...) {
  as.list(substitute(list(...)))[-1L]
}

# check the job
validate_job <- function(job) {

  # more checks here!!!

  # if the owner is not on the team, add them
  if(!(job$owner %in% job$team)) {
    job$team <- c(job$owner, job$team)
  }

  # if it lacks a hidden attribute, set it as false
  if(is.null(job$hidden)) {
    job$hidden <- FALSE
  }

  return(job)
}

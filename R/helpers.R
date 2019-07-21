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

# read project data from JSON file if it exists, and preprocess to deal with
# limitations to JSON storage
job_read <- function() {
  if(file.exists(job_file())) {

    jobs <- jsonlite::fromJSON(job_file())
    jobs <- purrr::map(jobs, function(j) {

      # empty lists become data frames
      if(class(j$urls) == "list") {j$urls <- empty_url()}
      if(class(j$notes) == "list") {j$notes <- empty_note()}
      if(class(j$tasks) == "list") {j$tasks <- empty_task()}

      # coerce to tibbles
      j$urls <- tibble::as_tibble(j$urls)
      j$notes <- tibble::as_tibble(j$notes)
      j$tasks <- tibble::as_tibble(j$tasks)

      return(j)
    })
    return(jobs)
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

  return(job)
}


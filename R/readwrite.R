
# returns the path to the workbch jobs file
job_file <- function() {
  file.path(getOption("workbch.home"), "workbch_jobs.json")
}

# write project data to JSON file (TODO: this can be condensed to
# jsonlite::writeJSON presumably)
job_write <- function(jobs) {
  job_str <- jsonlite::toJSON(jobs, pretty = TRUE)
  writeLines(job_str, job_file())
}

# read project data from JSON file, and preprocess to deal with
# limitations to JSON storage
job_read <- function() {
  if(file.exists(job_file())) {

    jobs <- jsonlite::fromJSON(job_file())
    jobs <- purrr::map(jobs, function(j) {

      # empty lists become data frames in some cases
      if(class(j$urls) == "list") {j$urls <- empty_url()}

      # empty lists become character vectors in others
      if(class(j$team) == "list") {j$team <- character(0)}
      if(class(j$tags) == "list") {j$tags <- character(0)}

      if(class(j$path) == "logical") {
        j$path <- as.character(j$path)
      }

      # don't let tags become matrices
      if(class(j$tags) == "matrix") {j$tags <- as.vector(j$tags)}

      # coerce to tibbles
      j$urls <- tibble::as_tibble(j$urls)

      return(j)
    })
    return(jobs)
  }
  return(NULL)
}

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
job_write <- function(prj) {
  job_str <- jsonlite::toJSON(prj, pretty = TRUE)
  writeLines(job_str, job_file())
}

#' Create a new project
#'
#' @param name name of the project to create
#' @export
job_create <- function(name) {
  prj <- job_read()
  prj[[name]] <- list(name = name)
  job_write(prj)
}

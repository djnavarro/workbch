# the directory where project records should be stored
project_home <- function() {
  getOption("protrack.home")
}

project_file <- function() {
  file.path(project_home(), "protrack.json")
}

# read project data from JSON file if it exists
project_read <- function() {
  if(file.exists(project_file())) {
    return(jsonlite::fromJSON(project_file()))
  }
  return(list())
}

# write project data to JSON file
project_write <- function(prj) {
  prj_str <- jsonlite::toJSON(prj, pretty = TRUE)
  writeLines(prj_str, project_file())
}

#' Create a new project
#'
#' @param name name of the project to create
#' @export
project_create <- function(name) {
  prj <- project_read()
  prj[[name]] <- list(name = name)
  project_write(prj)
}

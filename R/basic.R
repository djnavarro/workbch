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
#' @param urls list of urls
#' @param notes list of notes
#' @param tasks list of tasks
#' @export
job_create <- function(name, description, owner, status = "active",
                       team = character(0), priority = 1, deadline = NA,
                       urls = list(), notes = list(), tasks = list()) {

  # parse the names and make sure the owner is on the team
  owner <- real_name(owner)
  team <- real_name(team)
  if(!(owner %in% team)) {
    team <- c(owner, team)
  }

  # read the jobs data and append the new job
  jobs <- job_read()
  jobs[[name]] <- new_job(name = name, description = description, owner = owner,
                          status = status, team = team, priority = priority,
                          deadline = deadline, urls = urls, notes = notes, tasks = tasks)
  job_write(jobs)
}

# returns a list of expressions
capture_dots <- function(...) {
  as.list(substitute(list(...)))[-1L]
}

#' Edit a job
#'
#' @param name name of the project to edit
#' @param ... expressions to be evaluated in the job
#' @export
job_edit <- function(name, ...) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  j <- jobs[[name]]
  j[names(dots)] <- dots
  jobs[[name]] <- j

  # write
  job_write(jobs)
}


#' Edit the urls in a job
#'
#' @param name name of the project to edit
#' @param ... expressions to be evaluated within the urls field
#' @export
job_edit_urls <- function(name, ...) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  l <- jobs[[name]]$urls
  l[names(dots)] <- dots
  jobs[[name]]$urls <- l

  # write
  job_write(jobs)
}


#' Open a url linked to a job
#'
#' @param name name of the project
#' @param site label denoting the site (e.g., "github")
#' @export
job_browse_url <- function(name, site) {

  jobs <- job_read()
  url <- jobs[[name]]$url[[site]]
  utils::browseURL(url)

}






# constructor function for job objects
new_job <- function(name, description, owner, status = "active",
                    team = character(0), priority = 1, deadline = NA,
                    urls = list(), notes = list(), tasks = list()) {
  list(
    name = name,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    deadline = deadline,
    urls = urls,
    tasks = tasks,
    notes = notes
  )
}

#' List jobs
#'
#' @param ... expression to be passed to dplyr::filter
#' @export
job_list <- function(...) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("name", "owner", "priority", "status", "deadline", "description")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, name)
  if(!is.null(filter)) {
    job_tbl <- dplyr::filter(job_tbl, ...)
  }
  print(job_tbl)
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


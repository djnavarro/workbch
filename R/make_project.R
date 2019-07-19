
#' Create a new project
#'
#' @param name name of the project to create
#' @param description brief description of the project
#' @param status should be "active", "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname
#' @param team should be a vector of names/nicknames
#' @param priority numeric
#' @param deadline a date
#' @param path path to the project home directory
#' @param urls list of urls
#' @param notes list of notes
#' @param tasks list of tasks
#' @param hidden hide project (default = FALSE)
#' @export
job_create <- function(name, description, owner, status = "active",
                       team = character(0), priority = 1, deadline = NA,
                       path = NA, urls = list(), notes = list(), tasks = list(),
                       hidden = FALSE) {

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
                          deadline = deadline, path = path, urls = urls,
                          notes = notes, tasks = tasks, hidden = hidden)
  job_write(jobs)
}



# constructor function for job objects
new_job <- function(name, description, owner, status = "active",
                    team = character(0), priority = 1, deadline = NA,
                    path = NA, urls = list(), notes = list(), tasks = list(),
                    hidden = hidden) {
  list(
    name = name,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    deadline = deadline,
    path = path,
    urls = urls,
    tasks = tasks,
    notes = notes,
    hidden = hidden
  )
}


# constructor functions for jobs, tasks, notes and urls. also contains
# helper functions specifying empty tasks, notes and urls. none of these
# functions should be exported

new_job <- function(jobname, description, owner, status = "active",
                    team = character(0), priority = 1, deadline = NA,
                    path = NA, urls = NULL, notes = NULL,
                    tasks = NULL, hidden = FALSE) {

  if(is.null(urls)) {urls = empty_url()}
  if(is.null(notes)) {notes = empty_note()}
  if(is.null(tasks)) {tasks = empty_task()}

  list(
    jobname = jobname,
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

new_task <- function(jobname, id, description, owner, status = "active",
                     priority = 1, deadline = NA, hidden = FALSE) {
  tibble::tibble(
    jobname = jobname,
    id = id,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    deadline = deadline,
    hidden = hidden
  )
}

new_note <- function(jobname, note, id = NA, date = as.character(Sys.Date())) {
  tibble::tibble(
    jobname = jobname,  # name of the job it is linked to
    note = note,  # the message text
    id = id,      # unique identifier
    date = date   # the time stamp
  )
}

new_url <- function(site = character(0), link = character(0)) {
  tibble::tibble(
    site = site,
    link = link
  )
}

empty_task <- function() {
  new_task(jobname = character(0), id = numeric(0), description = character(0),
           owner = character(0), status = character(0), priority = numeric(0),
           deadline = as.Date(character(0)), hidden = logical(0))
}

empty_note <- function() {
  new_note(jobname = character(0), note = character(0), id = numeric(0),
           date = character(0))
}

empty_url <- function() {
  new_url(site = character(0), link = character(0))
}


empty_job <- function() {
  list()
}

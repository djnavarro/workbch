# constructor functions for jobs, tasks, and urls. also contains
# helper functions specifying empty tasks, and urls. none of these
# functions should be exported

new_job <- function(jobname, description, owner, status = "active",
                    team = character(0), priority = 1, deadline = NA,
                    tags = character(0), path = NA, urls = NULL,
                    tasks = NULL, hidden = FALSE) {

  if(is.null(urls)) {urls = empty_url()}
  if(is.null(tasks)) {tasks = empty_task()}

  list(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    deadline = deadline,
    tags = tags,
    path = path,
    urls = urls,
    tasks = tasks,
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

new_url <- function(site = character(0), link = character(0)) {
  tibble::tibble(
    site = site,
    link = link
  )
}

empty_task <- function() {
  new_task(jobname = character(0), id = numeric(0), description = character(0),
           owner = character(0), status = character(0), priority = numeric(0),
           deadline = character(0), hidden = logical(0))
}

empty_url <- function() {
  new_url(site = character(0), link = character(0))
}


empty_job <- function() {
  list()
}

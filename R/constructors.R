# constructor functions for jobs, tasks, and urls.


# constructors for new objects with default values ------------------------

new_job <- function(jobname, description, owner, status = NULL,
                    team = NULL, priority = NULL, deadline = NULL,
                    tags = NULL, path = NULL, urls = NULL,
                    tasks = NULL) {

  # replace nulls with default values
  if(is.null(status)) {status <- "active"}
  if(is.null(team)) {team <- character(0)}
  if(is.null(priority)) {priority <- 1}
  if(is.null(deadline)) {deadline <- NA_character_}
  if(is.null(tags)) {tags <- character(0)}
  if(is.null(path)) {path <- NA_character_}
  if(is.null(urls)) {urls = empty_url()}
  if(is.null(tasks)) {tasks = empty_task()}

  # return list
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
    tasks = tasks
  )
}

new_task <- function(jobname, id, description, owner, status = NULL,
                     priority = NULL, deadline = NULL) {

  # replace nulls with default values
  if(is.null(status)) {status <- "active"}
  if(is.null(priority)) {priority <- 1}
  if(is.null(deadline)) {deadline <- NA_character_}


  tibble::tibble(
    jobname = jobname,
    id = id,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    deadline = deadline
  )
}

new_url <- function(site = character(0), link = character(0)) {
  tibble::tibble(
    site = site,
    link = link
  )
}



# constructors for empty objects ------------------------------------------

empty_task <- function() {
  new_task(jobname = character(0), id = numeric(0), description = character(0),
           owner = character(0), status = character(0), priority = numeric(0),
           deadline = character(0))
}

empty_url <- function() {
  new_url(site = character(0), link = character(0))
}


empty_job <- function() {
  new_job(jobname = character(0), description = character(0),
          owner = character(0), status = character(0), team = character(0),
          priority = numeric(0), deadline = character(0), tags = character(0),
          path = character(0), urls = empty_url(), tasks = empty_task())
}

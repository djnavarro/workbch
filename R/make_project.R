
#' Set a new job, or modify an existing one
#'
#' @param name name of the job to create
#' @param description brief description of the job
#' @param status should be "active", "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname
#' @param team should be a vector of names/nicknames
#' @param priority numeric
#' @param deadline a date
#' @param path path to the project home directory
#' @param hidden hide job (default = FALSE)
#' @export
set_job <- function(name, description = NULL, owner = NULL, status = NULL,
                    team = NULL, priority = NULL, deadline = NULL,
                    path = NULL, hidden = NULL) {

  # read jobs file and check the names of the jobs
  jobs <- job_read()
  job_names <- purrr::map_chr(jobs, function(j) {j$name})

  # if it doesn't exist, create the job
  if(!(name %in% job_names)) {

    # check for mandatory fields
    if(is.null("description")) {
      stop("new jobs must have a description", call. = FALSE)
    }
    if(is.null("owner")) {
      stop("new jobs must have an owner", call. = FALSE)
    }

    # specify the defaults for other fields
    if(is.null(status)) {status <- "active"}
    if(is.null(team)) {team <- character(0)}
    if(is.null(priority)) {priority <- 1}
    if(is.null(deadline)) {deadline <- NA_character_}
    if(is.null(path)) {path <- NA_character_}
    if(is.null(hidden)) {hidden <- FALSE}

    # parse the names and make sure the owner is on the team
    owner <- real_name(owner)
    team <- real_name(team)
    if(!(owner %in% team)) {
      team <- c(owner, team)
    }

    # append the new job
    jobs[[name]] <- new_job(
      name = name,
      description = description,
      owner = owner,
      status = status,
      team = team,
      priority = priority,
      deadline = deadline,
      path = path,
      urls = empty_url(),
      notes = empty_note(),
      tasks = empty_task(),
      hidden = hidden
    )

  } else {

    job <- jobs[[name]]

    # edit fields per user specification
    if(!is.null(owner)) {job$owner <- real_name(owner)}
    if(!is.null(team)) {job$team <- real_name(team)}
    if(!is.null(status)) {job$status <- status}
    if(!is.null(priority)) {job$priority <- priority}
    if(!is.null(deadline)) {job$deadline <- deadline}
    if(!is.null(path)) {job$path <- path}
    if(!is.null(hidden)) {job$hidden <- hidden}

    # ensure that the edits produce a valid job state
    jobs[[name]] <- validate_job(job)

  }

  # write the file and return
  job_write(jobs)
}




new_url <- function(site = character(0), link = character(0)) {
  tibble::tibble(
    site = site,
    link = link
  )
}

empty_url <- function() {
  new_url(site = character(0), link = character(0))
}


# constructor function for job objects
new_job <- function(name, description, owner, status = "active",
                    team = character(0), priority = 1, deadline = NA,
                    path = NA, urls = NULL, notes = NULL,
                    tasks = NULL, hidden = FALSE) {

  if(is.null(urls)) {urls = empty_url()}
  if(is.null(notes)) {notes = empty_note()}
  if(is.null(tasks)) {tasks = empty_task()}

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


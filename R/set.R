# defines the "set" family of functions

#' Set a new job, or modify an existing one
#'
#' @param name name of the job to create
#' @param description brief description of the job
#' @param status should be "active", "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname
#' @param team should be a vector of names/nicknames
#' @param priority numeric
#' @param deadline a date
#' @param path path to the job home directory
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


#' Set a task attached to a job
#'
#' @param jobname name of the job the task attaches to
#' @param description brief description of the task
#' @param status should be "active" (default), "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname (defaults to job owner)
#' @param priority numeric (default is to match the job)
#' @param deadline a date (default is to match the job)
#' @param hidden hide the task (default is to match the job)
#' @export
set_task <- function(jobname, description, owner = NULL, status = "active",
                     priority = NULL, deadline = NULL, hidden = NULL) {

  # read the jobs
  jobs <- job_read()
  jb <- jobs[[jobname]]

  # throw error if the job doesn't exist
  if(is.null(jb)) {
    stop("there is no job named '", jobname, "'", call. = FALSE)
  }

  # set defaults as needed
  if(is.null(owner)) {owner <- jb$owner}
  if(is.null(priority)) {priority <- jb$priority}
  if(is.null(deadline)) {deadline <- jb$deadline}
  if(is.null(hidden)) {hidden <- jb$hidden}

  # parse the owner name and throw warning if not in team
  owner <- real_name(owner)
  if(!(owner %in% jb$team)) {
    warning("'", owner, "' is not on the team for '", jobname, "'", call. = FALSE)
  }

  # assign the task a unique id number
  id <- current_max_task_id(jobs) + 1

  # create the task object
  tsk <- new_task(name = jobname, id = id, description = description,
                  owner = owner, status = status, priority = priority,
                  deadline = deadline, hidden = hidden)

  # append it to the job
  if(identical(jb$tasks, list())) {
    jb$tasks <- tsk
  } else if(nrow(jb$tasks) == 0) {
    jb$tasks <- tsk
  } else {
    jb$tasks <- dplyr::bind_rows(jb$tasks, tsk)
  }

  # write it to the jobs list
  jobs[[jobname]] <- jb
  job_write(jobs)
}



#' Set the location of workbch files
#'
#' @param path Path to the folder
#'
#' @return Path to the folder
#' @export
set_home <- function(path = NULL) {
  if(!is.null(path)) {
    options(workbch.home = path)
  }
  job_home()
}




#' Set the members of a team
#'
#' @param name name of job to be edited
#' @param add character vector of names to add to the team
#' @param remove character vector of names to remove from the team
#' @details The role of \code{set_team()} is to make it a little easier to
#' alter the set of names listed in the "team" field of a job. The same task
#' could be done with \code{set_job()} but it is cumbersome to do so. The
#' function first appends any names in \code{add} to the team (ignoring
#' duplicates), and then removes any names listed in \code{remove}. The function
#' checks all elements of \code{add} and \code{remove} against the table of
#' known nicknames, and substitutes the full name in place of a nickname.
#' Note that it is not possible to remove the "owner" of a job using this
#' method, as this would leave the job without an owner. To remove the owner,
#' first use \code{set_job()} to change the owner, and then use
#' \code{set_team()} to remove the former owner from the team.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' set_team("myjob", add = "hayley")
#' set_team("myjob", add = c("hayley", "sarah"))
#' set_team("myjob", remove = c("hayley", "sarah"))
#'
#' # if "hayley" is the current owner"
#' set_job("myjob", owner = "sarah") # transfers the ownership to sarah
#' set_team("myjob", remove = "hayley") # removes hayley entirely
#' }
set_team <- function(name, add = NULL, remove = NULL) {

  jobs <- job_read()

  if(!is.null(add)) {
    add <- real_name(add)
    jobs[[name]]$team <- unique(c(jobs[[name]]$team, add))
  }

  if(!is.null(remove)) {
    remove <- real_name(remove)

    # cannot remove owner
    if(jobs[[name]]$owner %in% remove) {
      warning(
        "cannot remove owner from a team: use job_edit() to change owner first",
        call. = FALSE
      )
      remove <- setdiff(remove, jobs[[name]]$owner)
    }

    jobs[[name]]$team <- setdiff(jobs[[name]]$team, remove)
  }

  job_write(jobs)
}




#' Set a URL associated with a job
#'
#' @param name name of the job to edit
#' @param site string with the site nickname (e.g., "github")
#' @param link string with the link to the site
#' @export
#' @details The role of \code{job_edit_url()} is to make it a easier to
#' change a URL associated with a job.
#' @examples
#' \dontrun{
#'
#' set_url("myjob", "github", "https://github.com/myusername/myrepo")
#'
#' }
#
set_url <- function(name, site, link) {

  # read the jobs data
  jobs <- job_read()
  urls <- jobs[[name]]$urls

  # add or overwrite the url
  ind <- which(urls$site == site)
  if(length(ind) == 0) {
    urls <- dplyr::bind_rows(urls, new_url(site, link))
  } else if(length(ind) == 1) {
    urls$link[ind] <- link
  } else {
    stop("how did i get here???")
  }

  # arrange alphabetically and reinsert
  urls <- dplyr::arrange(urls, site)
  jobs[[name]]$urls <- urls

  # ensure that the edits produce a valid job state
  jobs[[name]] <- validate_job(jobs[[name]])

  # write
  job_write(jobs)
}



#' Set a note linked to a job
#'
#' @param name the job to which the note should be added
#' @param note the text of the note
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' set_note("myjob", "susan wanted me to notify her when done")
#' }
set_note <- function(name, note) {
  jobs <- job_read()
  jb <- jobs[[name]]

  if(is.null(dim(jb$notes))) {
    nt <- new_note(name, note, id = 1)
  } else if(nrow(jb$notes) == 0) {
    nt <- new_note(name, note, id = 1)
  } else {
    id <- max(jb$notes$id) + 1
    nt <- new_note(name, note, id = id)
  }

  jb$notes <- dplyr::bind_rows(jb$notes, nt)
  jb$notes <- dplyr::arrange(jb$notes, dplyr::desc(date), dplyr::desc(id))
  jobs[[name]] <- jb
  job_write(jobs)
}




#' Sets details for a new person
#'
#' @param name the name of the person
#' @param nickname a nickname for the person
#' @export
set_person <- function(name, nickname) {
  ppl <- ppl_read()
  ppl <- dplyr::bind_rows(
    ppl,
    tibble::tibble(
      name = name,
      nickname = nickname
    ))
  ppl_write(ppl)
}

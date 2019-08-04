#' Set the properties of an existing job
#'
#' @param jobname the current name for the job
#' @param newname the new name
#' @param description the new description
#' @param owner nick name for the new owner
#' @param status the new status
#' @param priority the new priority
#' @param path the new path to the job folder
#' @param deadline new deadline, string to be parsed by lubridate::dmy
#' @param add_tag character vector of tags to add to jobs
#' @param remove_tag character vector of tags to remove from job
#' @param site string with the site nickname (e.g., "github")
#' @param link string with the link to the site
#' @param add_team character vector of nick names to add to the team
#' @param remove_team character vector of nick names to remove from the team
#'
#' @name set_job
NULL

set_job <- function(
  jobname, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, deadline = NULL,
  add_tag = NULL, remove_tag = NULL, site = NULL, link = NULL, add_team = NULL,
  remove_team = NULL
){

  jobs <- job_read()
  verify_jobname(jobname, jobs)

  # ------- job name -------
  if(!is.null(newname)) {

    # don't let the user overwrite an existing job
    if(newname %in% job_names) {
      stop("job '", newname, "' already exists", call. = FALSE)
    }

    # rename the list entry itself
    job_names <- names(jobs)
    ind <- which(job_names == jobname)
    names(jobs)[ind] <- newname

    # rename within the field
    jobs[[newname]]$jobname <- newname
  }


  # ------- job description -------
  if(!is.null(description)) {
    jobs[[jobname]]$description <- description
  }


  # ------- job status -------
  if(!is.null(status)) {
    jobs[[jobname]]$status <- status
  }


  # ------- job owner -------
  if(!is.null(owner)) {
    jb <- jobs[[jobname]]
    jb$owner <- real_name(owner)

    # add new owner to team if needed
    if(!(jb$owner %in% jb$team)) {
      jb$team <- c(jb$owner, jb$team)
    }

    # write new values
    jobs[[jobname]] <- jb
  }


  # ------- job priority -------
  if(!is.null(priority)) {
    jobs[[jobname]]$priority <- priority
  }


  # ------- job path -------
  if(!is.null(path)) {
    jobs[[jobname]]$path <- path
  }

  # ------- job deadline -------
  if(!is.null(deadline)) {
    deadline <- format_date(deadline)
    jobs[[jobname]]$deadline <- deadline
  }


  # ------- job team -------
  if(!is.null(add_team) | !is.null(remove_team)) {

    if(!is.null(add_team)) {
      add_team <- real_name(add_team)
      jobs[[jobname]]$team <- unique(c(jobs[[jobname]]$team, add_team))
    }

    if(!is.null(remove_team)) {
      remove_team <- real_name(remove_team)
      if(jobs[[jobname]]$owner %in% remove_team) {
        warning("set_job_team() cannot remove owner from a team", call. = FALSE)
        remove_team <- setdiff(remove_team, jobs[[jobname]]$owner)
      }
      jobs[[jobname]]$team <- setdiff(jobs[[jobname]]$team, remove_team)
    }
  }


  # ------- job url -------
  if(!is.null(site)) {

    # get the urls
    urls <- jobs[[jobname]]$urls

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
    jobs[[jobname]]$urls <- urls
  }

  if(!is.null(add_tag) | !is.null(remove_tag)) {
    for(jbnm in jobname) {

      # check the name
      verify_jobname(jbnm, jobs)

      # if there are tags to add, add them
      if(!is.null(add_tag)) {
        jobs[[jbnm]]$tags <- unique(c(jobs[[jbnm]]$tags, add_tag))
      }

      # if there are tags to remove, remove them
      if(!is.null(remove_tag)) {
        jobs[[jbnm]]$tags <- setdiff(jobs[[jbnm]]$tags, remove_tag)
      }
    }
  }

  return(jobs)
}


#' @rdname set_job
#' @export
set_job_name <- function(jobname, newname) {
  job_write(set_job(jobname = jobname, newname = newname))
}

#' @rdname set_job
#' @export
set_job_description <- function(jobname, description) {
  job_write(set_job(jobname = jobname, description = description))
}

#' @rdname set_job
#' @export
set_job_status <- function(jobname, status) {
  job_write(set_job(jobname = jobname, status = status))
}

#' @rdname set_job
#' @export
set_job_owner <- function(jobname, owner) {
  job_write(set_job(jobname = jobname, owner = owner))
}

#' @rdname set_job
#' @export
set_job_priority <- function(jobname, priority) {
  job_write(set_job(jobname = jobname, priority = priority))
}

#' @rdname set_job
#' @export
set_job_path <- function(jobname, path) {
  job_write(set_job(jobname = jobname, path = path))
}

#' @rdname set_job
#' @export
set_job_deadline <- function(jobname, deadline) {
  job_write(set_job(jobname = jobname, deadline = deadline))
}

#' @rdname set_job
#' @export
set_job_team <- function(jobname, add_team = NULL, remove_team = NULL) {
  job_write(set_job(jobname = jobname, add_team = add_team, remove_team = remove_team))
}

#' @rdname set_job
#' @export
set_job_url <- function(jobname, site, link) {
  job_write(set_job(jobname = jobname, site = site, link = link))
}

#' @rdname set_job
#' @export
set_job_tag <- function(jobname, add_tag = NULL, remove_tag = NULL) {
  job_write(set_job(jobname = jobname, add_tag = add_tag, remove_tag = remove_tag))
}


#' Set the properties of an existing job
#'
#' @param jobname the current name for the job
#' @param newname the new name
#' @param description the new description
#' @param owner nick name for the new owner
#' @param status the new status
#' @param priority the new priority
#' @param path the new path to the job folder
#' @param add_tag character vector of tags to add to jobs
#' @param remove_tag character vector of tags to remove from job
#' @param site string with the site nickname (e.g., "github")
#' @param link string with the link to the site
#' @param add_team character vector of nick names to add to the team
#' @param remove_team character vector of nick names to remove from the team
#'
#' @name set_job
NULL


# user-facing functions ---------------------------------------------------

# these functions are all wrappers around set_job that write
# the resutl to file to ensure persistent storage

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


# everything passes through set_job ---------------------------------------

set_job <- function(
  jobname, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, add_tag = NULL,
  remove_tag = NULL, site = NULL, link = NULL, add_team = NULL,
  remove_team = NULL
){

  # note: set_job does not call a constructor function, it modifies an
  # existing job in place, so it has to call verify_ functions for anything
  # it modifies. it doesn't do the modification itself, merely verfies the
  # inputs where appropriate and passes it off to the relevant updater function

  jobs <- job_read()

  # verify old job exists
  verify_jobname(jobname)
  verify_jobexists(jobname, jobs)

  # ------- job name -------
  if(is_set(newname)) {
    verify_jobname(newname)
    verify_jobmissing(newname, jobs)
    jobs <- update_jobname(jobs, jobname, newname)
  }

  # ------- job description -------
  if(is_set(description)) {
    verify_description(description)
    jobs <- update_job(jobs, jobname, description)
  }

  # ------- job status -------
  if(is_set(status)) {
    verify_status(status)
    jobs <- update_job(jobs, jobname, status)
  }

  # ------- job owner -------
  if(is_set(owner)) {
    verify_character(owner)
    jobs <- update_jobowner(jobs, jobname, owner)
  }

  # ------- job priority -------
  if(is_set(priority)) {
    verify_priority(priority)
    jobs <- update_job(jobs, jobname, priority)
  }

  # ------- job path -------
  if(is_set(path)) {
    verify_path(path)
    jobs <- update_jobpath(jobs, jobname, path)
  }

  # ------- job team (add) -------
  if(is_set(add_team)) {
    verify_character(add_team)
    jobs <- update_addteam(jobs, jobname, add_team)
  }

  # ------- job team (remove) -------
  if(is_set(remove_team)) {
    verify_character(remove_team)
    jobs <- update_removeteam(jobs, jobname, remove_team)
  }

  # ------- job url -------
  if(is_set(site) | is_set(link)) {
    verify_site(site)
    verify_link(link)
    jobs <- update_joburl(jobs, jobname, site, link)
  }

  # ------- job tag (add) --------
  if(is_set(add_tag)) {
    verify_character(add_tag)
    for(j in jobname) { # vectorised
      verify_jobname(j)
      verify_jobexists(j, jobs)
      jobs <- update_addtag(jobs, j, add_tag)
    }
  }

  # ------- job tag (remove) --------
  if(is_set(remove_tag)) {
    verify_character(remove_tag)
    for(j in jobname) { # vectorised
      verify_jobname(j)
      verify_jobexists(j, jobs)
      jobs <- update_removetag(jobs, j, remove_tag)
    }
  }

  return(jobs)
}



# workhorse functions -----------------------------------------------------

# simple cases can be handled this way
update_job <- function(jobs, jobname, value) {
  field <- deparse(substitute(value))
  jobs[[jobname]][field] <- value
  return(jobs)
}


# path
update_jobpath <- function(jobs, jobname, path) {
  jobs[[jobname]]$path <- path
  if(dir.exists(path)) {
    write_sentinel(path, jobname, jobs[[jobname]]$idstring)
  }
  return(jobs)
}

# jobname
update_jobname <- function(job, jobname, newname) {
  job_names <- names(jobs)
  ind <- which(job_names == jobname)
  names(jobs)[ind] <- newname
  jobs[[newname]]$jobname <- newname
  return(jobs)
}

# owner
update_jobowner <- function(jobs, jobname, owner) {
  jobs[[jobname]]$owner <- ppl_fullname(owner)
  if(!(jobs[[jobname]]$owner %in% jobs[[jobname]]$team)) {
    jobs[[jobname]]$team <- c(jobs[[jobname]]$owner, jobs[[jobname]]$team)
  }
  return(jobs)
}

# team (add)
update_addteam <- function(jobs, jobname, add_team) {
  add_team <- ppl_fullname(add_team)
  jobs[[jobname]]$team <- unique(c(jobs[[jobname]]$team, add_team))
  return(jobs)
}

# team (remove)
update_removeteam <- function(jobs, jobname, remove_team) {
  remove_team <- ppl_fullname(remove_team)
  if(jobs[[jobname]]$owner %in% remove_team) {
    warning("set_job_team() cannot remove owner from a team", call. = FALSE)
    remove_team <- setdiff(remove_team, jobs[[jobname]]$owner)
  }
  jobs[[jobname]]$team <- setdiff(jobs[[jobname]]$team, remove_team)
  return(jobs)
}

# url
update_joburl <- function(jobs, jobname, site, link) {

  urls <- jobs[[jobname]]$urls
  ind <- which(urls$site == site)
  if(length(ind) == 0) {
    urls <- dplyr::bind_rows(urls, new_url(site, link))
  } else if(length(ind) == 1) {
    urls$link[ind] <- link
  } else stop("inconceivable!") # unreachable?

  urls <- dplyr::arrange(urls, site)
  jobs[[jobname]]$urls <- urls
  return(jobs)
}

# tag (add)
update_addtag <- function(jobs, jobname, add_tag) {
  jobs[[jobname]]$tags <- unique(c(jobs[[jobname]]$tags, add_tag))
  return(jobs)
}

# tag (remove)
update_removetag <- function(jobs, jobname, remove_tag) {
  jobs[[jobname]]$tags <- setdiff(jobs[[jobname]]$tags, remove_tag)
}

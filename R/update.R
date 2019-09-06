
# anything that updates a job passes through here ----------------------------

update_job <- function(
  jobname, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, add_tag = NULL,
  remove_tag = NULL, site = NULL, link = NULL, delete = FALSE
){

  # note: update_job does not call a constructor function, it modifies an
  # existing job in place, so it has to call verify_ functions for anything
  # it modifies. it doesn't do the modification itself, merely verfies the
  # inputs where appropriate and passes it off to the relevant updater function

  jobs <- job_read()

  # verify job exists
  verify_jobname(jobname)
  verify_jobexists(jobname, jobs)

  # ------- delete --------
  if(delete == TRUE) {
    jobs <- delete_job(jobs, jobname)
    return(jobs)
  }

  # ------- job name -------
  if(!is.null(newname)) {
    verify_jobname(newname)
    verify_jobmissing(newname, jobs)
    jobs <- update_jobname(jobs, jobname, newname)
  }

  # ------- job description -------
  if(!is.null(description)) {
    verify_description(description)
    jobs <- update_jobother(jobs, jobname, description)
  }

  # ------- job status -------
  if(!is.null(status)) {
    verify_status(status)
    jobs <- update_jobother(jobs, jobname, status)
  }

  # ------- job owner -------
  if(!is.null(owner)) {
    verify_character(owner)
    jobs <- update_jobother(jobs, jobname, owner)
  }

  # ------- job priority -------
  if(!is.null(priority)) {
    verify_priority(priority)
    jobs <- update_jobother(jobs, jobname, priority)
  }

  # ------- job path -------
  if(!is.null(path)) {
    verify_path(path)
    jobs <- update_jobpath(jobs, jobname, path)
  }

  # ------- job url: NA to remove -------
  if(!is.null(site) | !is.null(link)) {
    verify_site(site)
    if(is.na(link)) {
      jobs <- delete_joburl(jobs, jobname, site)
    } else {
      verify_link(link)
      jobs <- update_joburl(jobs, jobname, site, link)
    }
  }

  # ------- job tag (add) --------
  if(!is.null(add_tag)) {
    verify_character(add_tag)
    for(j in jobname) { # vectorised
      verify_jobname(j)
      verify_jobexists(j, jobs)
      jobs <- update_addtag(jobs, j, add_tag)
    }
  }

  # ------- job tag (remove) --------
  if(!is.null(remove_tag)) {
    verify_character(remove_tag)
    for(j in jobname) { # vectorised
      verify_jobname(j)
      verify_jobexists(j, jobs)
      jobs <- update_removetag(jobs, j, remove_tag)
    }
  }

  return(jobs)
}

# updater functions -----------------------------------------------------

# simple cases can be handled this way
update_jobother <- function(jobs, jobname, value) {
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
  return(jobs)
}


# deletion functions -----------------------------------------------------


delete_job <- function(jobs, jobname) {
  jobs[[jobname]] <- NULL
  return(jobs)
}

delete_joburl <- function(jobs, jobname, site) {
  jb <- jobs[[jobname]]
  if(site %in% jb$urls$site) {
    jb$urls <- dplyr::filter(jb$urls, site != {{site}})
    jobs[[jobname]] <- jb
  } else {
    warning("There is '", site, "' link in '", jobname, "'", call. = FALSE)
  }
  return(jobs)
}


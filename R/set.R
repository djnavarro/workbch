# defines the "set" family of functions

#' Sets the name of a job
#'
#' @param from the current name for the job
#' @param to the new name for the job
#'
#' @export
set_job_name <- function(from, to) {

  jobs <- job_read()
  verify_jobname(from, jobs)

  # don't let the user overwrite an existing job
  if(to %in% job_names) {
    stop("job '", to, "' already exists", call. = FALSE)
  }

  # rename the list entry itself
  job_names <- names(jobs)
  ind <- which(job_names == from)
  names(jobs)[ind] <- to

  # rename within the field
  jobs[[to]]$jobname <- to

  # write the file and return
  job_write(jobs)
}


#' Sets the description of a job
#'
#' @param description String specifying the description
#' @param jobname Name of the job
#'
#' @export
set_job_description <- function(description, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  # write new value
  jobs[[jobname]]$description <- description
  job_write(jobs)

}


#' Sets the status of a job
#'
#' @param status The new status
#' @param jobname Name of the job
#' @export
set_job_status <- function(status, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  # write new value
  jobs[[jobname]]$status <- status
  job_write(jobs)


}

#' Sets the owner of a job
#'
#' @param owner Nick name for the new owner
#' @param jobname Name of the job
#' @export
set_job_owner <- function(owner, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  # set new owner
  jb <- jobs[[jobname]]
  jb$owner <- real_name(owner)

  # add new owner to team if needed
  if(!(jb$owner %in% jb$team)) {
    jb$team <- c(jb$owner, jb$team)
  }

  # write new values
  jobs[[jobname]] <- jb
  job_write(jobs)

}

#' Sets the priority of a job
#'
#' @param priority The new priority
#' @param jobname Name of the job
#' @export
set_job_priority <- function(priority, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  # write new value
  jobs[[jobname]]$priority <- priority
  job_write(jobs)

}

#' Sets the path of a job
#'
#' @param path The path to the job folder
#' @param jobname Name of the job
#' @export
set_job_path <- function(path, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  # write new value
  jobs[[jobname]]$path <- path
  job_write(jobs)

}

#' Sets the visibility of a job
#'
#' @param hidden Logical
#' @param jobname Name of the job
#' @export
set_job_hidden <- function(hidden, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  # write new value
  jobs[[jobname]]$hidden <- hidden
  job_write(jobs)

}


#' Set the location of workbch files
#'
#' @param path Path to the folder
#'
#' @return Path to the folder
#' @export
set_workbch_home <- function(path = NULL) {
  if(!dir.exists(path)) {
    dir.create(path)
    message("new directory '", path, "' created")
  }
  if(!is.null(path)) {
    options(workbch.home = path)
  }
  job_home()
}




#' Set the members of a team
#'
#' @param add character vector of names to add to the team
#' @param remove character vector of names to remove from the team
#' @param jobname name of job to be edited
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
set_job_team <- function(add = NULL, remove = NULL, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  if(!is.null(add)) {
    add <- real_name(add)
    jobs[[jobname]]$team <- unique(c(jobs[[jobname]]$team, add))
  }

  if(!is.null(remove)) {
    remove <- real_name(remove)

    # cannot remove owner
    if(jobs[[jobname]]$owner %in% remove) {
      warning(
        "cannot remove owner from a team: use job_edit() to change owner first",
        call. = FALSE
      )
      remove <- setdiff(remove, jobs[[jobname]]$owner)
    }

    jobs[[jobname]]$team <- setdiff(jobs[[jobname]]$team, remove)
  }

  job_write(jobs)
}




#' Set a URL associated with a job
#'
#' @param site string with the site nickname (e.g., "github")
#' @param link string with the link to the site
#' @param jobname name of the job to edit
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
set_job_url <- function(site, link, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

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

  # ensure that the edits produce a valid job state
  jobs[[jobname]] <- validate_job(jobs[[jobname]])

  # write
  job_write(jobs)
}


#' Sets details for a new person
#'
#' @param fullname the full name of the person
#' @param nickname a short name for the person
#' @param make_default should this person be set as the "default"
#' @export
set_person <- function(fullname, nickname, make_default = FALSE) {
  ppl <- ppl_read()

  # remove an old default person if need be
  if(make_default == TRUE) {
    old_def <- which(ppl$default == TRUE)
    ppl$default[old_def] <- FALSE
  }

  # see if the nickname exists and/or the full name exists
  fn_ind <- which(ppl$fullname == fullname)
  nn_ind <- which(ppl$nickname == nickname)

  # if neither name exists, add a new person
  if(length(fn_ind) == 0 & length(nn_ind) == 0) {

    message("added '", fullname, "' with nickname '", nickname, "'")

    ppl <- dplyr::bind_rows(
      ppl,
      tibble::tibble(
        fullname = fullname,
        nickname = nickname,
        default = make_default
      ))
    ppl_write(ppl)
    return(invisible(ppl))

  }

  # if the nickname exists and not the fullname
  if(length(fn_ind) == 0 & length(nn_ind) > 0) {

    oldname <- ppl$fullname[nn_ind]
    message("changed full name of '", nickname, "' from '",
            oldname , "' to '", fullname, "'")

    # update the people file
    ppl$fullname[nn_ind] <- fullname
    ppl$default[nn_ind] <- make_default
    ppl_write(ppl)

    # update the jobs file
    jobs <- job_read()
    jobs <- purrr::map(jobs, function(.x) {
      if(.x$owner == oldname) {.x$owner <- fullname}
      .x$team <- gsub(oldname, fullname, .x$team, fixed = TRUE)
      return(.x)
    })
    job_write(jobs)

    return(invisible(ppl))
  }

  # if the fullname exists and not the nickname
  if(length(fn_ind) > 0 & length(nn_ind) == 0) {

    message("changed nickname of '", fullname, "' from '",
            ppl$nickname[fn_ind], "' to '", nickname, "'")
    ppl$nickname[fn_ind] <- nickname
    ppl$default[fn_ind] <- make_default
    ppl_write(ppl)
    return(invisible(ppl))

  }

  # if both exist and are the same index do nothing except possibly
  # update the default status
  if(identical(fn_ind, nn_ind)) {
    message("'", fullname, "' already has nickname '", nickname, "'")
    ppl$default[fn_ind] <- make_default
    ppl_write(ppl)
    return(invisible(ppl))
  }

  # if both exist but map to different people do nothing
  message("'", fullname, "' already has nickname '", ppl$nickname[fn_ind], "'")
  message("'", ppl$fullname[nn_ind], "' already has nickname '", nickname, "'")
  return(invisible(ppl))

}


#' Set the tags for a job
#'
#' @param add character vector of tags to add to jobs
#' @param remove character vector of tags to remove from jobd
#' @param jobname name of job(s) to be edited
#' @details The role of \code{set_tags()} is to...
#' @export
set_job_tag <- function(add = NULL, remove = NULL, jobname = NULL) {

  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}

  for(jbnm in jobname) {

    # check the name
    verify_jobname(jbnm, jobs)

    # if there are tags to add, add them
    if(!is.null(add)) {
      jobs[[jbnm]]$tags <- unique(c(jobs[[jbnm]]$tags, add))
    }

    # if there are tags to remove, remove them
    if(!is.null(remove)) {
      jobs[[jbnm]]$tags <- setdiff(jobs[[jbnm]]$tags, remove)
    }
  }
  job_write(jobs)
}


#' Set the deadline for a job
#'
#' @param date character string to be parsed by lubridate::dmy
#' @param jobname name of job(s) to be edited
#' @export
set_job_deadline <- function(date, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  date <- format_date(date)
  jobs[[jobname]]$deadline <- date
  job_write(jobs)
}


#' Set the deadline for a taskjob
#'
#' @param date character string to be parsed by lubridate::dmy
#' @param id id number of the task to be edited
#' @param jobname name of job(s) to be edited
#' @export
set_task_deadline <- function(date, id, jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}
  verify_jobname(jobname, jobs)

  date <- format_date(date)
  ind <- jobs[[jobname]]$tasks$id == id
  jobs[[jobname]]$tasks$deadline[ind] <- date
  job_write(jobs)
}




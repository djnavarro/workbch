
# to avoid the 'no visible binding' NOTE:
utils::globalVariables(c("priority", "status", "owner", "name"))

#' List jobs
#'
#' @param ... expression to be passed to dplyr::filter
#' @param show_hidden should hidden jobs be included
#' @export
view_joblist <- function(..., show_hidden = FALSE) {

  # read jobs and construct tibble listing them
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("name", "owner", "priority", "status", "deadline", "description")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, name)

  # filter according to user expression
  if(...length() > 0) {
    job_tbl <- dplyr::filter(job_tbl, ...)
  }

  # remove the hidden jobs if need be
  if(!show_hidden) {
    job_tbl <- remove_hiddenjobs(jobs, job_tbl)
  }

  return(job_tbl)
}


remove_hiddenjobs <- function(jobs, job_tbl) {

  # find them
  hidden <- purrr::map_chr(jobs, function(x) {
    if(!is.null(x$hidden)) {
      if(x$hidden == TRUE) {
        return(x$name)
      }
    }
    return("")
  })
  hidden <- hidden[hidden != ""]

  # remove them
  job_tbl <- dplyr::filter(job_tbl, !(name %in% hidden))

  return(job_tbl)
}

#' View jobs by priority
#'
#' @param priority numeric vector of priorities to display
#' @param ... expression to be passed to view_jobs
#' @param show_hidden should hidden jobs be included
#'
#' @return tibble of jobs
#' @export
view_priorities <- function(priority = 1, ..., show_hidden = FALSE) {
  jobs <- view_joblist(..., show_hidden = show_hidden)
  jobs <- dplyr::filter(jobs, priority %in% {{priority}})
  return(jobs)
}



#' Show the details of a job
#'
#' @param name Name of job to display
#' @export
view_job <- function(name) {
  jobs <- job_read()
  jb <- jobs[[name]]

  cat("\n")
  cat(jb$name, ":", jb$description, "\n")
  cat("\n")

  cat("  owner    :", jb$owner, "\n")
  cat("  team     :", paste(jb$team, collapse = ", "), "\n")
  cat("  priority :", jb$priority, "\n")
  cat("  status   :", jb$status, "\n")

  dl <- ifelse(is.na(jb$deadline), "none", jb$deadline)
  cat("  deadline :", dl, "\n")

  cat("\n")
  cat("  path =", jb$path, "\n")
  if(length(jb$urls) > 0) {
    for(i in 1:length(jb$urls)) {
      cat(" ", names(jb$urls)[i], "=", jb$urls[[i]], "\n")
    }
  }

  # legacy to deal with jobs that initialised notes
  # with an empty list rather than an empty tibble
  n_notes <- nrow(jb$notes)
  if(is.null(n_notes)) {
    n_notes <- 0
  }

  cat("\n")
  cat(" ", n_notes, "notes\n")
  cat(" ", length(jb$tasks), "tasks\n")

  cat("\n")
  return(invisible(jb))
}

#' View notes associated with a job
#'
#' @param name the job
#'
#' @details Displays all notes associated with a job in order of recency. The
#' display format is minimal, showing only an id number (to make it easy to
#' delete notes later) and the text of each note. Notes are shown in
#' chronological order, with most recent notes at the top of the output
#' @return Invisibly returns a tibble containing columns for the note,
#' the id number, the creation date, and the project name
#' @export
#'
#' @examples
#' \dontrun{
#'
#' view_notes("myjob")
#' }
view_notes <- function(name) {
  jobs <- job_read()
  nt <- jobs[[name]]$notes
  if(!is.null(dim(nt))) {
    if(nrow(nt) > 0) {
      cat("\n")
      for(i in 1:nrow(nt)) {
        cat(nt$id[i], ":  ", nt$note[i], "\n", sep = "")
      }
      cat("\n")
    }
  }
  return(invisible(nt))
}


#' View the project folder locations known to workbch
#'
#' @param show_hidden should hidden jobs be included
#'
#' @return A tibble
#' @export
view_projects <- function(show_hidden = FALSE) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    if(!is.null(x$path)) {
      return(tibble::as_tibble(x[c("name", "path")]))
    } else {
      return(tibble::tibble(name = character(0), path = character(0)))
    }
  })
  job_tbl <- dplyr::arrange(job_tbl, name)
  job_tbl <- dplyr::filter(job_tbl, !is.na(path))

  # remove the hidden jobs if need be
  if(!show_hidden) {
    job_tbl <- remove_hiddenjobs(jobs, job_tbl)
  }

  return(job_tbl)
}



#' List the job names known to workbch
#'
#' @param show_hidden should hidden jobs be included
#'
#' @return A character vector of names, in alphabetical order
#' @export
view_jobnames <- function(show_hidden = FALSE) {
  jobs <- job_read()
  job_names <- purrr::map_dfr(jobs, function(x){tibble::tibble(name = x$name)})
  job_names <- dplyr::arrange(job_names, name)

  if(!show_hidden) {
    job_names <- remove_hiddenjobs(jobs, job_names)
  }

  return(dplyr::pull(job_names, name))
}


#' Check the git status of projects
#'
#' @param show_hidden should hidden jobs be included?
#' @param show_clean should clean repos be included?
#'
#' @return A tibble
#' @export
view_gitstatus <- function(show_hidden = FALSE, show_clean = FALSE) {

  # get the project locations
  proj <- view_projects(show_hidden = show_hidden)
  x <- list()

  for(i in 1:nrow(proj)) {

    # for the sake of my sanity
    pp <- proj$path[i]
    pn <- proj$name[i]

    if(git2r::in_repository(pp)) {

      # get the branch
      repo_head <- git2r::repository_head(pp)
      upstream_head <- git2r::branch_get_upstream(repo_head)

      # number of commits ahead and behind
      if(is.null(upstream_head)) {
        repo_ab <- c(NA, NA)
      } else {
        repo_ab <- git2r::ahead_behind(
          local = repo_head,
          upstream = upstream_head
        )
      }

      # get the repo git status
      repo_status <- purrr::map_dfr(unclass(git2r::status(pp)), length)

      # put it all together in a tibble
      x[[i]] <- dplyr::bind_cols(
        tibble::tibble(name = pn),
        repo_status,
        tibble::tibble(ahead = repo_ab[1], behind = repo_ab[2])
      )
    }
  }

  # collapse to a single tibble
  gitst <- dplyr::bind_rows(x)

  if(!show_clean) {
    gitst <- dplyr::filter(gitst,
      !(staged == 0 & unstaged == 0 & untracked == 0 &
        (is.na(ahead) | ahead == 0) & (is.na(behind) | behind == 0))
    )
  }

  return(gitst)

}





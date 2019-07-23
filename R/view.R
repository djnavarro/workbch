# defines the "view" family of functions

#' View a list of jobs
#'
#' @param ... expression to be passed to dplyr::filter
#' @param show_hidden should hidden jobs be included
#' @export
view_jobs <- function(..., show_hidden = FALSE) {

  # read jobs and construct tibble listing them
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("jobname", "owner", "priority", "status", "deadline", "description")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, jobname)

  # filter according to user expression
  if(...length() > 0) {job_tbl <- dplyr::filter(job_tbl, ...)}

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- hide_jobs(jobs, job_tbl)}

  return(job_tbl)
}


#' View the list of known people
#'
#' @export
view_people <- function() {
  ppl_read()
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
  jobs <- view_jobs(..., show_hidden = show_hidden)
  jobs <- dplyr::filter(jobs, priority %in% {{priority}})
  return(jobs)
}



#' View the details of a job
#'
#' @param jobname Name of job to display
#' @export
view_job <- function(jobname) {
  jobs <- job_read()
  jb <- jobs[[jobname]]

  cat("\n")
  cat(jb$jobname, ":", jb$description, "\n")
  cat("\n")

  cat("  owner    :", jb$owner, "\n")
  cat("  team     :", paste(jb$team, collapse = ", "), "\n")
  cat("  priority :", jb$priority, "\n")
  cat("  status   :", jb$status, "\n")

  dl <- ifelse(is.na(jb$deadline), "none", jb$deadline)
  cat("  deadline :", dl, "\n")

  cat("\n")
  cat("  path =", jb$path, "\n")
  if(nrow(jb$urls) > 0) {
    for(i in 1:nrow(jb$urls)) {
      cat(" ", jb$urls$site[i], "=", jb$urls$link[i], "\n")
    }
  }

  cat("\n")
  cat(" ", nrow(jb$notes), "notes\n")
  cat(" ", nrow(jb$tasks), "tasks\n")

  cat("\n")
  return(invisible(jb))
}

#' View notes associated with a job
#'
#' @param jobname the job
#'
#' @details Displays all notes associated with a job in order of recency. The
#' display format is minimal, showing only an id number (to make it easy to
#' delete notes later) and the text of each note. Notes are shown in
#' chronological order, with most recent notes at the top of the output
#' @return Invisibly returns a tibble containing columns for the note,
#' the id number, the creation date, and the job name
#' @export
#'
#' @examples
#' \dontrun{
#'
#' view_notes("myjob")
#' }
view_notes <- function(jobname) {
  jobs <- job_read()
  nt <- jobs[[jobname]]$notes
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


#' View the job folder locations known to workbch
#'
#' @param show_hidden should hidden jobs be included
#'
#' @return A tibble
#' @export
view_paths <- function(show_hidden = FALSE) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    if(!is.null(x$path)) {
      return(tibble::as_tibble(x[c("jobname", "path")]))
    } else {
      return(tibble::tibble(jobname = character(0), path = character(0)))
    }
  })
  job_tbl <- dplyr::arrange(job_tbl, jobname)
  job_tbl <- dplyr::filter(job_tbl, !is.na(path))

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- hide_jobs(jobs, job_tbl)}

  return(job_tbl)
}



#' View job names known to workbch
#'
#' @param show_hidden should hidden jobs be included
#'
#' @return A character vector of names, in alphabetical order
#' @export
view_job_names <- function(show_hidden = FALSE) {
  jobs <- job_read()
  job_names <- purrr::map_dfr(jobs, function(x){tibble::tibble(jobname = x$jobname)})
  job_names <- dplyr::arrange(job_names, jobname)

  if(!show_hidden) {job_names <- hide_jobs(jobs, job_names)}

  return(dplyr::pull(job_names, jobname))
}


#' View the git status of jobs
#'
#' @param show_hidden should hidden jobs be included?
#' @param show_clean should clean repos be included?
#'
#' @return A tibble
#' @export
view_git_status <- function(show_hidden = FALSE, show_clean = FALSE) {

  # get the job locations
  proj <- view_jobs(show_hidden = show_hidden)
  x <- list()

  for(i in 1:nrow(proj)) {

    # for the sake of my sanity
    pp <- proj$path[i]
    pn <- proj$jobname[i]

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
        tibble::tibble(jobname = pn),
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



#' View tasks
#'
#' @param ... filtering expression to pass to dplyr::filter
#' @param show_hidden should hidden tasks be shown (default = FALSE)
#'
#' @details Displays all tasks, sorted by deadline then priority
#' @return A tibble containing the tasks
#' @export
#'
#' @examples
#' \dontrun{
#'
#' view_tasks()
#' }
view_tasks <- function(..., show_hidden = FALSE) {

  tasks <- task_read()

  if(...length() > 0) {
    tasks <- dplyr::filter(tasks, ...)
  }
  tasks <- dplyr::arrange(tasks, deadline, priority)
  if(!show_hidden) {
    tasks <- dplyr::filter(tasks, hidden == FALSE)
  }
  tasks$hidden <- NULL

  return(tasks)
}





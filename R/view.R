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
    tibble::as_tibble(x[c("jobname", "owner", "priority", "status", "deadline",
                          "description", "path")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, jobname)

  # filter according to user expression
  if(...length() > 0) {job_tbl <- dplyr::filter(job_tbl, ...)}

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- hide_jobs(job_tbl)}

  verify_path(job_tbl$jobname, job_tbl$path)
  job_tbl$path <- NULL
  return(as_wkbch_tbl(job_tbl))
}


#' View a list of jobs that possess a tag
#'
#' @param tag what tag to display
#' @param ... filtering expression to pass to dplyr::filter
#' @param show_hidden should hidden jobs be included
#' @param invert if TRUE, return jobs without the tag
#' @export
view_tag <- function(tag, ..., show_hidden = TRUE, invert = FALSE) {

  jobs <- job_read()

  # construct tibble
  has_tag <- purrr::map_lgl(jobs, function(x) {tag %in% x$tags})
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("jobname", "owner", "priority", "status", "deadline",
                          "description", "path")])})

  # subset of jobs that have (or dont have) the tag
  if(invert == FALSE) {
    job_tbl <- job_tbl[which(has_tag == TRUE), ]
  } else {
    job_tbl <- job_tbl[which(has_tag == FALSE), ]
  }

  # arrange
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, jobname)

  # filter according to user expression
  if(...length() > 0) {job_tbl <- dplyr::filter(job_tbl, ...)}

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- hide_jobs(job_tbl)}

  verify_path(job_tbl$jobname, job_tbl$path)
  job_tbl$path <- NULL
  return(as_wkbch_tbl(job_tbl))
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
  return(as_wkbch_tbl(jobs))
}



#' View the details of a job
#'
#' @param jobname Name of job to display
#' @export

view_job <- function(jobname = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  if(is.null(jobname)) {jobname <- get_current_jobname(jobs)}

  # check jobname
  verify_jobname(jobname)
  if(!job_exists(jobname, jobs)) {
    stop("job '", jobname, "' does not exist", call. = FALSE)
  }

  # get job
  jb <- jobs[[jobname]]

  cat("\n")
  cat(jb$jobname, ":", jb$description, "\n")
  cat("\n")

  cat("  owner    :", jb$owner, "\n")
  cat("  team     :", paste(jb$team, collapse = ", "), "\n")
  cat("  priority :", jb$priority, "\n")
  cat("  status   :", jb$status, "\n")
  cat("  tags     :", paste(jb$tags, collapse = ", "), "\n")

  dl <- ifelse(is.na(jb$deadline), "none", jb$deadline)
  cat("  deadline :", dl, "\n")

  if(nrow(jb$urls) > 0 | !is.na(jb$path)) {
    cat("\n  locations: \n")
    cat("     [path] ", jb$path, "\n", sep = "")
    if(nrow(jb$urls) > 0) {
      for(i in 1:nrow(jb$urls)) {
        cat("     [", jb$urls$site[i], "] ", jb$urls$link[i], "\n", sep = "")
      }
    }
    cat("\n")
  }

  # display tasks
  if(nrow(jb$tasks) > 0) {
    cat("  tasks: \n")
    for(i in 1:nrow(jb$tasks)) {
      cat("     [", jb$tasks$id[i], "] ", jb$tasks$description[i], " (",
          jb$tasks$deadline[i], ")\n", sep = "")
    }
  }

  verify_path(jb$jobname, jb$path)
  return(invisible(jb))
}

#' View the git status of jobs
#'
#' @param show_hidden should hidden jobs be included?
#' @param show_clean should clean repos be included?
#'
#' @return A tibble
#' @export
view_git_status <- function(show_hidden = TRUE, show_clean = FALSE) {

  # get the job locations
  proj <- workbch_paths(show_hidden = show_hidden)
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

  return(as_wkbch_tbl(gitst))

}



#' View tasks
#'
#' @param ... filtering expression to pass to dplyr::filter
#' @param show_hidden should hidden tasks be shown (default = TRUE)
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
view_tasks <- function(..., show_hidden = TRUE) {

  tasks <- task_read()

  if(...length() > 0) {
    tasks <- dplyr::filter(tasks, ...)
  }
  tasks <- dplyr::arrange(tasks, lubridate::dmy(deadline), priority)

  # remove the hidden tasks if need be
  if(!show_hidden) {tasks <- hide_jobs(tasks)}

  return(as_wkbch_tbl(tasks))
}



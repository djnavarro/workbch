
# to avoid the 'no visible binding' NOTE:
utils::globalVariables(c("priority", "status", "owner", "name"))

#' List jobs
#'
#' @param ... expression to be passed to dplyr::filter
#' @export
view_joblist <- function(...) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("name", "owner", "priority", "status", "deadline", "description")])})
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, name)
  if(...length() > 0) {
    job_tbl <- dplyr::filter(job_tbl, ...)
  }
  return(job_tbl)
}


#' View jobs by priority
#'
#' @param priority numeric vector of priorities to display
#' @param ... expression to be passed to view_jobs
#'
#' @return tibble of jobs
#' @export
view_priorities <- function(priority = 1, ...) {
  jobs <- view_joblist()
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
  cat(" ", nrow(jb$notes), "notes\n")
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

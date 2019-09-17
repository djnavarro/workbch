
#' Modify the properties of a job
#'
#' @param jobname the current name for the job
#' @param newname the new name
#' @param description the new description
#' @param owner nick name for the new owner
#' @param status the new status
#' @param priority the new priority
#' @param path the new path to the job folder
#' @param tags string with tags (uses | as separator)
#' @param url string specifying url
#' @param delete should this job be deleted (default = FALSE)
#'
#' @details The role of the \code{job_modify()} function is to change one or more
#' parameters of an existing workbch job. It can be called in two ways, interactively
#' or programmatically. To call the function interactively, R must be in interactive
#' mode and the function should be called specifying either the \code{jobname} argument
#' only, or no arguments specified. If no \code{jobname} is specified the
#' \code{job_modify()} function will attempt to guess the
#' current job by looking at any open RStudio projects. If no project is open
#' or the RStudio API is not available it attempts to guess by looking at the
#' working directory
#'
#' When called interactiely, the user
#' will be presented with a sequence of prompts, asking them to specify each
#' of the parameters that define a job (e.g., a character string for \code{descripton},
#' a number for \code{priority}). When used interactively, you do not need to include
#' quote marks when entering a string: \code{job_modify()} will coerce the input to
#' the appropriate format, and then append the created job to the job file.
#'
#' When called programmatically, the user must specify the arguments in the
#' call to \code{job_modify()}. Onlt the \code{jobname} argument is mandatory.
#' To rename an existing job the \code{newname} argument should be specified.
#' To modify \code{description}, \code{owner}, \code{status}, \code{priority},
#' \code{path}, \code{tags} or \code{urls} the relevant argument shoul be
#' specified.
#'
#' The \code{status} of a job should be \code{"active"}, \code{"inactive"},
#' \code{"complete"}, \code{"abandoned"} or \code{"masked"}. The \code{priority} for
#' a job should be a positive integer: the intent is that priority 1 is the highest
#' priority, followed by priority 2, and so one. The \code{tags} for a job can be
#' specified as a single string, using \code{|} as a separator character (e.g.,
#' \code{tags = "research | statistics"} would create two tags for the job).
#' The \code{path} should specify the location of a folder containing the project
#' files. The format for \code{urls} is the same as for tags. For example to
#' specify a GitHub link one might write \code{urls = "github \
#' https://github.com/djnavarro/workbch"}. Multiple URLs can be specified by
#' extending this syntax using \code{|} as the separator. For example:
#' \code{urls = "github \ https://github.com/djnavarro/workbch | docs |
#' https://djnavarro.github.io/workbch"}.
#'
#' Setting \code{delete = TRUE} will delete the job.
#'
#' @export
job_modify <- function(
  jobname = NULL, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, tags = NULL, url = NULL,
  delete = FALSE
){

  # use prompt method if we are interactive and the user has
  # not specified any arguments (except possibly jobname)
  use_prompt <- interactive() & is.null(newname) &
    is.null(description) & is.null(owner) & is.null(status) &
    is.null(priority) & is.null(path) & is.null(tags) & is.null(url) &
    delete == FALSE

  # interactive version
  if(use_prompt) {

    # read jobs and use the current job if the user does
    # not specify an argument
    jobs <- job_read()
    if(is.null(jobname)) {
      jobname <- suppressMessages(job_getcurrent(jobs))
    }

    cat("Current details for job:", jobname, "\n")
    job_glimpse(jobname)
    cat("\n")

    cat("What do you want to do?\n\n")
    cat("  [1] change the job name\n")
    cat("  [2] change the description\n")
    cat("  [3] change the job owner\n")
    cat("  [4] change the job status\n")
    cat("  [5] change the job priority\n")
    cat("  [6] change the job location\n")
    cat("  [7] add or remove a url\n")
    cat("  [8] change the tags\n")
    cat("  [9] delete this job\n")
    cat("\n")
    ans <- readline("Selection: ")
    ans <- suppressWarnings(as.numeric(ans))
    cat("\n")
    if(ans == 1) return(prompt_rename(jobname))
    if(ans == 2) return(prompt_description(jobname))
    if(ans == 3) return(prompt_owner(jobname))
    if(ans == 4) return(prompt_status(jobname))
    if(ans == 5) return(prompt_priority(jobname))
    if(ans == 6) return(prompt_path(jobname))
    if(ans == 7) return(prompt_url(jobname))
    if(ans == 8) return(prompt_tag(jobname))
    if(ans == 9) return(prompt_delete(jobname))
    return(invisible(NULL))

  # programmatic version
  } else {

    if(is.null(jobname)) {
      stop("'jobname' is required", call. = FALSE)
    }

    jobs <- update_job(
      jobname = jobname,
      newname = newname,
      description = description,
      owner = owner,
      status = status,
      priority = priority,
      path = path,
      tags = tags,
      url = url,
      delete = delete
    )
    job_write(jobs)
    return(invisible(NULL))

  }
}

prompt_rename <- function(jobname) {
  newname <- readline("Enter new job name: ")
  job_write(update_job(jobname = jobname, newname = newname))
}

prompt_description <- function(jobname) {
  description <- readline("Enter new job description: ")
  job_write(update_job(jobname = jobname, description = description))
}

prompt_owner <- function(jobname) {
  description <- readline("Enter new job owner: ")
  job_write(update_job(jobname = jobname, owner = owner))
}

prompt_delete <- function(jobname) {
  jobname2 <- readline("To confirm deletion, type the job name: ")
  if(jobname2 == jobname) {
    job_write(update_job(jobname = jobname, delete = TRUE))
  } else {
    message("Deletion aborted")
  }
}

prompt_priority <- function(jobname) {
  priority <- readline("Enter new job priority: ")
  job_write(update_job(jobname = jobname, priority = priority))
}

prompt_path <- function(jobname) {
  priority <- readline("Enter new job path: ")
  job_write(update_job(jobname = jobname, path = path))
}

prompt_url <- function(jobname) {
  url <- readline("Enter new url, in 'site | url' format: ")
  job_write(update_job(jobname = jobname, url = url))
}

prompt_tag <- function(jobname) {
  tags <- readline("Enter new tags, in 'tag | tag' format: ")
  job_write(update_job(jobname = jobname, tags = tags))
}

prompt_status <- function(jobname) {
  status <- readline("Enter new job status: ")
  job_write(update_job(jobname = jobname, status = status))
}



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
#' @export
job_modify <- function(
  jobname = NULL, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, tags = NULL, url = NULL,
  delete = FALSE
){

  use_prompt <- interactive() & is.null(jobname) & is.null(newname) &
    is.null(description) & is.null(owner) & is.null(status) &
    is.null(priority) & is.null(path) & is.null(tags) & is.null(url) &
    delete == FALSE

  # interactive version
  if(use_prompt) {

    jobs <- job_read()
    jobname <- suppressMessages(job_getcurrent(jobs))
    cat("The current job is:", jobname, "\n\n")
    cat("What do you want to do?\n")
    cat("  [1] change job name\n")
    cat("  [2] change description\n")
    cat("  [3] change owner\n")
    cat("  [4] change status\n")
    cat("  [5] change priority\n")
    cat("  [6] change job location\n")
    cat("  [7] add/remove a url\n")
    cat("  [8] change tags\n")
    cat("  [9] delete this job\n")
    cat("\n")
    ans <- readline(" Selection: ")
    ans <- suppressWarnings(as.numeric(ans))
    if(ans == 1) return(prompt_rename(jobname))
    if(ans == 2) return(prompt_description(jobname))
    if(ans == 3) return(prompt_owner(jobname))
    if(ans == 4) return(prompt_status(jobname))
    if(ans == 5) return(prompt_priority(jobname))
    if(ans == 6) return(prompt_path(jobname))
    if(ans == 7) return(prompt_url(jobname))
    if(ans == 8) return(prompt_tag(jobname))
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

prompt_status <- function(jobname) {
  status <- readline("Enter new job status: ")
  job_write(update_job(jobname = jobname, status = status))
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




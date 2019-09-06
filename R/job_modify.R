
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
#' @param delete should this job be deleted (default = FALSE)
#'
#' @export
job_modify <- function(
  jobname = NULL, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, add_tag = NULL,
  remove_tag = NULL, site = NULL, link = NULL, delete = FALSE
){

  use_prompt <- interactive() & is.null(jobname) & is.null(newname) &
    is.null(description) & is.null(owner) & is.null(status) &
    is.null(priority) & is.null(path) & is.null(add_tag) &
    is.null(remove_tag) & is.null(site) & is.null(link) & delete == FALSE

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
    cat("  [7] change job links\n")
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
      add_tag = add_tag,
      remove_tag = remove_tag,
      site = site,
      link = link,
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

prompt_url <- function(jobname) {}
prompt_tag <- function(jobname) {}



# user-facing functions ---------------------------------------------------
#
# set_job_owner <- function(jobname, owner) {
#   job_write(set_job(jobname = jobname, owner = owner))
# }
#
# set_job_team <- function(jobname, add_team = NULL, remove_team = NULL) {
#   job_write(set_job(jobname = jobname, add_team = add_team, remove_team = remove_team))
# }
#
# set_job_url <- function(jobname, site, link) {
#   job_write(set_job(jobname = jobname, site = site, link = link))
# }
#
# set_job_tag <- function(jobname, add_tag = NULL, remove_tag = NULL) {
#   job_write(set_job(jobname = jobname, add_tag = add_tag, remove_tag = remove_tag))
# }


prompt_jobname <- function() {
  opts <- job_view()$jobname
  if(length(opts)>9) opts <- opts[1:9]
  for(i in 1:length(opts)) {
    cat(i, ":", opts[i], "\n")
  }
  cat("other: please type jobname\n")
  jnum <- readline("which job? ")
  if(jnum %in% as.character(1:length(opts))) {
    jobname <- opts[as.numeric(jnum)]
  } else {
    jobname <- jnum
  }
  return(jobname)
}




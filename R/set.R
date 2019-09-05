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
#'
#' @export
workbch_setjob <- function(
  jobname, newname = NULL, description = NULL, owner = NULL,
  status = NULL, priority = NULL, path = NULL, add_tag = NULL,
  remove_tag = NULL, site = NULL, link = NULL
){
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
    link = link
  )
  job_write(jobs)
}


#' Prompts user to set properties of the current job
#'
#' @export
job_set <- function() {
  jobs <- job_read()
  jobname <- suppressMessages(job_getcurrent(jobs))
  cat("The current job is:", jobname, "\n\n")
  cat("What do you want to modify:\n")
  cat("  [1] job name\n")
  cat("  [2] description\n")
  cat("  [3] owner\n")
  cat("  [4] status\n")
  cat("  [5] priority\n")
  cat("  [6] job location\n")
  cat("  [7] job links\n")
  cat("  [8] tags\n")
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
}

prompt_rename <- function(jobname) {
  newname <- readline("Enter new job name: ")
  workbch_setjob(jobname = jobname, newname = newname)
}

prompt_description <- function(jobname) {
  description <- readline("Enter new job description: ")
  workbch_setjob(jobname = jobname, description = description)
}

prompt_owner <- function(jobname) {
  description <- readline("Enter new job owner: ")
  workbch_setjob(jobname = jobname, owner = owner)
}

prompt_status <- function(jobname) {
  status <- readline("Enter new job status: ")
  workbch_setjob(jobname = jobname, status = status)
}

prompt_priority <- function(jobname) {
  priority <- readline("Enter new job priority: ")
  workbch_setjob(jobname = jobname, priority = priority)
}

prompt_path <- function(jobname) {
  priority <- readline("Enter new job path: ")
  workbch_setjob(jobname = jobname, path = path)
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



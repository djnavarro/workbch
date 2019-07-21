
#' Edit the details of a job
#'
#' @param name name of the project to edit (character)
#' @param ... expressions to be evaluated in the job
#' @details The role of \code{job_edit} is to make changes to an existing project,
#' specified in the \code{name} argument. Any named expressions in \code{...} are
#' evaluated within a list that represents the job. After updating, the results
#' are written to the `workbch_jobs.json` file.
#' @export
#' @examples
#' \dontrun{
#'
#' job_edit("myjob", owner = "hayley")
#' job_edit("myjob", status = "inactive")
#' job_edit("myjob", priority = 2, description = "Just a job")
#' }
job_edit <- function(name, ...) {

  # capture dots
  dots <- capture_dots(...)  # list of expressions
  dots <- purrr::map(dots, eval) # evaluate them

  # substitute real names if necessary
  if(!is.null(dots$team)) {
    dots$team <- real_name(dots$team)
  }
  if(!is.null(dots$owner)) {
    dots$owner <- real_name(dots$owner)
  }

  # read the jobs data
  jobs <- job_read()

  # overwrite/append fields
  j <- jobs[[name]]
  j[names(dots)] <- dots
  jobs[[name]] <- j

  # ensure that the edits produce a valid job state
  jobs[[name]] <- validate_job(jobs[[name]])

  # write
  job_write(jobs)
}

#' Add or remove people from a team
#'
#' @param name name of project to be edited
#' @param add character vector of names to add to the team
#' @param remove character vector of names to remove from the team
#' @details The role of \code{job_edit_team()} is to make it a little easier to
#' alter the set of names listed in the "team" field of a job. The same task
#' could be done with \code{job_edit()} but it is cumbersome to do so. The
#' function first appends any names in \code{add} to the team (ignoring
#' duplicates), and then removes any names listed in \code{remove}. The function
#' checks all elements of \code{add} and \code{remove} against the table of
#' known nicknames, and substitutes the full name in place of a nickname.
#' Note that it is not possible to remove the "owner" of a job using this
#' method, as this would leave the job without an owner. To remove the owner,
#' first use \code{job_edit()} to change the owner, and then use
#' \code{job_edit_team()} to remove the former owner from the team.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' job_edit_team("myjob", add = "hayley")
#' job_edit_team("myjob", add = c("hayley", "sarah"))
#' job_edit_team("myjob", remove = c("hayley", "sarah"))
#'
#' # if "hayley" is the current owner"
#' job_edit("myjob", owner = "sarah") # transfers the ownership to sarah
#' job_edit_team("myjob", remove = "hayley") # removes hayley entirely
#' }
job_edit_team <- function(name, add = NULL, remove = NULL) {

  jobs <- job_read()

  if(!is.null(add)) {
    add <- real_name(add)
    jobs[[name]]$team <- unique(c(jobs[[name]]$team, add))
  }

  if(!is.null(remove)) {
    remove <- real_name(remove)

    # cannot remove owner
    if(jobs[[name]]$owner %in% remove) {
      warning(
        "cannot remove owner from a team: use job_edit() to change owner first",
        call. = FALSE
      )
      remove <- setdiff(remove, jobs[[name]]$owner)
    }

    jobs[[name]]$team <- setdiff(jobs[[name]]$team, remove)
  }

  job_write(jobs)
}


#' Add or edit urls associated with a job
#'
#' @param name name of the project to edit
#' @param site string with the site nickname (e.g., "github")
#' @param link string with the link to the site
#' @export
#' @details The role of \code{job_edit_url()} is to make it a easier to
#' change a URL associated with a job.
#' @examples
#' \dontrun{
#'
#' job_edit_url("myjob", "github", "https://github.com/myusername/myrepo")
#'
#' }
#
job_edit_url <- function(name, site, link) {

  # read the jobs data
  jobs <- job_read()
  urls <- jobs[[name]]$urls

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
  jobs[[name]]$urls <- urls

  # ensure that the edits produce a valid job state
  jobs[[name]] <- validate_job(jobs[[name]])

  # write
  job_write(jobs)
}

#' Delete a job
#'
#' @param name name of the job to delete
#' @export
#' @details Deletes a job from the JSON file. At the moment, it does so without
#' asking for the user to confirm, so be careful.
#' @examples
#' \dontrun{
#'
#' job_delete("myjob")
#' }
job_delete <- function(name) {
  jobs <- job_read()
  jobs[[name]] <- NULL
  job_write(jobs)
}

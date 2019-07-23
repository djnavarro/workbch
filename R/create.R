#' Create a new job
#'
#' @param jobname name of the job to create
#' @param description brief description of the job
#' @param status should be "active", "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname
#' @param team should be a vector of names/nicknames
#' @param priority numeric
#' @param deadline a date
#' @param path path to the job home directory
#' @param hidden hide job (default = FALSE)
#' @export
create_job <- function(jobname, description, owner, status = NULL,
                    team = NULL, priority = NULL, deadline = NULL,
                    path = NULL, hidden = NULL) {

  # read jobs file and check the names of the jobs
  jobs <- job_read()
  job_names <- purrr::map_chr(jobs, function(j) {j$jobname})

  # if it already exists, throw error
  if(jobname %in% job_names) {
    stop("a job already exists with name '", jobname, "'", call. = FALSE)
  }

    # specify the defaults for other fields
    if(is.null(status)) {status <- "active"}
    if(is.null(team)) {team <- character(0)}
    if(is.null(priority)) {priority <- 1}
    if(is.null(deadline)) {deadline <- NA_character_}
    if(is.null(path)) {path <- NA_character_}
    if(is.null(hidden)) {hidden <- FALSE}

    # parse the names and make sure the owner is on the team
    owner <- real_name(owner)
    team <- real_name(team)
    if(!(owner %in% team)) {
      team <- c(owner, team)
    }

    # append the new job
    jobs[[jobname]] <- new_job(
      jobname = jobname,
      description = description,
      owner = owner,
      status = status,
      team = team,
      priority = priority,
      deadline = deadline,
      path = path,
      urls = empty_url(),
      notes = empty_note(),
      tasks = empty_task(),
      hidden = hidden
    )

  # write the file and return
  job_write(jobs)
}





#' Create in bulk by searching for git repos
#'
#' @param proj_dir where to look for git repositories
#' @param workbch_dir where to store workbench files
#'
#' @export
create_by_git <- function(proj_dir, workbch_dir = getOption("workbch.home")) {

  # ensure that we have set the workbench home directory
  set_home(workbch_dir)

  # find all git repositories
  repos <- list.files(
    path = proj_dir, pattern = "\\.git$", full.names = TRUE,
    recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  paths <- normalizePath(gsub("\\.git$", "", repos))

  # guess the job name from repo name
  job_names <- gsub("^.*[/\\]", "", paths)

  # create minimal person listing
  set_person("me", "me")

  # now create
  purrr::walk2(
    .x = paths,
    .y = job_names,
    .f = function(.x, .y) {
      cat("adding: ", .x, "\n")
      set_job(
        jobname = .y,
        description = .y,
        owner = "me",
        path = .x
        )
    })

}

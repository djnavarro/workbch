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
make_job <- function(jobname, description, owner = NULL, status = NULL,
                       team = NULL, priority = NULL, deadline = NULL,
                       path = NULL, hidden = NULL) {

  # read jobs file and check the names of the jobs
  jobs <- job_read()
  job_names <- purrr::map_chr(jobs, function(j) {j$jobname})

  # if a job with this name already exists, throw error
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

  # set the owner
  if(is.null(owner)) {
    owner <- default_person()
  } else {
    owner <- real_name(owner)
  }

  # set the team
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
#' @param dir where to look for git repositories
#' @param owner the owner for created jobs (a nick name)
#' @param status the status for created jobs
#' @param priority the priority of created jobs
#' @param deadline the deadline of created jobs
#' @param hidden the visibility of created jobs
#'
#' @examples
#' \dontrun{
#'
#' make_jobs_by_git(dir = "~/my_projects", owner = "me")
#' }
#'
#' @export
make_jobs_by_git <- function(dir, owner = NULL, status = "active", priority = 1,
                               deadline = NA, hidden = FALSE) {

  # set owner
  if(is.null(owner)) {
    owner <- default_person()
  } else {
    owner <- real_name(owner)
  }

  # find all git repositories
  found_paths <- list.files(
    path = dir, pattern = "\\.git$", full.names = TRUE,
    recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  found_paths <- gsub("\\.git$", "", found_paths)
  found_paths <- normalizePath(found_paths, winslash = .Platform$file.sep)

  # guess the job name by splitting the path to find terminal folder
  found_jobnames <- strsplit(found_paths, .Platform$file.sep, fixed = TRUE)
  found_jobnames <- purrr::map_chr(found_jobnames, ~ .x[length(.x)])
  found_jobnames <- janitor::make_clean_names(found_jobnames)

  # load the existing jobs
  jobs <- job_read()
  job_names <- get_jobnames(jobs)
  job_paths <- get_paths(jobs)
  job_paths <- suppressWarnings( # suppress b/c some jobs may have no path
    normalizePath(job_paths, winslash = .Platform$file.sep)
  )

  # initialise output
  created_jobs <- tibble::tibble(jobname = character(0), path = character(0))
  n_known <- 0
  n_skipped <- 0
  n_created <- 0

  # add jobs
  for(i in seq_along(found_paths)) {

    # if the path is already mapped to a job, skip it
    if(found_paths[i] %in% job_paths) {
      n_known <- n_known + 1
      message("repo at '", found_paths[i], "' is listed... skipping")

    # for a new path, make guesses and ask the user
    } else {

      # ensure the found job name doesn't already exist
      while(found_jobnames[i] %in% job_names) {
        found_jobnames[i] <- paste0(found_jobnames[i], "X") # ... TODO: do this better!!!
      }

      # guess url for git repo
      found_remote <- git2r::remote_url(found_paths[i])[1] # use first
      site <- NA
      if(grepl("bitbucket|github|gitlab", found_remote)) { # if it's bb/gh/gl..
        site <- gsub(".*(bitbucket|github|gitlab).*", "\\1", found_remote)
        found_remote <- strsplit(found_remote, "/")[[1]]
        user <- found_remote[length(found_remote)-1]
        repo <- found_remote[length(found_remote)]
        repo <- gsub("\\.git$", "/", repo)
        if(site == "bitbucket") prefix <- "https://bitbucket.org/"
        if(site == "github") prefix <- "https://github.com/"
        if(site == "gitlab") prefix <- "https://gitlab.com/"
        url_path <- paste0(prefix, user, "/", repo)
      }

      # construct message to the for the user
      message("")
      message("unlisted repository found:")
      message("    ")
      message("    jobname:     ", found_jobnames[i])
      message("    owner:       ", owner)
      message("    path:        ", found_paths[i])
      if(!is.na(site)){
        message("    ", site, " url:  ", url_path)
      }
      message("    status:      ", status)
      message("    priority:    ", priority)
      message("    deadline:    ", deadline)
      message("    hidden:      ", hidden)
      message("    ")

      # make the user decide
      acc <- ""
      while(acc != "y" & acc != "n") {
        acc <- readline("    create job with these values? [y/n] ")
        acc <- tolower(acc)
        acc <- trimws(acc, "both")
      }
      message("")

      # if the user says no, skip
      if(acc == "n") {
        n_skipped <- n_skipped + 1
        message("    ... ok, skipping")
      }

      # if the user says yes, update
      if(acc == "y") {
        n_created <- n_created + 1

        # create new job... without url
        if(is.na(site)) {
          jobs[[found_jobnames[i]]] <- new_job(
            jobname = found_jobnames[i],
            description = found_jobnames[i],
            owner = owner,
            status = status,
            team = owner,
            priority = priority,
            deadline = deadline,
            path = found_paths[i],
            notes = empty_note(),
            tasks = empty_task(),
            hidden = hidden
          )

        # or else create new job... with a url
        } else {
          jobs[[found_jobnames[i]]] <- new_job(
            jobname = found_jobnames[i],
            description = found_jobnames[i],
            owner = owner,
            status = status,
            team = owner,
            priority = priority,
            deadline = deadline,
            path = found_paths[i],
            urls = new_url(site = site, link = url_path),
            notes = empty_note(),
            tasks = empty_task(),
            hidden = hidden
          )
        }

        # write it now, in case user aborts later
        job_write(jobs)

        # update output
        created_jobs <- dplyr::bind_rows(
          created_jobs,
          tibble::tibble(
            jobname = found_jobnames[i],
            path = found_paths[i]
          )
        )

        # update list of known paths/jobnames
        job_names <- get_jobnames(jobs)
        job_paths <- get_paths(jobs)
        job_paths <- suppressWarnings(
          normalizePath(job_paths)
        )

        message("    ... done! new job created!")
      }

    }

  }

  # print a summary for the user
  message("")
  message(n_known, " repositories were already known")
  message(n_skipped, " repositories were skipped by user")
  message(n_created, " repositories were created")
  message("")

  return(created_jobs)
}



#' Create a new job
#'
#' @param jobname name of the job to create
#' @param description brief description of the job
#' @param status should be "active", "inactive", "complete", "abandoned", "masked"
#' @param owner should be a name or a nickname
#' @param team should be a vector of names/nicknames
#' @param priority numeric
#' @param deadline a date
#' @param tags character vector of tags
#' @param path path to the job home directory
#' @export
make_job <- function(jobname, description, owner = NULL, status = NULL,
                     team = NULL, priority = NULL, deadline = NULL,
                     tags = NULL, path = NULL) {

  # make_job calls the constructor function at the end, which verifies all
  # input arguments. so the only verifications that occur here are those
  # needed by make_job itself

  # read jobs file and check the names of the jobs
  jobs <- job_read()
  job_names <- job_getnames(jobs)

  # make sure no job exists with this name
  verify_jobname(jobname)
  verify_jobmissing(jobname, jobs)

  # append the new job
  jobs[[jobname]] <- new_job(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    deadline = deadline,
    tags = tags,
    path = path,
    urls = empty_url(),
    tasks = empty_task()
  )

  # write the file and return
  job_write(jobs)
}



#' Make a task and attach to a job
#'
#' @param description brief description of the task
#' @param jobname name of the job the task attaches to
#' @param status should be "active" (default), "inactive", "complete", "abandoned"
#' @param owner should be a name or a nickname (defaults to job owner)
#' @param priority numeric (default is to match the job)
#' @param deadline a date (default is to match the job)
#' @export
make_task <- function(description, jobname = NULL, owner = NULL, status = NULL,
                     priority = NULL, deadline = NULL) {

  # read the jobs & verify the name
  jobs <- job_read()
  jobname <- jobname %||% job_getcurrent(jobs)
  verify_jobexists(jobname, jobs)

  # get this job
  jb <- jobs[[jobname]]

  # set defaults as needed
  if(is.null(owner)) {owner <- jb$owner}
  if(is.null(priority)) {priority <- jb$priority}
  if(is.null(hidden)) {hidden <- jb$hidden}

  # parse the deadline
  if(is.null(deadline)) {
    deadline <- jb$deadline
  } else {
    deadline <- format_date(deadline)
  }

  # parse the owner name and throw warning if not in team
  owner <- ppl_fullname(owner)
  if(!(owner %in% jb$team)) {
    warning("'", owner, "' is not on the team for '",
            jobname, "'", call. = FALSE)
  }

  # extract existing ids and construct a new one
  all_ids <- task_getids(jobs)
  id <- task_makeid(all_ids)

  # create the task object
  tsk <- new_task(
    jobname = jobname,
    id = id,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    deadline = deadline
  )

  # append it to the job
  if(identical(jb$tasks, list())) {
    jb$tasks <- tsk
  } else if(nrow(jb$tasks) == 0) {
    jb$tasks <- tsk
  } else {
    jb$tasks <- dplyr::bind_rows(jb$tasks, tsk)
  }

  # write it to the jobs list
  jobs[[jobname]] <- jb
  job_write(jobs)
}




#' Create in bulk by searching for git repos
#'
#' @param dir where to look for git repositories
#' @param owner the owner for created jobs (a nick name)
#' @param status the status for created jobs
#' @param priority the priority of created jobs
#' @param deadline the deadline of created jobs
#'
#' @examples
#' \dontrun{
#'
#' make_jobs_by_git(dir = "~/my_projects", owner = "me")
#' }
#'
#' @export
make_jobs_by_git <- function(dir, owner = NULL, status = NULL,
                             priority = NULL, deadline = NULL) {

  # set & verify owner
  owner <- ppl_parseowner(owner)

  # verification step for other inputs
  verify_status(status)
  verify_priority(priority)
  verify_deadline(deadline)

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
  job_names <- job_getnames(jobs)
  job_paths <- job_getpaths(jobs)
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
        found_remote <- strsplit(found_remote, "[/:]")[[1]]
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
            tasks = empty_task()
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
            tasks = empty_task()
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
        job_names <- job_getnames(jobs)
        job_paths <- job_getpaths(jobs)
        job_paths <- suppressWarnings(normalizePath(job_paths))

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



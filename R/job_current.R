
job_getcurrent <- function(jobs) {

  # search heuristic:
  # - look first the active RStudio project
  # - if that doesn't do it, look at working directory
  # - if that doesn't do it, throw error

  # get all job names and paths
  job_names <- pull_jobnames(jobs)
  job_paths <- pull_jobpaths(jobs)

  # restrict to jobs with non-NA paths & normalise
  known <- !is.na(job_paths)
  job_paths <- normalizePath(job_paths[known])
  job_names <- job_names[known]

  # preferentially use the RStudio project path
  project_path <- NULL
  if(rstudioapi::isAvailable()) {
    project_path <- rstudioapi::getActiveProject() # NULL if no project
  }

  # attempt to match against project path & return if successful
  if(!is.null(project_path)) {
    match_ind <- which(job_paths == project_path)
    if(length(match_ind) > 0) {
      matched_job <- job_names[match_ind]
      message("using job '", matched_job, "'")
      return(matched_job)
    }
  }

  # attempt to match against the working directory & return if successful
  working_dir <- getwd()
  split_job_paths <- strsplit(job_paths, .Platform$file.sep)
  split_wd <- strsplit(working_dir, .Platform$file.sep)[[1]]

  # find match
  match_lgl <- purrr::map_lgl(split_job_paths, function(x) {
    len <- length(x);
    if(length(split_wd) < len) {
      return(FALSE) # if wd is shorter than job path, it doesn't match
    }
    return(identical(x, split_wd[1:len])) # if wd is a sub dir it matches
  })
  match_ind <- which(match_lgl)

  if(length(match_ind) == 1) {
    matched_job <- job_names[match_ind]
    message("using job '", matched_job, "'")
    return(matched_job)
  }

  stop("could not detect current job", call. = FALSE)
}

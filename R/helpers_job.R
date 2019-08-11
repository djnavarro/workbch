# contains helper functions that are oriented towards jobs


# read and write ----------------------------------------------------------

job_file <- function() {
  file.path(workbch_gethome(), "workbch_jobs.json")
}

# read project data from JSON file, and preprocess to deal with
# limitations to JSON storage
job_read <- function() {
  if(file.exists(job_file())) {

    jobs <- jsonlite::fromJSON(job_file())
    jobs <- purrr::map(jobs, function(j) {

      # empty lists become data frames in some cases
      if(class(j$urls) == "list") {j$urls <- empty_url()}
      if(class(j$tasks) == "list") {j$tasks <- empty_task()}
      if(is.null(j$tasks)) {j$tasks <- empty_task()}

      # empty lists become character vectors in others
      if(class(j$team) == "list") {j$team <- character(0)}
      if(class(j$tags) == "list") {j$tags <- character(0)}

      # NA_character_ is read as NA (logical) due to JSON limits
      # so we need to catch this and correct for type stability
      if(class(j$deadline) == "logical") {
        j$deadline <- as.character(j$deadline)
      }
      if(class(j$path) == "logical") {
        j$path <- as.character(j$path)
      }


      # don't let tags become matrices
      if(class(j$tags) == "matrix") {j$tags <- as.vector(j$tags)}

      # coerce to tibbles
      j$urls <- tibble::as_tibble(j$urls)
      j$tasks <- tibble::as_tibble(j$tasks)

      return(j)
    })
    return(jobs)
  }
  return(NULL)
}

# write project data to JSON file (TODO: this can be condensed to
# jsonlite::writeJSON presumably)
job_write <- function(jobs) {
  job_str <- jsonlite::toJSON(jobs, pretty = TRUE)
  writeLines(job_str, job_file())
}




# pull information --------------------------------------------------------


job_getnames <- function(jobs) {
  if(is.null(jobs)) { return(character(0)) }
  return(purrr::map_chr(jobs, function(j) {j$jobname}))
}

job_getpaths <- function(jobs) {
  if(is.null(jobs)) { return(character(0)) }
  return(purrr::map_chr(jobs, function(j) {j$path}))
}

job_getcurrent <- function(jobs) {

  # search heuristic:
  # - look first the active RStudio project
  # - if that doesn't do it, look at working directory
  # - if that doesn't do it, throw error

  # get all job names and paths
  job_names <- job_getnames(jobs)
  job_paths <- job_getpaths(jobs)

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




# check information -------------------------------------------------------

# throw warning if a job path does not exist
job_pathcheck <- function(jobname, path) {
  if(length(path) > 0) {
    bad <- which(!dir.exists(as.character(path)))
    if(length(bad) > 0) {
      for(b in bad) {
        if(!is.na(path[b])) {
          warning("The path for job '", jobname[b], "' is set to '",
                  path[b], "' but does not exist", call. = FALSE)
        }
      }
    }
  }
  return(invisible(TRUE))
}



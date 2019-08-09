# validator functions

# throw error if the job doesn't exist
verify_jobname <- function(jobname, jobs) {
  if(length(jobname) != 1 | !is.character(jobname)) {
    stop("jobname must be character and length 1", call. = FALSE)
  }

  job_names <- purrr::map_chr(jobs, function(j) {j$jobname})
  if(!(jobname %in% job_names)) {
    stop("there is no job named '", jobname, "'", call. = FALSE)
  }
  return(invisible(NULL))
}

# throw warning if a job path does not exist
verify_path <- function(jobname, path) {
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
}

# throw error if an unknown status is used
verify_status <- function(status) {
  if(length(status) != 1) {
    stop("job status must have length 1", call. = FALSE)
  }
  if(!(status %in% c("active", "inactive", "complete", "abandoned", "masked"))) {
    stop("job status must be 'active', 'inactive', 'complete', 'abandoned' or 'masked'", call. = FALSE)
  }
}

# throw error if a priority is not a positive integer
verify_priority <- function(priority) {
  if(length(priority) != 1) {
    stop("job priority must be a positive integer", call. = FALSE)
  }
  if(!is.numeric(priority)) {
    stop("job priority must be a positive integer", call. = FALSE)
  }
  if(priority != as.integer(priority)) {
    stop("job priority must be a positive integer", call. = FALSE)
  }
  if(priority < 1) {
    stop("job priority must be a positive integer", call. = FALSE)
  }
}

# throw error if description is not a length 1 character
verify_description <- function(description) {
  if(length(description) != 1 | !is.character(description)) {
    stop("description must be character and length 1", call. = FALSE)
  }
}


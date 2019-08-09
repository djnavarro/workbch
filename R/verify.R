# validator functions

# throw error if the job doesn't exist
verify_jobname <- function(jobname, jobs) {
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


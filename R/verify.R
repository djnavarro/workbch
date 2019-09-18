# validator functions


# generic checks ----------------------------------------------------------

# a lot of the validation checks involve verifying an an input argument
# is length one character so let's not duplicate code my dear...
verify_onestring <- function(object) {
  # tests for length, character, and lack-of-dimension
  if(length(object) != 1 | !is.character(object) | !is.null(dim(object))) {
    stop(deparse(substitute(object)), " must be character and length 1",
         call. = FALSE)
  }
  return(invisible(TRUE))
}

# similar deal as above really
verify_character <- function(object) {
  if(!is.character(object) | !is.null(dim(object))) {
    stop(deparse(substitute(object)), " must be character",
         call. = FALSE)
  }
  return(invisible(TRUE))
}


# argument checks ---------------------------------------------------------


# throw error if the jobname is invalid
verify_jobname <- function(jobname) {
  verify_onestring(jobname)
}

# throw error if the path is invalid
verify_path <- function(path) {
  verify_onestring(path)
}

# throw error if an unknown status is used
verify_status <- function(status) {
  verify_onestring(status)

  if(!(status %in% c("active", "inactive", "complete", "abandoned", "masked"))) {
    stop("job status must be 'active', 'inactive', 'complete', 'abandoned' or 'masked'", call. = FALSE)
  }
  return(invisible(TRUE))
}

# throw error if a priority is not a positive integer
verify_priority <- function(priority) {
  if(length(priority) != 1) {
    stop("job priority must be a positive integer", call. = FALSE)
  }
  if(is.na(priority) | is.nan(priority) | is.infinite(priority)) {
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
  return(invisible(TRUE))
}

# throw error if description is not a length 1 character
verify_description <- function(description) {
  verify_onestring(description)
}

verify_site <- function(site) {
  verify_onestring(site)
}

verify_link <- function(link) {
  verify_onestring(link)
}

verify_owner <- function(owner) {
  verify_onestring(owner)
}


# status checks -----------------------------------------------------------

# ensure there is no job called jobname
verify_jobmissing <- function(jobname, jobs, strict = TRUE) {
  missing <- !(jobname %in% pull_jobnames(jobs))
  if(strict & missing == FALSE)  {
    stop("a job already exists with name '", jobname, "'", call. = FALSE)
  }
  return(invisible(TRUE))
}

# ensure there is no job called jobname
verify_jobexists <- function(jobname, jobs, strict = TRUE) {
  present <- jobname %in% pull_jobnames(jobs)
  if(strict & present == FALSE)  {
    stop("no job exists with name '", jobname, "'", call. = FALSE)
  }
  return(invisible(TRUE))
}

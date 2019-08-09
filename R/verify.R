# validator functions

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

# throw error if the jobname is invalid
verify_jobname <- function(jobname) {
  verify_onestring(jobname)
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
  return(invisible(TRUE))
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

# throw error if lubridate::dmy can't handle the deadline
verify_deadline <- function(deadline) {
  safe_date <- purrr::safely(function(x) {lubridate::dmy(x, quiet = TRUE)})
  out <- safe_date(deadline)

  # check dmy doesn't throw an error
  if(!is.null(out$error)) {
    stop("deadline must be a day-month-year format", call. = FALSE)
  }

  # check input is length 1
  if(length(out$result) != 1) {
    stop("deadline must be length 1", call. = FALSE)
  }

  # check that the only NA allowed is if input is NA
  if(is.na(out$result) & !is.na(deadline)) {
    stop("deadline must be a day-month-year format", call. = FALSE)
  }

  return(invisible(TRUE))
}

verify_site <- function(site) {
  verify_onestring(site)
}

verify_link <- function(link) {
  verify_onestring(link)
}

# for now, the name "verifications" merely check
# that the user has specified one string, and
# defers checks against database to real_name()
verify_nickname <- function(nickname) {
  verify_onestring(nickname)
}

verify_fullname <- function(fullname) {
  verify_onestring(fullname)
}

verify_owner <- function(owner) {
  verify_onestring(owner)
}

verify_makedefault <- function(make_default) {
  if(length(make_default) != 1 || !is.logical(make_default) || is.na(make_default)) {
    stop("make_default must be TRUE or FALSE", call. = FALSE)
  }
  return(invisible(TRUE))
}

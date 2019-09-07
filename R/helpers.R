
# read and write ----------------------------------------------------------

job_file <- function() {
  file.path(getOption("workbch.home"), "workbch_jobs.json")
}

# read project data from JSON file, and preprocess to deal with
# limitations to JSON storage
job_read <- function() {
  if(file.exists(job_file())) {

    jobs <- jsonlite::fromJSON(job_file())
    jobs <- purrr::map(jobs, function(j) {

      # empty lists become data frames in some cases
      if(class(j$urls) == "list") {j$urls <- empty_url()}

      # empty lists become character vectors in others
      if(class(j$team) == "list") {j$team <- character(0)}
      if(class(j$tags) == "list") {j$tags <- character(0)}

      if(class(j$path) == "logical") {
        j$path <- as.character(j$path)
      }

      # don't let tags become matrices
      if(class(j$tags) == "matrix") {j$tags <- as.vector(j$tags)}

      # coerce to tibbles
      j$urls <- tibble::as_tibble(j$urls)

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

job_allpaths <- function(show_hidden = TRUE) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    if(!is.null(x$path)) {
      return(tibble::as_tibble(x[c("jobname", "path", "idstring")]))
    } else {
      return(tibble::tibble(jobname = character(0), path = character(0), idstring = character(0)))
    }
  })
  job_tbl <- dplyr::arrange(job_tbl, jobname)
  job_tbl <- dplyr::filter(job_tbl, !is.na(path))

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- apply_mask(job_tbl)}

  # throw warnings
  return(as_wkbch_tbl(job_tbl))
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

# returns the names of jobs for which the expected sentinel files are not found
# at the expected location
job_missingsentinels <- function() {
  dat <- job_allpaths()
  info <- purrr::transpose(dat)
  missing <- purrr::map_chr(info, function(job) {
    f <- file.path(normalizePath(job$path, mustWork = FALSE), ".workbch")
    if(file.exists(f)) {
      s <- readLines(f)
      if(s[1] == job$jobname & s[2] == job$idstring) {
        return("")
      }
    }
    return(job$jobname)
  })
  missing <- missing[missing != ""]
  return(missing)
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

job_checksentinels <- function() {
  missing <- job_missingsentinels()
  if(length(missing) > 0) {
    warning(
      "Some job folders have moved or been deleted. Run workbch_findjobs() to fix",
      call. = FALSE
    )
  }
}

split_tags <- function(tags) {
  tags <- strsplit(tags, "|", fixed = TRUE)[[1]]
  tags <- trimws(tags, which = "both")
  return(tags)
}

split_url <- function(url) {
  url <- strsplit(url, "|", fixed = TRUE)[[1]]
  url <- trimws(url, which = "both")
  return(url)
}



# miscellaneous helpers ---------------------------------------------------

# find the jobs that need to be hidden and hide them
apply_mask <- function(tbl) {
  tbl <- dplyr::filter(tbl, status %in% c("active", "inactive"))
  return(tbl)
}

# repeatedly ask user until a stop signal is reached
multireadline <- function(prompt, stop = "") {
  out <- character(0)
  ask <- "BLAH"
  while(ask != stop) {
    ask <- readline(prompt = prompt)
    out <- c(out, ask)
  }
  return(out[-length(out)])
}

# generate 10 letter idstring
idstring <- function() {
  paste0(sample(c(letters, LETTERS), 10, TRUE), collapse="")
}

# locate sentinal files
find_sentinels <- function(dirs = getOption("workbch.search")) {
  unlist(purrr::map(dirs, function(d) {
    list.files(path = d, pattern = "\\.workbch$", recursive = TRUE,
               all.files = TRUE, full.names = TRUE)
  }))
}

# write a sentinal file
write_sentinel <- function(dir, jobname, idstring) {
  dir <- normalizePath(dir)
  file <- normalizePath(file.path(dir, ".workbch"))
  writeLines(text = c(jobname, idstring), con = file)
}

# print methods


# Specify a print method for a workbench tibble
#' @export
print.wkbch_tbl <- function(x, n = 100, ...) {
  x <- de_wkbch_tbl(x)
  print(x, n = n, ...)
}

# strange coercion
as_wkbch_tbl <- function(x) {
  class(x) <- c("wkbch_tbl", class(x))
  return(x)
}

# remove wkbch class
de_wkbch_tbl <- function(x) {
  class(x) <- setdiff(class(x), "wkbch_tbl")
  return(x)
}



# utility functions -------------------------------------------------------

# returns a list of expressions
capture_dots <- function(...) {
  as.list(substitute(list(...)))[-1L]
}

#' @importFrom rlang %||%
NULL


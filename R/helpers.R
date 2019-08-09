# miscellaneous helper functions used throughout the
# package. none of these are exported, so the interface
# hasn't been thought through

# the directory where project records should be stored
job_home <- function() {
  getOption("workbch.home")
}

job_file <- function() {
  file.path(job_home(), "workbch_jobs.json")
}

ppl_file <- function() {
  file.path(job_home(), "workbch_people.csv")
}

# read project data from JSON file if it exists, and preprocess to deal with
# limitations to JSON storage
job_read <- function() {
  if(file.exists(job_file())) {

    jobs <- jsonlite::fromJSON(job_file())
    jobs <- purrr::map(jobs, function(j) {

      # empty lists become data frames
      if(class(j$urls) == "list") {j$urls <- empty_url()}
      if(class(j$tasks) == "list") {j$tasks <- empty_task()}

      # don't let tags become matrices
      if(class(j$tags) == "matrix") {j$tags <- as.vector(j$tags)}

      # coerce to tibbles
      j$urls <- tibble::as_tibble(j$urls)
      j$tasks <- tibble::as_tibble(j$tasks)

      return(j)
    })
    return(jobs)
  }
  return(empty_job())
}

# read the tasks
task_read <- function() {
  jobs <- job_read()
  tasks <- purrr::map_dfr(jobs, function(j) {j$tasks})
  return(tasks)
}

# write project data to JSON file
job_write <- function(jobs) {
  job_str <- jsonlite::toJSON(jobs, pretty = TRUE)
  writeLines(job_str, job_file())
}

# read people data from CSV file if it exists
ppl_read <- function() {
  if(file.exists(ppl_file())) {
    return(suppressMessages(readr::read_csv(ppl_file())))
  }
  return(
    tibble::tibble(
      fullname = character(0),
      nickname = character(0),
      default = logical(0))
    )
}

# write people data to CSV file
ppl_write <- function(ppl) {
  readr::write_csv(ppl, ppl_file())
}

# returns a list of expressions
capture_dots <- function(...) {
  as.list(substitute(list(...)))[-1L]
}

# retrieve the largest known task number
current_max_task_id <- function(jobs) {
  task_ids <- purrr::map_dbl(jobs, function(j) {
    tsk <- j$tasks
    if(!is.null(tsk) & length(tsk$id) > 0) {
      return(max(tsk$id))
    }
    return(0)
  })
  return(max(task_ids))
}

# if name is a nickname, substitute with the real one
real_name <- function(nickname) {

  # if there aren't any names, return early
  if(length(nickname) == 0) {return(nickname)}

  # read the nicknames and substitute
  ppl <- ppl_read()

  # search for the name
  fullname <- character(length(nickname))
  for(i in 1:length(nickname)) {

    # if it's a known nickname, substitute the full
    if(nickname[i] %in% ppl$nickname) {
      fullname[i] <- ppl$fullname[ppl$nickname == nickname[i]]

    } else {
      if(nickname[i] %in% ppl$fullname) {
        nn <- ppl$nickname[ppl$fullname == nickname[i]]
        warning("'", nickname[i], "' has a known nick name '", nn, "'", call. = FALSE)
      } else{
        warning("'", nickname[i], "' is not a known nick name", call. = FALSE)
      }
    }
  }
  return(fullname)
}

# find the jobs (or tasks) that need to be hidden and hide them
hide_jobs <- function(job_tbl) {
  job_tbl <- dplyr::filter(job_tbl, status %in% c("active", "inactive"))
  return(job_tbl)
}

# get the default person and throw
default_person <- function(strict = TRUE) {
  ppl <- ppl_read()
  def <- ppl$fullname[ppl$default == TRUE]
  if(strict & length(def) == 0) {
    stop("'owner' argument must be specified if default person is set",
         call. = FALSE)
  }
  return(def)
}

get_jobnames <- function(jobs) {
  if(length(jobs) == 0) {return(character(0))}
  return(purrr::map_chr(jobs, function(j) {j$jobname}))
}

get_paths <- function(jobs) {
  if(length(jobs) == 0) {return(character(0))}
  return(purrr::map_chr(jobs, function(j) {j$path}))
}

# used in generating the markdown output
prettify <- function(x) {
  kableExtra::kable_styling(
    knitr::kable(x),
    "striped",
    font_size = 12)
}

format_date <- function(date) {
  date <- lubridate::dmy(date)
  date <- format(date, format = "%d %b %Y")
  return(date)
}

# search heuristic:
# - look first the active RStudio project
# - if that doesn't do it, look at working directory
# - if that doesn't do it, throw error
get_current_jobname <- function(jobs) {

  # get all job names and paths
  job_names <- purrr::map_chr(jobs, ~ .x$jobname)
  job_paths <- purrr::map_chr(jobs, ~ .x$path)

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


# check if job name exists
job_exists <- function(jobname, jobs) {
  job_names <- purrr::map_chr(jobs, function(j) {j$jobname})
  return(jobname %in% job_names)
}

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
      if(class(j$notes) == "list") {j$notes <- empty_note()}
      if(class(j$tasks) == "list") {j$tasks <- empty_task()}

      # coerce to tibbles
      j$urls <- tibble::as_tibble(j$urls)
      j$notes <- tibble::as_tibble(j$notes)
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

# check the job
validate_job <- function(job) {

  # more checks here!!!

  return(job)
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

# find the jobs that need to be hidden and hide them
hide_jobs <- function(jobs, job_tbl) {

  # find them
  hidden <- purrr::map_chr(jobs, function(x) {
    if(!is.null(x$hidden)) {
      if(x$hidden == TRUE) {
        return(x$jobname)
      }
    }
    return("")
  })
  hidden <- hidden[hidden != ""]

  # remove them
  job_tbl <- dplyr::filter(job_tbl, !(jobname %in% hidden))

  return(job_tbl)
}

# throw warning if a job path does not exist
verify_paths <- function(jobname, path) {
  if(!is.null(path)) {
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
}


# throw error if the job doesn't exist
verify_jobname <- function(jobname, jobs) {
  job_names <- purrr::map_chr(jobs, function(j) {j$jobname})
  if(!(jobname %in% job_names)) {
    stop("there is no job named '", jobname, "'", call. = FALSE)
  }
  return(invisible(NULL))
}

# get the default person - currently it's just the first person (TODO make this better)
default_person <- function() {
  ppl <- ppl_read()
  def <- ppl$fullname[1]
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


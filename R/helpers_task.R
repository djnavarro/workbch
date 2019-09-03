
task_getid <- function(ref, tasks) {

  # if ref is numeric match against id
  if(is.numeric(ref)) {
    if(length(which(tasks$id == ref)) == 0) {
      stop("no task with id '", ref, "' exists", call. = FALSE)
    }
    return(ref)
  }

  # if ref is character match against description
  if(is.character(ref)) {
    str <- with(tasks, paste(jobname, description, owner))
    str <- tolower(str)
    hits <- grep(ref, str)

    # error if no matches
    if(length(hits) == 0) {
      stop("no task matching '", ref, "' found", call. = FALSE)
    }

    # check with user in interactive mode
    if(interactive()) {
      cat("[0] none of these\n")
      for(i in 1:length(hits)) {
        cat("[", i, "] ", tasks$jobname[hits[i]], ": ",
            tasks$description[hits[i]], "\n", sep = "")
      }
      usr <- readline("make selection: ")
      if(!(usr %in% as.character(1:length(hits)))) {
        stop("aborted by user", call. = FALSE)
      }
      return(hits[as.numeric(usr)])
    }

    # if not interactive throw error unless it matches exactly one job
    if(length(hits) == 1) {
      return(hits)
    }

    stop("`ref` must match exactly one task", call. = FALSE)
  }

  stop("`ref` must be numeric or character", call. = FALSE)
}

# returns a vector of all task id numbers
task_ids <- function(jobs) {
  purrr::reduce(
    .x = jobs,
    .f = function(id, j) { c(id, (j$tasks$id %||% numeric(0L))) },
    .init = numeric(0)
  )
}

# make new id value by incrementing maxid (fix this)
task_makeid <- function(ids) {
  max(ids) + 1
}

# append a new task to an existing list of tasks
task_append <- function(old, new) {
  if(identical(old, list())) { return(new) }
  if(nrow(old) == 0) { return(new) }
  return(dplyr::bind_rows(old, new))
}

# read the tasks
task_read <- function() {
  jobs <- job_read()
  if(is.null(jobs)) {
    tasks <- empty_task()
  } else {
    tasks <- purrr::map_dfr(jobs, function(j) {j$tasks})
  }
  return(tasks)
}

task_promptref <- function(tsk) {

  # prompt doesn't work unless interactive
  if(!interactive()) {
    stop("Task reference must be specified", call. = FALSE)
  }

  # get user response
  print(tsk)
  cat("\n")
  ref <- readline("  Enter the id of task... ")
  ref <- as.numeric(ref)

  # throw error if it's not numeric
  if(is.na(ref)) {
    stop("Task id must be numeric", call. = FALSE)
  }

  # throw error if this is not a recognised task
  if(!(ref %in% tsk$id)) {
    stop("Task ", ref, " does not exist", call. = FALSE)
  }

  return(ref)
}




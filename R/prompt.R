prompt_jobname <- function() {
  opts <- view_jobs()$jobname
  if(length(opts)>9) opts <- opts[1:9]
  for(i in 1:length(opts)) {
    cat(i, ":", opts[i], "\n")
  }
  cat("other: please type jobname\n")
  jnum <- readline("which job? ")
  if(jnum %in% as.character(1:length(opts))) {
    jobname <- opts[as.numeric(jnum)]
  } else {
    jobname <- jnum
  }
  return(jobname)
}


prompt_taskref <- function(tsk) {

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


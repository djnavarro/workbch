
#' View the information stored about a job
#'
#' @param jobname Name of the job to show (defaults to the current job)
#'
#' @details The \code{job_glimpse()} function displays parameter values for the
#' job specified in the \code{jobname} argument. If no argument is specified
#' \code{job_glimpse()} will attempt to guess the "current" job by looking at
#' any open RStudio projects. If no project is open (or the RStudio API is not
#' available) it attempts to guess by looking at the working directory. The
#' output is presented to the user as a message.
#'
#' @return Invisibly returns the job as a list
#' @export
job_glimpse <- function(jobname = NULL) {

  # read jobs and use the current job if the user does
  # not specify an argument
  jobs <- job_read()
  if(is.null(jobname)) {
    jobname <- suppressMessages(job_getcurrent(jobs))
  } else {
    verify_jobname(jobname)
    verify_jobexists(jobname, jobs)
  }

  # obtain the job
  jb <- jobs[[jobname]]

  msg_str <- ""
  msg_str <- paste0(msg_str, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  jobname:      ", jb$jobname, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  description:  ", jb$jobname, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  owner:        ", jb$owner, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  priority:     ", jb$priority, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  status:       ", jb$status, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  path:         ", jb$path, "\n", collapse = "")
  msg_str <- paste0(msg_str, "  tags:         ", paste0(jb$tags, collapse = " | "), "\n", collapse = "")
  if(nrow(jb$urls) > 0) {
    for(i in 1:nrow(jb$urls)) {
      site <- jb$urls$site[i]
      npad <- max(13-nchar(site), 1)
      pad <- paste(rep(" ", npad), collapse = "")
      msg_str <- paste0(msg_str, "  ", site, ":", pad, jb$urls$link[i], "\n", collapse = "")
    }
  }

  message(msg_str)
  return(invisible(jb))
}

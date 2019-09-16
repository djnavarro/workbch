
#' View the information stored about a job
#'
#' @param jobname Name of the job to show (defaults to the current job)
#'
#' @return Invisibly returns the job as a list
#' @export
job_glimpse <- function(jobname = NULL) {

  # read jobs and use the current job if the user does
  # not specify an argument
  jobs <- job_read()
  if(is.null(jobname)) {
    jobname <- suppressMessages(job_getcurrent(jobs))
  }

  # obtain the job
  jb <- jobs[[jobname]]

  cat("\n")
  cat("  jobname:      ", jb$jobname, "\n")
  cat("  description:  ", jb$jobname, "\n")
  cat("  owner:        ", jb$owner, "\n")
  cat("  priority:     ", jb$priority, "\n")
  cat("  status:       ", jb$status, "\n")
  cat("  path:         ", jb$path, "\n")
  cat("  tags:         ", jb$tags, "\n")
  if(nrow(jb$urls) > 0) {
    for(i in 1:nrow(jb$urls)) {
      site <- jb$urls$site[i]
      npad <- max(14-nchar(site), 1)
      pad <- paste(rep(" ", npad), collapse = "")
      cat("  ", site, ":", pad, jb$urls$link[i], "\n", sep = "")
    }
  }

  return(invisible(jb))
}

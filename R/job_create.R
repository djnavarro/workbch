#' Create a new job
#'
#' @param jobname name of the job to create
#' @param description brief description of the job
#' @param status should be "active", "inactive", "complete", "abandoned", "masked"
#' @param owner should be a name or a nickname
#' @param priority numeric
#' @param tags a string containing comma separated list of tags
#' @param path path to the job home directory
#'
#' @details The role of the \code{job_create()} function is to create new workbch job.
#' It can be called in two ways, interactively or programmatically. To call the
#' function interactively, R must be in interactive mode and the function should
#' be called with no arguments specified. When called in this fashion the user
#' will be presented with a sequence of prompts, asking them to specify each
#' of the parameters that define a job (e.g., a character string for \code{jobname},
#' a number for \code{priority}). When used interactively, you do not need to include
#' quote marks when entering a string: \code{job_create()} will coerce the input to
#' the appropriate format, and then append the created job to the job file.
#'
#' When called with programmatically, the user must specify any arguments in the
#' call to \code{job_create()}. The \code{jobname}, \code{description} and
#' \code{owner} arguments should be character strings of length 1, and all three
#' are mandatory. The \code{status} for a job should be one of the following
#' values: \code{"active"}, \code{"inactive"}, \code{"complete"}, \code{"abandoned"}
#' or \code{"masked"}. The \code{priority} for
#' a job should be a positive integer: the intent is that priority 1 is the
#' highest priority, followed by priority 2, and so one. The \code{tags} for a job
#' can be specified as a single string, using \code{|} as a separator character
#' (e.g., \code{tags = "research | statistics"} would create two tags for the job).
#' Finally, the \code{path} should specify the location of a folder containing the
#' project files.
#'
#' Note that, although jobs can also be associated with URLs (e.g., link to a
#' GitHub repository or a document on Overleaf), the \code{job_create()} function
#' does not (at this time) allow you to specify URLs. These can be added using
#' \code{job_modify()}.
#'
#' @return Invisibly returns a list containing the parameters for the job
#' @export
job_create <- function(jobname = NULL, description = NULL, owner = NULL,
                       status = NULL, priority = NULL, tags = NULL,
                       path = NULL) {

  use_prompt <- is.null(jobname) & is.null(description) & is.null(owner) &
    is.null(status) & is.null(priority) & is.null(tags) & is.null(path) &
    interactive()

  if(use_prompt) {

    cat("\nDetails of the new job:\n")
    cat("(Press enter to skip or use default values)\n\n")

    # elicit responses from user
    jobname     <- readline(            "  Job name............ ")
    description <- readline(            "  Description......... ")
    owner       <- readline(            "  Owner............... ")
    status      <- readline(            "  Status.............. ")
    priority    <- as.numeric(readline( "  Priority............ "))
    tags        <- readline(       "  Tags (comma separated) ........... ")
    path        <- readline(            "  Path................ ")

    # treat no response as default value
    if(jobname == "") jobname <- NULL
    if(description == "") description <- NULL
    if(owner == "") owner <- NULL
    if(status == "") status <- NULL
    if(is.na(priority)) priority <- NULL
    if(length(tags) == 0) tags <- NULL
    if(path == "") path <- NULL
  }

  if(is.null(jobname)) stop("'jobname' cannot be empty", call. = FALSE)
  if(is.null(description)) stop("'description' cannot be empty", call. = FALSE)
  if(is.null(owner)) stop("'owner' cannot be empty", call. = FALSE)

  # read jobs file and check the names of the jobs
  jobs <- job_read()
  job_names <- job_getnames(jobs)

  # make sure no job exists with this name
  verify_jobname(jobname)
  verify_jobmissing(jobname, jobs)

  # split the tags if necessary
  if(!is.null(tags)) {tags <- split_tags(tags)}

  # append the new job
  jb <- new_job(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    tags = tags,
    path = path,
    urls = empty_url()
  )
  jobs[[jobname]] <- jb

  # write the file
  job_write(jobs)

  # invisibly returns the created job
  return(invisible(jb))
}




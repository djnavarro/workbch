#' Create a new job
#'
#' @param jobname name of the job to create
#' @param description brief description of the job
#' @param status should be "active", "inactive", "complete", "abandoned", "masked"
#' @param owner should be a name or a nickname
#' @param priority numeric
#' @param tags a string containing comma separated list of tags
#' @param path path to the job home directory
#' @export
job_create <- function(jobname = NULL, description = NULL, owner = NULL, status = NULL,
                       priority = NULL, tags = NULL, path = NULL) {

  # make_job calls the constructor function at the end, which verifies all
  # input arguments. so the only verifications that occur here are those
  # needed by make_job itself

  if((is.null(jobname) | is.null(description)) & interactive()) {

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
  if(!is.null(tags)) {
    tags <- strsplit(tags, ",", fixed = TRUE)[[1]]
    tags <- trimws(tags, which = "both")
  }

  # append the new job
  jobs[[jobname]] <- new_job(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    tags = tags,
    path = path,
    urls = empty_url()
  )

  # write the file and return
  job_write(jobs)
}




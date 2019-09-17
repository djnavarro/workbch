#' View a list of all jobs
#'
#' @param search input query used to extract a subset of jobs
#' @param select character vector of columns to return
#'
#' @details The \code{job_list()} function is used to display a summary of the
#' jobs known to workbch, shown as a tibble with one row per job. By default
#' only those jobs with a \code{priority} value of 1 or 2 will be shown, and
#' only if their \code{status} is "active" or "inactive". Jobs that do not have
#' sufficient priority or whose status is "complete", "abandoned" or "masked"
#' are not shown.
#'
#' The default behaviour can be overridden by specifying a \code{search} string (or
#' object that can be coerced to a string). If the user specifies a value to
#' \code{search} then \code{job_list()} attempts to match the search string with a parameter
#' value. For instance \code{job_list("active")} will return all jobs that have status
#' \code{active} and \code{job_list(1)} will return all jobs that have priority 1. At
#' present this functionality requires that the \code{search} string be an exact value
#' (e.g., \code{search = "Dani"} would not match a job owned by \code{"Danielle"}),
#' though this may be extended in the future.
#'
#' The output is returned to the user as a tibble, and by default the columns
#' displayed are \code{jobname}, \code{owner}, \code{priority}, \code{status},
#' and \code{description}. The user can directly specify which columns to be
#' included using the \code{select} argument, which should be a character vector
#' specifying the names of the columns to include. In addition to the five
#' columns made available by default, the output can include the \code{path},
#' \code{tags} and \code{urls} associated with the job. For convenience, if you
#' set \code{select = NULL} then all columns will be returned.
#'
#' @return An object of class worbch_tbl. It is regular tibble with a slightly
#' modified print method that ensures all rows are printed in the output
#' @export
job_list <- function(
  search = NULL,
  select = c("jobname", "owner", "priority", "status", "description")
){

  # read jobs
  jobs <- job_read()

  # if there are no jobs, return null invisibly
  if(is.null(jobs)) {
    message("No known jobs")
    return(invisible(NULL))
  }

  # construct tibble with the simple fields
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("jobname", "owner", "priority", "status",
                          "description", "path")])})

  # add tags
  job_tbl$tags <- purrr::map_chr(jobs, function(x) {
    paste0(x$tags, collapse = " | ")
  })

  # add urls
  job_tbl$urls <- purrr::map(jobs, function(x) {
    x$urls
  })

  # drop rows as needed
  if(is.null(search)) {
    job_tbl <- job_tbl[job_tbl$status %in% c("active", "inactive"), ]
    job_tbl <- job_tbl[job_tbl$priority %in% 1:2, ]
  } else {
    target <- as.character(search)
    keep <- purrr::map_lgl(jobs, function(jb) {
      job_flat <- unname(unlist(unclass(jb)))
      if(any(job_flat == target, na.rm = TRUE)) {return(TRUE)}
      return(FALSE)
    })
    job_tbl <- job_tbl[keep, ]
  }

  #if(!is.null(status)) job_tbl <- job_tbl[job_tbl$status %in% status, ]
  #if(!is.null(priority)) job_tbl <- job_tbl[job_tbl$priority %in% priority, ]
  #if(!is.null(owner)) job_tbl <- job_tbl[job_tbl$owner %in% owner, ]

  if(nrow(job_tbl) == 0) return(NULL)

  # arrange
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, jobname)

  # drop cols as needed
  if(!is.null(select)) job_tbl <- job_tbl[, select]

  # check job paths to warn user
  job_checksentinels()

  return(as_wkbch_tbl(job_tbl))
}


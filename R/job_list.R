# defines the "view" family of functions

#' View a list of all jobs
#'
#' @param search input query used to extract a subset of jobs
#' @param select character vector of columns to return
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


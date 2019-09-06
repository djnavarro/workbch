# defines the "view" family of functions

#' View a list of all jobs
#'
#' @param priority what priority levels to show in the output
#' @param status what status values to show in the output
#' @param owner what owner values to show in the output
#' @param tags what tags to show in the output
#' @param cols what columns to show in the output
#' @export
job_list <- function(
  priority = 1:2,
  status = c("active", "inactive"),
  owner = NULL,
  tags = NULL,
  cols = c("jobname", "owner", "priority", "status", "description")
){

  # read jobs
  jobs <- job_read()

  # construct tibble with the simple fields
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("jobname", "owner", "priority", "status",
                          "description", "path")])})

  # add tags
  job_tbl$tags <- purrr::map_chr(jobs, function(x) {
    paste0(x$tags, collapse = ";")
  })

  # add urls
  job_tbl$urls <- purrr::map(jobs, function(x) {
    x$urls
  })

  # arrange
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, jobname)

  # drop rows as needed
  if(!is.null(status)) job_tbl <- job_tbl[job_tbl$status %in% status, ]
  if(!is.null(priority)) job_tbl <- job_tbl[job_tbl$priority %in% priority, ]
  if(!is.null(owner)) job_tbl <- job_tbl[job_tbl$owner %in% owner, ]

  if(nrow(job_tbl) == 0) return(NULL)

  # drop cols as needed
  if(!is.null(cols)) job_tbl <- job_tbl[, cols]

  # check job paths to warn user
  job_checksentinels()

  return(as_wkbch_tbl(job_tbl))
}


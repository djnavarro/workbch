
# helper functions that take a list of jobs as input
# and "pull" handy information as the output

# returns a character vector of all job names
pull_jobnames <- function(jobs) {
  if(is.null(jobs)) { return(character(0)) }
  return(purrr::map_chr(jobs, function(j) {j$jobname}))
}

# returns a character vector of all job paths
pull_jobpaths <- function(jobs) {
  if(is.null(jobs)) { return(character(0)) }
  return(purrr::map_chr(jobs, function(j) {
    normalizePath(j$path, mustWork = FALSE)
  }))
}

# returns a tibble with jobname, path and idstring, only
# for those jobs that contain a path. technically this
# isn't analogous to dplyr::pull but whatever
pull_jobinfo <- function(jobs) {
  job_tbl <- purrr::map_df(jobs, function(x){
    if(!is.null(x$path)) {
      return(tibble::as_tibble(
        x[c("jobname", "path", "idstring")]
      ))
    } else {
      return(tibble::tibble(
        jobname = character(0),
        path = character(0),
        idstring = character(0)
      ))
    }
  })
  job_tbl <- dplyr::arrange(job_tbl, jobname)
  job_tbl <- dplyr::filter(job_tbl, !is.na(path))

  return(as_wkbch_tbl(job_tbl))
}



#' Location of workbch files
#'
#' @param path Path to the folder
#'
#' @return Path to the folder
#' @export
workbch_home <- function(path = NULL) {
  if(!dir.exists(path)) {
    dir.create(path)
    message("new directory '", path, "' created")
  }
  if(!is.null(path)) {
    options(workbch.home = path)
  }
  job_home()
}



#' People known to workbch
#'
#' @export
workbch_people <- function() {
  as_wkbch_tbl(ppl_read())
}



#' Tags used by workbch
#'
#' @export
workbch_tags <- function() {

  jobs <- job_read()

  # vector containing each instance of a tag
  all_tags <- purrr::map(jobs, ~ .x$tags)
  all_tags <- unlist(all_tags)

  # tabulate and enframe
  freq_tags <- table(all_tags)
  tag_tbl <- tibble::tibble(
    tag = names(freq_tags),
    jobs = unname(freq_tags)
  )

  # arrange
  tag_tbl <- dplyr::arrange(tag_tbl, dplyr::desc(jobs), tag)

  return(as_wkbch_tbl(tag_tbl))
}





#' Paths known to workbch
#'
#' @param show_hidden should hidden jobs be included
#'
#' @return A tibble
#' @export
workbch_paths <- function(show_hidden = TRUE) {
  jobs <- job_read()
  job_tbl <- purrr::map_df(jobs, function(x){
    if(!is.null(x$path)) {
      return(tibble::as_tibble(x[c("jobname", "path")]))
    } else {
      return(tibble::tibble(jobname = character(0), path = character(0)))
    }
  })
  job_tbl <- dplyr::arrange(job_tbl, jobname)
  job_tbl <- dplyr::filter(job_tbl, !is.na(path))

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- hide_jobs(job_tbl)}

  # throw warnings
  verify_paths(job_tbl$jobname, job_tbl$path)
  return(as_wkbch_tbl(job_tbl))
}



#' Export to HTML file
#'
#' @export
workbch_export <- function() {
  rmarkdown::render(
    input = system.file("extdata", "status.Rmd", package = "workbch"),
    output_file = "workbch_status.html",
    output_dir = getOption("workbch.home"),
    params = list(path = getOption("workbch.home"))
  )
  utils::browseURL(url = file.path(getOption("workbch.home"), "workbch_status.html"))
}




#' Get location of workbch files
#'
#' @return Path to the folder
#' @export
workbch_gethome <- function() {
  getOption("workbch.home")
}

#' Set location of workbch files
#'
#' @param path Path to the folder
#'
#' @return Path to the folder
#' @export
workbch_sethome <- function(path) {
  if(is.null(path)) {
    return(workbch_gethome())
  }
  if(!dir.exists(path)) {
    dir.create(path)
    message("new directory '", path, "' created")
  }
  options(workbch.home = path)
  workbch_gethome()
}

#' Recover location of missing jobs
#'
#' @export
workbch_findjobs <- function() {

  missing <- job_missingsentinels()
  if(length(missing) == 0) return(NULL)

  dat <- workbch_paths()
  missing <- dat[dat$jobname %in% missing,]

  sentinels <- find_sentinels()
  state <- purrr::map_dfr(sentinels, function(s) {
    x <- readLines(s)
    np <- normalizePath(gsub("\\.workbch$", "", s))
    tibble::tibble(jobname = x[1], idstring = x[2], foundpath = np)
  })

  missing <- dplyr::left_join(missing, state, by = c("jobname", "idstring"))
  missing <- missing[,c("jobname", "idstring", "path", "foundpath")]

  if(interactive()) {
   # prompt user to update...
  }

  return(invisible(missing))
}


#' Returns the workbch search paths
#'
#' @return Paths searched
#' @export
workbch_getsearchpath <- function() {
  readLines(opt_file())
}

#' Sets the workbch search paths
#'
#' @param paths Paths to search
#'
#' @return Path to the folder
#' @export
workbch_setsearchpath <- function(paths) {
  writeLines(paths, opt_file())
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
      return(tibble::as_tibble(x[c("jobname", "path", "idstring")]))
    } else {
      return(tibble::tibble(jobname = character(0), path = character(0), idstring = character(0)))
    }
  })
  job_tbl <- dplyr::arrange(job_tbl, jobname)
  job_tbl <- dplyr::filter(job_tbl, !is.na(path))

  # remove the hidden jobs if need be
  if(!show_hidden) {job_tbl <- apply_mask(job_tbl)}

  # throw warnings
  #job_pathcheck(job_tbl$jobname, job_tbl$path)
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


#' File paths specified from job home
#'
#' @param jobname Name of job
#' @param ... Arguments to be passed to file.path
#' @export
there <- function(jobname, ...) {
  verify_jobname(jobname)
  all <- workbch_paths()
  home <- all$path[all$jobname == jobname]
  if(length(home) == 0) {
    stop("No path known for job '", jobname, "'", call. = FALSE)
  }
  home <- normalizePath(home)
  path <- normalizePath(file.path(home, ...))
  return(path)
}



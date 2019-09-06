
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
work_recover <- function() {

  # find the names of missing jobs
  missing <- job_missingsentinels()

  # if none are missing, invisibly return NULL
  if(length(missing) == 0) return(invisible(NULL))

  # add the paths and idstrings for those
  dat <- workbch_paths()
  missing <- dat[dat$jobname %in% missing,]

  # find all sentinel files
  sentinels <- find_sentinels()

  # extract informatio from all sentinel files
  state <- purrr::map_dfr(sentinels, function(s) {
    x <- readLines(s)
    np <- normalizePath(gsub("\\.workbch$", "", s))
    tibble::tibble(jobname = x[1], idstring = x[2], foundpath = np)
  })

  # try to match a sentinal to each missing job
  missing <- dplyr::left_join(missing, state, by = c("jobname", "idstring"))
  missing <- missing[,c("jobname", "idstring", "path", "foundpath")]

  # loop over missing jobs...
  if(interactive()) {
    for(i in 1:nrow(missing)) {

      # ask the user if they want to update the path information
      fpstring <- ifelse(
        test = is.na(missing$foundpath[i]),
        yes = "[none found]",
        no = missing$foundpath[i]
      )
      cat("\n")
      cat("Job '", missing$jobname[i], "':\n", sep = "")
      cat("   Current path... ", missing$path[i], "\n")
      cat("   New path....... ", fpstring, "\n")
      cat("\n")
      ans <- readline("   Do you want to update/remove the path? [y/n] ")

      # if yes, update it
      if(ans == "y") {
        workbch_setjob(jobname = missing$jobname[i], path = missing$foundpath[i])
        cat("   Job path updated\n")
      }

    }
  }

  # invisibly return the data frame
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
  return(as_wkbch_tbl(job_tbl))
}


#' Scans for jobs
#'
#' @param dirs directories to scan
#' @param seek search criterion
#' @param nesting allow nested jobs (default = FALSE)
#' @param owner default owner for a created job
#' @param priority default priority for a created job
#' @param status default status for a created job
#' @param tags default tags for a created job
#'
#' @details The \code{job_seek()} searches the folders listed in \code{dirs}
#' to find possible candidates that could be added as workbch jobs, or to
#' recover the location of any such jobs that may have moved. When candidate
#' folders are found, the user is presented with prompts asking if they would
#' like to add the folder as a workbch job (or pair it with an existing job
#' if a match is discovered).
#'
#' The search process uses a heuristic to discover possible jobs. A folder
#' is considered a possible candidate if it has a \code{.Rproj} file, a
#' \code{.workbch} file, or a git repository. If \code{nesting = FALSE} (the
#' default), a directory that is contained within another directory that meets
#' one of these criteria is not considered a candidate.
#'
#' As much as possible the \code{job_seek()} function attempts to guess values
#' for the various parameters. For instance, if there is a git repository that
#' has an upstream remote (on gitbub, bitbucket or gitlab), it attempts to guess
#' an appropriate URL for the remote. Similarly, if there is a \code{.workbch}
#' sentinel file, it uses the contents of the file to try to match the folder
#' against a known job and takes the jobname from the sentinel file. Otherwise
#' it guesses a default value using the folder name.
#' For parameter values that cannot be guessed from the folder itself, default
#' values are specified using the \code{owner}, \code{priority}, \code{status}
#' and \code{tags} arguments.
#'
#' Regardless of the values guessed, the user is prompted either to confirm
#' the guessed value (by hitting enter) or overriding the guess by supplying
#' the value manually.
#'
#' @export
job_seek <- function(dirs = getOption("workbch.search"),
                     seek = c("workbch", "git", "Rproj"),
                     nesting = FALSE,
                     owner = "",
                     priority = 1,
                     status = "active",
                     tags = character(0)
) {

  # enforce interactive
  require_interactive("job_seek")

  # search for possible job paths
  cat("\n  Scanning for possible jobs... ")
  paths <- get_jobpaths(dirs, seek, nesting)
  cat("done.\n")

  # if there are no known jobs...
  jobs <- job_read()
  if(is.null(jobs)) {
    jobs <- list() # initialise an empty list

  # if there are jobs, look for detached sentinel files...
  } else {

    jobids <- get_jobids(jobs)
    paths <- remove_duplicates(jobids, paths)
    sentinels <- get_detachedsentinels(jobids, paths)
    paths <- prompt_movedjobs(sentinels, jobids, jobs, paths)

  }

  # for new jobs, guide the user with prompts
  prompt_unmatchedjobs(jobs, paths, owner, priority, status, tags)
}




# helper functions --------------------------------------------------------


require_interactive <- function(name) {
  if(!interactive()) {
    stop(name, "can only be called interactively", call. = FALSE)
  }
}

prompt_unmatchedjobs <- function(jobs, paths, default_owner, default_priority,
                     default_status, default_tags) {

  # confirm with user before prompting individually...
  cat("\n  Scan found", length(paths), "unmatched job candidates\n")
  ans <- readline("  Do you want to continue? [y/n] ")

  # if yes, guide user with prompts...
  if(ans == "y") {
    for(p in paths) {
      Sys.sleep(1)
      cat("\f") # clear screen then prompt
      jobs <- prompt_from_scan(jobs, p, default_owner, default_priority,
                       default_status, default_tags)
    }
  }
  return(invisible(NULL))

}

get_jobpaths <- function(dirs, seek, nesting) {

  # construct the regex
  seek <- paste0(seek, collapse = "|")
  pattern <- paste0("\\.(", seek, ")$")

  # find all paths matching the regex
  paths <- unlist(purrr::map(dirs, function(d) {
    list.files(path = d, pattern = pattern, recursive = TRUE,
               all.files = TRUE, full.names = TRUE, include.dirs = TRUE)
  }))

  # trim to unique paths
  fsep <- .Platform$file.sep
  tailpattern <- paste0(fsep, "[^", fsep, "]*$")
  paths <- gsub(tailpattern, "", paths)
  paths <- unique(paths)

  # if nesting is forbidden, remove subpaths
  if(nesting == FALSE) {
    paths <- purrr::reduce(paths, function(pth, p) {
      if(!(p %in% pth)) return(pth)
      subdirs <- grep(p, pth)
      pth <- c(p, pth[-subdirs])
      return(pth)
    }, .init = paths)
  }

  return(paths)

}



get_jobids <- function(jobs) {
  purrr::map_dfr(
    .x = jobs,
    .f = ~ tibble::tibble(
      jobname = .x$jobname,
      idstring = .x$idstring,
      path = .x$path
    )
  )
}


remove_duplicates <- function(job_ids, paths) {

  # paths that are already known to workbch
  stored_paths <- job_ids$path
  stored_paths <- stored_paths[!is.na(stored_paths)]
  stored_paths <- normalizePath(stored_paths, mustWork = FALSE)

  # ignore all directories that are already linked to a job
  # todo: at this step we could check to see that they're still
  # there and appear to be workbch jobs?????
  duplicates <- intersect(stored_paths, paths)
  paths <- setdiff(paths, duplicates)
  return(paths)
}


get_detachedsentinels <- function(job_ids, paths) {

  detached_sentinels <- purrr::map_chr(
    .x = paths,
    .f = function(x) {
      if(file.exists(file.path(x, ".workbch"))) {
        return(x)
      }
      return(NA_character_)
    }
  )
  detached_sentinels <- detached_sentinels[!is.na(detached_sentinels)]
  return(detached_sentinels)
}


prompt_movedjobs <- function(detached_sentinels, job_ids, jobs, paths) {

  # if there are no detached sentinel files just return the paths
  if(length(detached_sentinels) == 0) {
    return(paths)
  }

  # if there are any paths with detached sentinel files, see if they
  # match any entries that workbch knows about
  found_sentinels <- purrr::map_dfr(
    .x = detached_sentinels,
    .f = function(x) {
      info <- readLines(file.path(x, ".workbch"))
      return(tibble::tibble(
        jobname = info[1],
        idstring = info[2],
        found_path = x))
    }
  )

  # try to match the found sentinels against any known jobs
  moved_sentinels <- dplyr::inner_join(
    x = job_ids,
    y = found_sentinels,
    by = c("jobname", "idstring")
  )

  # if there are any moved sentinels ask user if they want to fix
  # the link to any matches
  if(nrow(moved_sentinels) > 0) {
    for(i in 1:nrow(moved_sentinels)) {
      cat("\n")
      cat("  The job '", moved_sentinels$jobname[i],
          "' may have moved\n", sep="")
      cat("    Previous location: ", moved_sentinels$path[i], "\n")
      cat("    New location:      ", moved_sentinels$found_path[i], "\n")
      cat("\n")
      ans <- readline("  Set the job path to the new location? [y/n] ")
      if(ans == "y") {

        jobs <- update_jobpath(
          jobs = jobs,
          jobname = moved_sentinels$jobname[i],
          path = moved_sentinels$found_path[i]
        )
        job_write(jobs)
        cat("  Okay, path updated\n")

      } else {
        cat("  Okay, skipping\n")
      }
    }
  }

  paths <- setdiff(paths, moved_sentinels$found_path)
  return(paths)

}

# todo: make sure we use every piece of information to assist the
# user with default values...
prompt_from_scan <- function(jobs, def_path, def_owner, def_priority,
                             def_status, def_tags) {

  # guess the job name
  def_jobname <- guess_jobname(def_path)

  # by default set the descripton to match the job name
  def_description <- def_jobname

  # if there is a git repository, use it to guess
  # the location of the git remote
  def_site <- NA
  if(dir.exists(file.path(def_path, ".git"))) {
    found_remote <- git2r::remote_url(def_path)[1] # use first
    if(grepl("bitbucket|github|gitlab", found_remote)) { # if it's bb/gh/gl..
      def_site <- gsub(".*(bitbucket|github|gitlab).*", "\\1", found_remote)
      found_remote <- strsplit(found_remote, "[/:]")[[1]]
      user <- found_remote[length(found_remote)-1]
      repo <- found_remote[length(found_remote)]
      repo <- gsub("\\.git$", "/", repo)
      if(def_site == "bitbucket") prefix <- "https://bitbucket.org/"
      if(def_site == "github") prefix <- "https://github.com/"
      if(def_site == "gitlab") prefix <- "https://gitlab.com/"
      def_urlpath <- paste0(prefix, user, "/", repo)
    }
  }

  cat("\nSuggested values:\n\n")
  cat("  Path:             ", def_path, "\n")
  cat("  Job name:         ", def_jobname, "\n")
  cat("  Description:      ", def_description, "\n")
  cat("  Owner:            ", def_owner, "\n")
  cat("  Status:           ", def_status, "\n")
  cat("  Priority:         ", def_priority, "\n")
  cat("  Tags:             ", def_tags, "\n")
  if(!is.na(def_site)) {
    cat("  Git remote (site):", def_site, "\n")
    cat("  Git remote (url): ", def_urlpath, "\n")
  }
  cat("\n")

  # Ask user if we want to include this one
  ans <- readline("Track this folder with workbch? [y/n] ")
  if(ans == "y") {

    cat("\nType new values or press enter to use the suggested value:\n\n")

    # elicit responses from user
    jobname     <- readline("  Job name...... ")
    description <- readline("  Description... ")
    owner       <- readline("  Owner......... ")
    status      <- readline("  Status........ ")
    priority    <- readline("  Priority...... ")
    tags        <- readline("  Tags.......... ")

    # process
    priority  <- as.numeric(priority)

    # treat no response as suggested value
    if(jobname == "") jobname <- def_jobname
    if(description == "") description <- def_description
    if(owner == "") owner <- def_owner
    if(status == "") status <- def_status
    if(is.na(priority)) priority <- def_priority
    if(length(tags) == 0) tags <- def_tags

    # specify URL if there is one
    if(!is.na(def_site)) {
      def_url <- new_url(site = def_site, link = def_urlpath)
    } else {
      def_url <- NULL
    }

    # specify the idstring using .workbch if there is one
    sentinel <- file.path(def_path, ".workbch")
    if(file.exists(sentinel)) {
      info <- readLines(sentinel)
      def_id <- info[2]
    } else {
      def_id <- NULL
    }

    # make a new job
    jb <- new_job(
      jobname = jobname,
      description = description,
      owner = owner,
      status = status,
      priority = priority,
      path = def_path,
      tags = def_tags,
      urls = def_url,
      idstring = def_id
    )

    jobs[[jobname]] <- jb
    job_write(jobs)

    cat("  Okay, new job created\n")

  } else {
    cat("  Okay, skipping\n")
  }


  return(jobs)

}

# jobname is either recorded in a sentinel file (if one exists)
# or else constructed from the path and cleaned
guess_jobname <- function(path) {

  # if there is a sentinel file use it and do not modify
  sentinel <- file.path(path, ".workbch")
  if(file.exists(sentinel)) {
    info <- readLines(sentinel)
    jobname <- info[1]
    return(jobname)
  }

  # otherwise, return cleaned folder name
  jobname <- gsub(paste0("^.*", .Platform$file.sep), "", path)
  jobname <- janitor::make_clean_names(jobname)
  return(jobname)
}


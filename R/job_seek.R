
#' Scans for jobs
#'
#' @param dirs directories to scan
#' @param seek search criterion
#' @param nesting allow nested jobs (default = FALSE)
#' @param default_owner default owner for a created job
#' @param default_priority default priority for a created job
#' @param default_status default status for a created job
#' @param default_tags default tags for a created job
#'
#' @export
job_seek <- function(dirs = getOption("workbch.search"),
                     seek = c("workbch", "git", "Rproj"),
                     nesting = FALSE,
                     default_owner = "",
                     default_priority = 1,
                     default_status = "active",
                     default_tags = character(0)
) {

  # enforce interactive
  require_interactive("job_seek")

  # search for jobs
  cat("\n  Scanning for possible jobs... ")
  jobs <- job_read()
  job_ids <- get_jobids(jobs)
  paths <- get_jobpaths(dirs, seek, nesting)
  paths <- remove_duplicates(job_ids, paths)
  detached_sentinels <- get_detachedsentinels(job_ids, paths)
  cat("done.\n")

  # guide the user with prompts
  paths <- prompt_movedjobs(detached_sentinels, job_ids, jobs, paths)
  prompt_unmatchedjobs(paths, default_owner, default_priority,
                       default_status, default_tags)
}




# helper functions --------------------------------------------------------


require_interactive <- function(name) {
  if(!interactive()) {
    stop(name, "can only be called interactively", call. = FALSE)
  }
}

prompt_unmatchedjobs <- function(paths, default_owner, default_priority,
                     default_status, default_tags) {

  # confirm with user before prompting individually...
  cat("\n  Scan found", length(paths), "unmatched job candidates\n")
  ans <- readline("  Do you want to continue? [y/n] ")

  # if yes, guide user with prompts...
  if(ans == "y") {
    for(p in paths) {
      prompt_from_scan(p, default_owner, default_priority,
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
        cat("  Okay, skipping this job\n")
      }
    }
  }

  paths <- setdiff(paths, moved_sentinels$found_path)
  return(paths)

}

# todo: make sure we use every piece of information to assist the
# user with default values...
prompt_from_scan <- function(def_path, def_owner, def_priority,
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

  }


  return(NULL)

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



# work_scan <- function(dir, owner = NULL, status = NULL, priority = NULL) {
#
#   # verification step for other inputs
#   verify_status(status)
#   verify_priority(priority)
#
#   # find all git repositories
#   found_paths <- list.files(
#     path = dir, pattern = "\\.git$", full.names = TRUE,
#     recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
#   found_paths <- gsub("\\.git$", "", found_paths)
#   found_paths <- normalizePath(found_paths, winslash = .Platform$file.sep)
#
#   # guess the job name by splitting the path to find terminal folder
#   found_jobnames <- strsplit(found_paths, .Platform$file.sep, fixed = TRUE)
#   found_jobnames <- purrr::map_chr(found_jobnames, ~ .x[length(.x)])
#   found_jobnames <- janitor::make_clean_names(found_jobnames)
#
#   # load the existing jobs
#   jobs <- job_read()
#   job_names <- job_getnames(jobs)
#   job_paths <- job_getpaths(jobs)
#   job_paths <- suppressWarnings( # suppress b/c some jobs may have no path
#     normalizePath(job_paths, winslash = .Platform$file.sep)
#   )
#
#   # initialise output
#   created_jobs <- tibble::tibble(jobname = character(0), path = character(0))
#   n_known <- 0
#   n_skipped <- 0
#   n_created <- 0
#
#   # add jobs
#   for(i in seq_along(found_paths)) {
#
#     # if the path is already mapped to a job, skip it
#     if(found_paths[i] %in% job_paths) {
#       n_known <- n_known + 1
#       message("repo at '", found_paths[i], "' is listed... skipping")
#
#       # for a new path, make guesses and ask the user
#     } else {
#
#       # if there is a git repository, try to find the remote
#       found_remote <- git2r::remote_url(found_paths[i])[1] # use first
#       site <- NA
#       if(grepl("bitbucket|github|gitlab", found_remote)) { # if it's bb/gh/gl..
#         site <- gsub(".*(bitbucket|github|gitlab).*", "\\1", found_remote)
#         found_remote <- strsplit(found_remote, "[/:]")[[1]]
#         user <- found_remote[length(found_remote)-1]
#         repo <- found_remote[length(found_remote)]
#         repo <- gsub("\\.git$", "/", repo)
#         if(site == "bitbucket") prefix <- "https://bitbucket.org/"
#         if(site == "github") prefix <- "https://github.com/"
#         if(site == "gitlab") prefix <- "https://gitlab.com/"
#         url_path <- paste0(prefix, user, "/", repo)
#       }
#
#       # ensure the found job name doesn't already exist
#       # ... TODO: do this better!!!
#       while(found_jobnames[i] %in% job_names) {
#         found_jobnames[i] <- paste0(found_jobnames[i], "X")
#       }
#       # construct message to the for the user
#       message("")
#       message("unlisted repository found:")
#       message("    ")
#       message("    jobname:     ", found_jobnames[i])
#       message("    owner:       ", owner)
#       message("    path:        ", found_paths[i])
#       if(!is.na(site)){
#         message("    ", site, " url:  ", url_path)
#       }
#       message("    status:      ", status)
#       message("    priority:    ", priority)
#       message("    ")
#
#       # make the user decide
#       acc <- ""
#       while(acc != "y" & acc != "n") {
#         acc <- readline("    create job with these values? [y/n] ")
#         acc <- tolower(acc)
#         acc <- trimws(acc, "both")
#       }
#       message("")
#
#       # if the user says no, skip
#       if(acc == "n") {
#         n_skipped <- n_skipped + 1
#         message("    ... ok, skipping")
#       }
#
#       # if the user says yes, update
#       if(acc == "y") {
#         n_created <- n_created + 1
#
#         # create new job... without url
#         if(is.na(site)) {
#           jobs[[found_jobnames[i]]] <- new_job(
#             jobname = found_jobnames[i],
#             description = found_jobnames[i],
#             owner = owner,
#             status = status,
#             priority = priority,
#             path = found_paths[i]
#           )
#
#           # or else create new job... with a url
#         } else {
#           jobs[[found_jobnames[i]]] <- new_job(
#             jobname = found_jobnames[i],
#             description = found_jobnames[i],
#             owner = owner,
#             status = status,
#             priority = priority,
#             path = found_paths[i],
#             urls = new_url(site = site, link = url_path)
#           )
#         }
#
#         # write it now, in case user aborts later
#         job_write(jobs)
#
#         # update output
#         created_jobs <- dplyr::bind_rows(
#           created_jobs,
#           tibble::tibble(
#             jobname = found_jobnames[i],
#             path = found_paths[i]
#           )
#         )
#
#         # update list of known paths/jobnames
#         job_names <- job_getnames(jobs)
#         job_paths <- job_getpaths(jobs)
#         job_paths <- suppressWarnings(normalizePath(job_paths))
#
#         message("    ... done! new job created!")
#       }
#
#     }
#
#   }
#
#   # print a summary for the user
#   message("")
#   message(n_known, " repositories were already known")
#   message(n_skipped, " repositories were skipped by user")
#   message(n_created, " repositories were created")
#   message("")
#
#   return(created_jobs)
# }
#
#
#
# work_recover <- function(dirs = getOption("workbch.search")) {
#
#   # find the names of missing jobs
#   missing <- job_missingsentinels()
#
#   # if none are missing, invisibly return NULL
#   if(length(missing) == 0) return(invisible(NULL))
#
#   # add the paths and idstrings for those
#   dat <- job_allpaths()
#   missing <- dat[dat$jobname %in% missing,]
#
#   # find all sentinel files
#   sentinels <- find_sentinels(dirs)
#
#   # extract informatio from all sentinel files
#   state <- purrr::map_dfr(sentinels, function(s) {
#     x <- readLines(s)
#     np <- normalizePath(gsub("\\.workbch$", "", s))
#     tibble::tibble(jobname = x[1], idstring = x[2], foundpath = np)
#   })
#
#   # try to match a sentinal to each missing job
#   missing <- dplyr::left_join(missing, state, by = c("jobname", "idstring"))
#   missing <- missing[,c("jobname", "idstring", "path", "foundpath")]
#
#   # loop over missing jobs...
#   if(interactive()) {
#     for(i in 1:nrow(missing)) {
#
#       # ask the user if they want to update the path information
#       fpstring <- ifelse(
#         test = is.na(missing$foundpath[i]),
#         yes = "[none found]",
#         no = missing$foundpath[i]
#       )
#       cat("\n")
#       cat("Job '", missing$jobname[i], "':\n", sep = "")
#       cat("   Current path... ", missing$path[i], "\n")
#       cat("   New path....... ", fpstring, "\n")
#       cat("\n")
#       ans <- readline("   Do you want to update/remove the path? [y/n] ")
#
#       # if yes, update it
#       if(ans == "y") {
#         job_write(
#           update_job(
#             jobname = missing$jobname[i],
#             path = missing$foundpath[i]
#           )
#         )
#         cat("   Job path updated\n")
#       }
#
#     }
#   }
#
#   # invisibly return the data frame
#   return(invisible(missing))
# }


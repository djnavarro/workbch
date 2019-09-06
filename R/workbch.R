

#' Recover location of missing jobs
#'
#' @param dirs vector of paths to search recursively
#' @export
work_recover <- function(dirs = getOption("workbch.search")) {

  # find the names of missing jobs
  missing <- job_missingsentinels()

  # if none are missing, invisibly return NULL
  if(length(missing) == 0) return(invisible(NULL))

  # add the paths and idstrings for those
  dat <- job_allpaths()
  missing <- dat[dat$jobname %in% missing,]

  # find all sentinel files
  sentinels <- find_sentinels(dirs)

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
        job_write(update_job(jobname = missing$jobname[i], path = missing$foundpath[i]))
        cat("   Job path updated\n")
      }

    }
  }

  # invisibly return the data frame
  return(invisible(missing))
}

# currently not exported because I'm not sure what to do with this
work_tags <- function() {

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



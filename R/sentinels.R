
# locate sentinal files
find_sentinels <- function(dirs = getOption("workbch.search")) {
  unlist(purrr::map(dirs, function(d) {
    list.files(path = d, pattern = "\\.workbch$", recursive = TRUE,
               all.files = TRUE, full.names = TRUE)
  }))
}

# write a sentinal file
write_sentinel <- function(dir, jobname, idstring) {
  dir <- normalizePath(dir)
  file <- normalizePath(file.path(dir, ".workbch"), mustWork = FALSE)
  writeLines(text = c(jobname, idstring), con = file)
}


job_checksentinels <- function() {
  missing <- job_missingsentinels()
  if(length(missing) > 0) {
    warning(
      "Some job folders have moved or been deleted. Use job_seek() to fix",
      call. = FALSE
    )
  }
}

# returns the names of jobs for which the expected sentinel files are not found
# at the expected location
job_missingsentinels <- function() {
  jobs <- job_read()
  dat <- pull_jobinfo(jobs)
  info <- purrr::transpose(dat)
  missing <- purrr::map_chr(info, function(job) {
    f <- file.path(normalizePath(job$path, mustWork = FALSE), ".workbch")
    if(file.exists(f)) {
      s <- readLines(f)
      if(s[1] == job$jobname & s[2] == job$idstring) {
        return("")
      }
    }
    return(job$jobname)
  })
  missing <- missing[missing != ""]
  return(missing)
}


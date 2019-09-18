
# write one sentinel file
sentinel_write <- function(dir, jobname, idstring) {

  verify_onestring(dir)
  verify_onestring(jobname)
  verify_onestring(idstring)

  dir <- normalizePath(dir)
  file <- normalizePath(file.path(dir, ".workbch"), mustWork = FALSE)
  writeLines(text = c(jobname, idstring), con = file)
}


# jobnames for which sentinels have been lost
sentinel_missing <- function() {
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


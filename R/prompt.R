prompt_jobname <- function() {
  opts <- view_jobs()$jobname
  if(length(opts)>9) opts <- opts[1:9]
  for(i in 1:length(opts)) {
    cat(i, ":", opts[i], "\n")
  }
  cat("other: please type jobname\n")
  jnum <- readline("which job? ")
  if(jnum %in% as.character(1:length(opts))) {
    jobname <- opts[as.numeric(jnum)]
  } else {
    jobname <- jnum
  }
  return(jobname)
}

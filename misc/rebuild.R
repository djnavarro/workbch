
rebuild_job <- function(job) {

  # start from scratch including only the mandatory fields
  jb <- workbch:::new_job(
    jobname = job$jobname,
    description = job$description,
    owner = job$owner
  )

  if(!is.null(job$status)) {jb$status <- job$status}
  if(!is.null(job$team)) {jb$team <- job$team}
  if(!is.null(job$priority)) {jb$priority <- job$priority}
  if(!is.null(job$deadline)) {jb$deadline <- job$deadline}
  if(!is.null(job$path)) {jb$path <- job$path}
  if(!is.null(job$hidden)) {jb$hidden <- job$hidden}

  # fix notes
  if(!is.null(job$notes)) {
    if(is(job$notes, "data.frame")) {
      jb$notes <- job$notes
    }
  }

  # fix tasks
  if(!is.null(job$tasks)) {
    if(is(job$tasks, "data.frame")) {
      jb$tasks <- job$tasks
    }
  }

  # fix urls
  if(!is.null(job$urls)) {
    if(is(job$urls, "data.frame")) {
      jb$urls <- job$urls
    }
  }
  return(jb)

}

jobs <- workbch:::job_read()
for(i in 1:length(jobs)) {
  j <- jobs[[i]]
  print(j$jobname)
  jobs[[i]] <- rebuild_job(j)
}

#workbch:::job_write(jobs)


# reset home to a tempory directory and
# delete the job file if it exists
loc <- tempdir()
options(workbch.home = loc)
if(file.exists(job_file())) {
  file.remove(job_file())
}

test_that("task helpers work", {

  jobs <- job_read()
  print(task_read())
  print(empty_task())
  expect_equal(task_read(), empty_task())
  expect_equal(task_maxid(jobs), 0)

  # create a job directly from the constructor
  jobs <- empty_job()
  jb <- new_job(jobname = "toxic",
                description = "a song",
                owner = "britney")
  jobs[["toxic"]] <- jb
  job_write(jobs)


})

